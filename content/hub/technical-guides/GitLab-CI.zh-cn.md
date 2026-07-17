---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

持续集成（CI）是一种在代码发生更改时自动测试、构建和验证代码的方法。

**GitLab** 通过 `.gitlab-ci.yml` 文件提供内置 CI/CD 功能。该文件位于仓库根目录，用于告诉 GitLab 如何构建和测试项目。它定义了每次推送更改时在干净环境中运行的阶段和脚本。

本文档概述 Lumi 的 GitLab CI/CD 流水线如何工作，包括 `.gitlab-ci.yml` 文件、Shell 脚本以及 Meson 和 Ninja 等外部工具的作用。

有关 Lumi CI 构建过程的详细技术文档，请参阅仓库中的 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。

## GitLab CI/CD 基础知识

CI 由名为 `.gitlab-ci.yml` 的文件控制。该文件定义：

- **阶段**：有序的作业组（例如 `build-this`、`build-that`、`package-up`）
- **作业**：每个阶段内运行的单独任务
- **脚本**：为每个作业执行的 Shell 命令
- **运行器**：GitLab 用来运行流水线中定义作业的计算机

在 Lumi 中，流水线阶段为：

- `dependencies`
- `build lumi`
- `appimage`

## 基于容器的构建

Lumi 流水线使用容器化来实现一致的构建：

1. **创建构建容器**：第一阶段使用 Buildah 创建包含所有依赖项的 Docker 镜像
2. **使用容器**：后续阶段在该容器内运行，确保环境一致
3. **可重复构建**：容器隔离保证不同运行器得到相同结果

这种方式确保构建在任何 GitLab 运行器上都以相同方式工作，并为复杂构建过程提供受控环境。

### 集成依赖源

Lumi 的 CI 依赖镜像从 **仓库内集成源**（而非外部克隆）构建分叉栈：

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

这些目录被复制到容器构建上下文中，并编译到依赖前缀（通常为 `/opt/lumi-deps`）。这保持了 CI 的可重复性，并确保 AppImage 构建与本地开发使用相同的真实来源。

## Shell 脚本的作用

`.gitlab-ci.yml` 中的作业通常直接调用 Shell 命令。复杂操作通常会移到仓库中的独立脚本。

Lumi CI 使用模块化 Shell 脚本来组织构建逻辑：

**脚本调用示例：**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**这种方式的好处：**
- **简洁的 YAML**：让 `.gitlab-ci.yml` 专注于作业结构
- **可维护性**：复杂逻辑在 Shell 脚本中更容易调试和修改
- **可重用性**：脚本可在不同上下文或环境中使用
- **模块化**：可将构建的不同方面拆分为独立脚本

这能在保持 CI 配置简洁的同时，支持复杂的构建流程。

## 与构建系统集成

Lumi 使用 **Meson** 和 **Ninja** 来准备并构建代码。

例如：

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

其中：

- `meson setup` 准备构建目录并生成 `build.ninja`
- `ninja` 按定义执行构建命令

## Meson 构建系统结构

**Meson** 构建系统使用位于项目根目录的根 `meson.build` 文件。该文件定义构建过程的顶层配置和入口点。

- 根 `meson.build` 通常与 `.gitlab-ci.yml` 位于同一目录
- 从那里 **递归地** 延伸到子目录，每个子目录可能有自己的 `meson.build` 文件
- 这些子目录文件定义与该目录相关的目标、源文件、依赖项和构建指令

## 环境变量

Lumi 流水线中的关键变量包括：

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**作业特定变量：**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

这些变量控制构建行为，并确保不同阶段和运行器之间的一致性。

## 结构示例

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

在此结构中：

- 根 `meson.build` 文件配置整体构建环境
- 子目录 `meson.build` 文件处理特定组件或模块的编译细节
- 这种分层布局使构建逻辑保持模块化和可维护

## 阶段之间的工件

工件是后续阶段所需的、由作业生成的文件：

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 流水线阶段和依赖关系

Lumi 流水线由三个主要阶段组成：

1. **Dependencies**：创建包含所有必需工具和库的容器化构建环境
2. **Build Lumi**：在准备好的环境中使用 Meson 和 Ninja 编译 Lumi
3. **AppImage**：将构建的应用程序打包为可分发的 AppImage 格式

**阶段依赖关系：**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

每个阶段仅在其依赖项成功完成后运行，以确保正确的构建顺序和工件可用性。

## 当前作业名称

Lumi 的 `.gitlab-ci.yml` 当前定义了这些作业名称：

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 总结

- `.gitlab-ci.yml` 定义流水线的结构和逻辑
- 作业包含 Shell 命令或外部脚本
- Meson 和 Ninja 等工具作为构建过程的一部分在作业中使用

Lumi 使用 GitLab CI 自动为基于 Debian 的平台构建 AppImage。流水线会构建依赖项、编译 Lumi，然后打包 AppImage。

有关源级别详细信息，请参阅：

- Lumi 仓库根目录中的 `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

有关环境设置、脚本架构和故障排除等 Lumi CI 构建过程的全面技术细节，请参阅 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。
