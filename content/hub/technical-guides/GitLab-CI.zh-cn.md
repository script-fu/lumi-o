---
title: "亚搏体育appGitLab持续集成"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
持续集成 (CI) 是一种在代码发生更改时自动测试、构建和验证代码的方法。

**GitLab** 通过其 `.gitlab-ci.yml` 文件提供内置 CI/CD 功能。该文件位于存储库的根目录中，告诉 GitLab 如何构建和测试您的项目。它定义了每次推送更改时在干净的环境中运行的阶段和脚本。

本文档概述了 Lumi 的 GitLab CI/CD 管道的工作原理，包括 `.gitlab-ci.yml` 文件、shell 脚本以及 Meson 和 Ninja 等外部工具的作用。

有关 Lumi CI 构建过程的详细技术文档，请参阅存储库中的[README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。

## GitLab CI/CD 基础知识

CI 由名为 `.gitlab-ci.yml` 的文件控制。该文件定义：

- **阶段**：有序的作业组（例如，`build-this`、`build-that`、`package-up`）
- **作业**：每个阶段内运行的单独任务
- **脚本**：为每个作业执行的 Shell 命令
- **运行器**：GitLab 用于运行管道中定义的作业的计算机。

在 Lumi 中，管道阶段是：

- `dependencies`
- `build lumi`
- `appimage`

## 基于容器的构建

Lumi 管道使用容器化来实现一致的构建：

1. **创建构建容器**：第一阶段使用Buildah创建具有所有依赖项的Docker镜像
2. **使用容器**：后续阶段在该容器内运行，确保环境一致
3. **可重复的构建**：容器隔离保证不同运行者获得相同的结果

这种方法确保构建在任何 GitLab 运行器上都以相同的方式工作，并为复杂的构建过程提供受控环境。

## Shell 脚本的作用

`.gitlab-ci.yml` 中的作业通常直接调用 shell 命令。复杂的操作通常被移至存储在存储库中的单独脚本中。

Lumi CI 使用模块化 shell 脚本来组织构建逻辑：

**脚本调用示例：**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**这种方法的好处：**
- **干净的 YAML**：使 `.gitlab-ci.yml` 文件专注于作业结构
- **可维护性**：复杂的逻辑在shell脚本中更容易调试和修改
- **可重用性**：脚本可以在不同的上下文或环境中使用
- **模块化**：构建的不同方面可以分为有针对性的脚本

这可以保持 CI 配置干净，同时允许复杂的构建过程。

## 与构建系统集成

Lumi 使用 **Meson** 和 **Ninja** 来准备并构建代码。

例如：

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

这里：

- `meson setup` 准备构建目录并生成`build.ninja`
- `ninja` 按照定义运行构建命令

## 介子构建系统结构

**Meson** 构建系统使用位于项目根目录的根 `meson.build` 文件。该文件定义了构建过程的顶级构建配置和入口点。

- 根`meson.build` 通常位于与`.gitlab-ci.yml` 相同的目录中
- 从那里，它**递归地**到子目录中，每个子目录可能有自己的`meson.build` 文件
- 这些子目录文件定义与该目录相关的目标、源、依赖项和构建指令

## 环境变量

Lumi 管道中的关键变量包括：

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**特定于工作的变量：**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```这些变量控制构建行为并确保不同阶段和运行者之间的一致性。

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

在这个结构中：

-根`meson.build`文件配置整体构建环境
- 子目录`meson.build`文件处理特定组件或模块的编译详细信息
- 这种分层布局保持构建逻辑的模块化和可维护性

## 阶段之间的工件

工件是后续阶段所需的作业生成的文件：

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 管道阶段和依赖关系

Lumi 管道由三个主要阶段组成：

1. **依赖项**：使用所有必需的工具和库创建容器化构建环境
2. **Build Lumi**：在准备好的环境中使用Meson和Ninja编译Lumi
3. **AppImage**：将构建的应用程序打包成可分发的AppImage格式

**阶段依赖性：**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

每个阶段仅在其依赖项成功完成后运行，以确保正确的构建顺序和工件可用性。

## 当前职位名称

Lumi `.gitlab-ci.yml` 当前定义了这些作业名称：

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 总结

- `.gitlab-ci.yml`定义了管道的结构和逻辑
- 作业包含 shell 命令或外部脚本
- Meson 和 Ninja 等工具作为构建过程的一部分在作业中使用

Lumi 使用 GitLab CI 自动为基于 Debian 的平台构建 AppImage。该管道构建依赖项，编译 Lumi，然后打包 AppImage。

有关源级别的详细信息，请使用：

- `.gitlab-ci.yml` 在 Lumi 存储库根目录中
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

有关 Lumi CI 构建过程的全面技术详细信息，包括环境设置、脚本架构和故障排除，请参阅[README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。