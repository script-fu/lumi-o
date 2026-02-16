---
title: "亞搏體育appGitLab持續集成"
type: docs
url: "hub/technical-guides/folder/GitLab-CI"
---
持續整合 (CI) 是一種在程式碼發生變更時自動測試、建置和驗證程式碼的方法。

**GitLab** 透過其 `.gitlab-ci.yml` 檔案提供內建 CI/CD 功能。該文件位於儲存庫的根目錄中，告訴 GitLab 如何建置和測試您的專案。它定義了每次推送更改時在乾淨的環境中運行的階段和腳本。

本文檔概述了 Lumi 的 GitLab CI/CD 管道的工作原理，包括 `.gitlab-ci.yml` 文件、shell 腳本以及 Meson 和 Ninja 等外部工具的作用。

有關 Lumi CI 建置過程的詳細技術文檔，請參閱儲存庫中的[README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。

## GitLab CI/CD 基礎知識

CI 由名為 `.gitlab-ci.yml` 的檔案控制。該文件定義：

- **階段**：有序的作業組（例如，`build-this`、`build-that`、`package-up`）
- **作業**：每個階段內執行的單獨任務
- **腳本**：為每個作業執行的 Shell 指令
- **運行器**：GitLab 用於運行管道中定義的作業的電腦。

在 Lumi 中，管道階段是：

- `dependencies`
- `build lumi`
- `appimage`

## 基於容器的構建

Lumi 管道使用容器化來實現一致的建置：

1. **建立建置容器**：第一階段使用Buildah建立具有所有相依性的Docker映像
2. **使用容器**：後續階段在該容器內運行，確保環境一致
3. **可重複的建置**：容器隔離保證不同運行者獲得相同的結果

這種方法確保建置在任何 GitLab 運行器上都以相同的方式運作，並為複雜的建置流程提供受控環境。

## Shell 腳本的作用

`.gitlab-ci.yml` 中的作業通常直接呼叫 shell 命令。複雜的操作通常會移至儲存在儲存庫中的單獨腳本中。

Lumi CI 使用模組化 shell 腳本來組織建構邏輯：

**腳本呼叫範例：**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**這種方法的好處：**
- **乾淨的 YAML**：使 `.gitlab-ci.yml` 文件專注於作業結構
- **可維護性**：複雜的邏輯在shell腳本中更容易調試和修改
- **可重複使用性**：腳本可以在不同的情境或環境中使用
- **模組化**：建構的不同面向可以分為有針對性的腳本

這可以保持 CI 配置乾淨，同時允許複雜的建置過程。

## 與建置系統集成

Lumi 使用 **Meson** 和 **Ninja** 來準備並建立程式碼。

例如：

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

這裡：

- `meson setup` 準備建置目錄並產生`build.ninja`
- `ninja` 依照定義執行建置命令

## 介子建構系統結構

**Meson** 建置系統使用位於專案根目錄的根 `meson.build` 檔案。該文件定義了建置過程的頂級建置配置和入口點。

- 根`meson.build` 通常位於與`.gitlab-ci.yml` 相同的目錄中
- 從那裡，它**遞歸地**到子目錄中，每個子目錄可能都有自己的`meson.build` 文件
- 這些子目錄檔案定義與該目錄相關的目標、來源、依賴項和建置指令

## 環境變數

Lumi 管道中的關鍵變數包括：

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**特定於工作的變數：**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```這些變數控制建置行為並確保不同階段和運行者之間的一致性。

## 結構範例

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

在這個結構中：

-根`meson.build`檔案配置整體建置環境
- 子目錄`meson.build`檔案處理特定組件或模組的編譯詳細信息
- 這種分層佈局保持建構邏輯的模組化和可維護性

## 階段之間的工件

工件是後續階段所需的作業產生的文件：

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 管道階段和依賴關係

Lumi 管道由三個主要階段組成：

1. **依賴項**：使用所有必要的工具和庫來建立容器化建置環境
2. **Build Lumi**：在準備好的環境中使用Meson和Ninja編譯Lumi
3. **AppImage**：將建置的應用程式打包成可分發的AppImage格式

**階段依賴性：**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

每個階段僅在其相依性成功完成後運行，以確保正確的建置順序和工件可用性。

## 目前職位名稱

Lumi `.gitlab-ci.yml` 目前定義了這些作業名稱：

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 總結

- `.gitlab-ci.yml`定義了管道的結構和邏輯
- 作業包含 shell 指令或外部腳本
- Meson 和 Ninja 等工具作為建置過程的一部分在作業中使用

Lumi 使用 GitLab CI 自動為基於 Debian 的平台建立 AppImage。該管道建置依賴項，編譯 Lumi，然後打包 AppImage。

有關來源層級的詳細信息，請使用：

- `.gitlab-ci.yml` 在 Lumi 儲存庫根目錄中
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

有關 Lumi CI 建置過程的全面技術詳細信息，包括環境設定、腳本架構和故障排除，請參閱[README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。