---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

持續整合（CI）是一種在程式碼變更時自動測試、建置和驗證程式碼的方法。

**GitLab** 透過 `.gitlab-ci.yml` 檔案提供內建 CI/CD 功能。該檔案位於儲存庫根目錄，用來告訴 GitLab 如何建置和測試專案。它定義了每次推送變更時在乾淨環境中執行的階段和腳本。

本文件概述 Lumi 的 GitLab CI/CD 流水線如何運作，包括 `.gitlab-ci.yml` 檔案、Shell 腳本以及 Meson 和 Ninja 等外部工具的作用。

有關 Lumi CI 建置過程的詳細技術文件，請參閱儲存庫中的 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。

## GitLab CI/CD 基礎知識

CI 由名為 `.gitlab-ci.yml` 的檔案控制。該檔案定義：

- **階段**：有序的作業群組（例如 `build-this`、`build-that`、`package-up`）
- **作業**：每個階段內執行的個別任務
- **腳本**：為每個作業執行的 Shell 指令
- **執行器**：GitLab 用來執行流水線中定義作業的電腦

在 Lumi 中，流水線階段為：

- `dependencies`
- `build lumi`
- `appimage`

## 基於容器的建置

Lumi 流水線使用容器化來實現一致的建置：

1. **建立建置容器**：第一階段使用 Buildah 建立包含所有相依性的 Docker 映像
2. **使用容器**：後續階段在該容器內執行，確保環境一致
3. **可重複建置**：容器隔離保證不同執行器得到相同結果

這種方式確保建置在任何 GitLab 執行器上都以相同方式運作，並為複雜建置流程提供受控環境。

### 整合相依性來源

Lumi 的 CI 相依性映像從 **儲存庫內整合來源**（而非外部複製）建置分叉堆疊：

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

這些目錄會複製到容器建置內容中，並編譯到相依性前綴（通常為 `/opt/lumi-deps`）。這維持 CI 的可重複性，並確保 AppImage 建置與本機開發使用相同的真實來源。

## Shell 腳本的作用

`.gitlab-ci.yml` 中的作業通常直接呼叫 Shell 命令。複雜操作通常會移到儲存庫中的獨立腳本。

Lumi CI 使用模組化 Shell 腳本來組織建置邏輯：

**腳本呼叫範例：**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**這種方式的好處：**
- **簡潔的 YAML**：讓 `.gitlab-ci.yml` 專注於作業結構
- **可維護性**：複雜邏輯在 Shell 腳本中更容易除錯和修改
- **可重複使用**：腳本可在不同情境或環境中使用
- **模組化**：可將建置的不同面向拆分為獨立腳本

這能在保持 CI 設定簡潔的同時，支援複雜的建置流程。

## 與建置系統整合

Lumi 使用 **Meson** 和 **Ninja** 來準備並建置程式碼。

例如：

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

其中：

- `meson setup` 準備建置目錄並產生 `build.ninja`
- `ninja` 依定義執行建置命令

## Meson 建置系統結構

**Meson** 建置系統使用位於專案根目錄的根 `meson.build` 檔案。該檔案定義建置流程的頂層設定和進入點。

- 根 `meson.build` 通常與 `.gitlab-ci.yml` 位於同一目錄
- 從那裡 **遞迴地** 延伸到子目錄，每個子目錄可能有自己的 `meson.build` 檔案
- 這些子目錄檔案定義與該目錄相關的目標、來源、相依性和建置指令

## 環境變數

Lumi 流水線中的關鍵變數包括：

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**作業特定變數：**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

這些變數控制建置行為，並確保不同階段和執行器之間的一致性。

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

在此結構中：

- 根 `meson.build` 檔案設定整體建置環境
- 子目錄 `meson.build` 檔案處理特定元件或模組的編譯細節
- 這種分層配置使建置邏輯保持模組化且易於維護

## 階段之間的成品

成品是後續階段所需、由作業產生的檔案：

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## 流水線階段和相依性

Lumi 流水線由三個主要階段組成：

1. **Dependencies**：建立包含所有必要工具和函式庫的容器化建置環境
2. **Build Lumi**：在準備好的環境中使用 Meson 和 Ninja 編譯 Lumi
3. **AppImage**：將建置的應用程式打包為可分發的 AppImage 格式

**階段相依性：**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

每個階段僅在其相依性成功完成後執行，以確保正確的建置順序和成品可用性。

## 目前作業名稱

Lumi 的 `.gitlab-ci.yml` 目前定義了這些作業名稱：

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 總結

- `.gitlab-ci.yml` 定義流水線的結構和邏輯
- 作業包含 Shell 指令或外部腳本
- Meson 和 Ninja 等工具作為建置流程的一部分在作業中使用

Lumi 使用 GitLab CI 自動為基於 Debian 的平台建置 AppImage。流水線會建置相依性、編譯 Lumi，然後打包 AppImage。

有關來源層級詳細資訊，請參閱：

- Lumi 儲存庫根目錄中的 `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

有關環境設定、腳本架構和故障排除等 Lumi CI 建置過程的全面技術細節，請參閱 [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md)。
