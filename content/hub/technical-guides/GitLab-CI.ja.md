---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

継続的インテグレーション（CI）は、変更があるたびにコードを自動的にテスト、ビルド、検証する仕組みです。

**GitLab** は `.gitlab-ci.yml` ファイルを通じて CI/CD 機能を提供します。このファイルをリポジトリのルートに置くと、GitLab にプロジェクトのビルドとテスト方法を指示できます。プッシュのたびにクリーンな環境で実行されるステージとスクリプトを定義します。

このドキュメントでは、`.gitlab-ci.yml`、シェルスクリプト、Meson や Ninja などの外部ツールの役割を含め、Lumi の GitLab CI/CD パイプラインの仕組みを説明します。

Lumi CI ビルドプロセスの詳細な技術ドキュメントは、リポジトリ内の [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) を参照してください。

## GitLab CI/CD の基本

CI は `.gitlab-ci.yml` というファイルで制御されます。このファイルは次を定義します:

- **ステージ**: 順序付けられたジョブのグループ（例: `build-this`、`build-that`、`package-up`）
- **ジョブ**: 各ステージ内で実行する個別タスク
- **スクリプト**: 各ジョブで実行されるシェルコマンド
- **ランナー**: パイプラインで定義されたジョブを実行する GitLab が使うコンピュータ

Lumi のパイプラインステージは次のとおりです:

- `dependencies`
- `build lumi`
- `appimage`

## コンテナベースのビルド

Lumi パイプラインは、一貫したビルドのためにコンテナ化を使います:

1. **ビルドコンテナの作成**: 最初のステージで Buildah を使い、すべての依存関係を含む Docker イメージを作成します
2. **コンテナの利用**: 後続ステージはこのコンテナ内で実行され、環境の一貫性が保たれます
3. **再現可能なビルド**: コンテナの分離により、ランナーが異なっても同じ結果が得られます

この方式により、どの GitLab ランナーでも同じようにビルドでき、複雑なビルドプロセスを制御された環境で実行できます。

### 統合された依存関係ソース

Lumi の CI 依存関係イメージは、**リポジトリ内の統合ソース**（外部クローンではなく）からフォークしたスタックをビルドします:

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

これらのディレクトリはコンテナのビルドコンテキストにコピーされ、依存関係プレフィックス（通常は `/opt/lumi-deps`）にコンパイルされます。これにより CI の再現性が保たれ、AppImage ビルドがローカル開発と同じソースを参照できます。

## シェルスクリプトの役割

`.gitlab-ci.yml` のジョブは通常、シェルコマンドを直接呼び出します。複雑な処理は、リポジトリ内の別スクリプトに切り出すことがよくあります。

Lumi CI はモジュール式のシェルスクリプトでビルドロジックを整理します。

**スクリプト呼び出しの例:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**この方式の利点:**
- **クリーンな YAML**: `.gitlab-ci.yml` をジョブ構造に集中させられます
- **保守性**: 複雑なロジックはシェルスクリプトの方がデバッグ・変更しやすいです
- **再利用性**: スクリプトは別のコンテキストや環境でも使えます
- **モジュール性**: ビルドの各部分を独立したスクリプトに分けられます

これにより CI 設定をすっきり保ちながら、高度なビルドプロセスを実現できます。

## ビルドシステムとの統合

Lumi は **Meson** と **Ninja** でコードの準備とビルドを行います。

例:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

ここでは:

- `meson setup` がビルドディレクトリを準備し、`build.ninja` を生成します
- `ninja` が定義どおりにビルドコマンドを実行します

## Meson ビルドシステムの構造

**Meson** ビルドシステムは、プロジェクトのルートディレクトリに置くルート `meson.build` ファイルを使います。このファイルがトップレベルのビルド設定とビルドプロセスの入口を定義します。

- ルート `meson.build` は通常 `.gitlab-ci.yml` と同じディレクトリにあります
- そこから **再帰的に** サブディレクトリへと連鎖し、各サブディレクトリに独自の `meson.build` がある場合があります
- サブディレクトリのファイルは、そのディレクトリに関連するターゲット、ソース、依存関係、ビルド手順を定義します

## 環境変数

Lumi パイプラインの主な変数は次のとおりです:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**ジョブ固有の変数:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

これらの変数はビルドの挙動を制御し、ステージやランナー間の一貫性を保ちます。

## 構造の例

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

この構造では:

- ルート `meson.build` が全体のビルド環境を構成します
- サブディレクトリの `meson.build` が各コンポーネントやモジュールのコンパイル詳細を扱います
- この階層構造により、ビルドロジックをモジュール化して保守しやすく保てます

## ステージ間のアーティファクト

アーティファクトは、後続ステージで必要になるジョブの出力ファイルです:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## パイプラインのステージと依存関係

Lumi パイプラインは主に 3 つのステージで構成されます:

1. **Dependencies**: 必要なツールとライブラリを備えたコンテナ化ビルド環境を作成します
2. **Build Lumi**: 準備された環境で Meson と Ninja を使って Lumi をコンパイルします
3. **AppImage**: ビルドしたアプリケーションを配布可能な AppImage 形式にパッケージします

**ステージの依存関係:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

各ステージは依存関係が正常に完了した後にのみ実行され、ビルド順序とアーティファクトの可用性が保証されます。

## 現在のジョブ名

Lumi の `.gitlab-ci.yml` で現在定義されているジョブ名は次のとおりです:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## まとめ

- `.gitlab-ci.yml` がパイプラインの構造とロジックを定義します
- ジョブにはシェルコマンドまたは外部スクリプトが含まれます
- Meson や Ninja などのツールは、ビルドプロセスの一部としてジョブ内で使われます

Lumi は GitLab CI を使って Debian ベースのプラットフォーム向け AppImage を自動ビルドします。パイプラインは依存関係を構築し、Lumi をコンパイルしてから AppImage をパッケージします。

ソースレベルの詳細は次を参照してください:

- Lumi リポジトリルートの `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

環境セットアップ、スクリプト構成、トラブルシューティングなど、Lumi CI ビルドプロセスの包括的な技術詳細は [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) を参照してください。
