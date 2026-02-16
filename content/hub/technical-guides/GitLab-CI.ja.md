---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
継続的インテグレーション (CI) は、変更が行われるたびにコードを自動的にテスト、構築、検証する方法です。

**GitLab** は、`.gitlab-ci.yml` ファイルを通じて組み込みの CI/CD 機能を提供します。このファイルはリポジトリのルートに配置され、GitLab にプロジェクトのビルド方法とテスト方法を指示します。変更がプッシュされるたびにクリーンな環境で実行されるステージとスクリプトを定義します。

このドキュメントでは、`.gitlab-ci.yml` ファイル、シェル スクリプト、Meson や Ninja などの外部ツールの役割など、Lumi の GitLab CI/CD パイプラインがどのように機能するかについて概要を説明します。

Lumi CI ビルドプロセスの詳細な技術ドキュメントについては、リポジトリの [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) を参照してください。

## GitLab CI/CD の基本

CI は `.gitlab-ci.yml` という名前のファイルによって制御されます。このファイルは以下を定義します。

- **ステージ**: 順序付けられたジョブのグループ (例: `build-this`、`build-that`、`package-up`)
- **ジョブ**: 各ステージ内で実行する個別のタスク
- **スクリプト**: ジョブごとに実行されるシェル コマンド
- **ランナー**: パイプラインで定義されたジョブを実行するために GitLab が使用するコンピューター。

Lumi のパイプライン ステージは次のとおりです。

- `dependencies`
- `build lumi`
- `appimage`

## コンテナベースのビルド

Lumi パイプラインは、一貫したビルドのためにコンテナ化を使用します。

1. **ビルド コンテナの作成**: 最初の段階では、Buildah を使用して、すべての依存関係を含む Docker イメージを作成します。
2. **コンテナの使用**: 後続のステージはこのコンテナ内で実行され、一貫した環境が確保されます。
3. **再現可能なビルド**: コンテナーの分離により、異なるランナー間で同じ結果が保証されます。

このアプローチにより、どの GitLab ランナーでもビルドが同じように動作することが保証され、複雑なビルド プロセスに制御された環境が提供されます。

## シェルスクリプトの役割

`.gitlab-ci.yml` のジョブは通常、シェル コマンドを直接呼び出します。複雑な操作は、リポジトリに保存される個別のスクリプトに移動されることがよくあります。

Lumi CI は、モジュール式のシェル スクリプトを使用してビルド ロジックを編成します。

**スクリプト呼び出しの例:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**このアプローチの利点:**
- **クリーンな YAML**: `.gitlab-ci.yml` ファイルをジョブ構造に重点を置いたままにします
- **保守性**: 複雑なロジックはシェル スクリプトでデバッグおよび変更が容易です
- **再利用性**: スクリプトはさまざまなコンテキストや環境で使用できます。
- **モジュール性**: ビルドのさまざまな側面を、焦点を絞ったスクリプトに分離できます。

これにより、洗練されたビルド プロセスが可能になりながら、CI 構成がクリーンな状態に保たれます。

## ビルド システムとの統合

Lumi は **Meson** と **Ninja** を使用してコードを準備し、ビルドします。

たとえば:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

ここで:

- `meson setup` はビルド ディレクトリを準備し、`build.ninja` を生成します
- `ninja` は定義どおりにビルド コマンドを実行します

## Meson Build システムの構造

**Meson** ビルド システムは、プロジェクトのルート ディレクトリに配置されたルート `meson.build` ファイルを使用します。このファイルは、トップレベルのビルド構成とビルド プロセスのエントリ ポイントを定義します。

- ルート `meson.build` は通常、`.gitlab-ci.yml` と同じディレクトリにあります。
- そこから、**再帰的に**サブディレクトリにカスケードされます。各サブディレクトリには独自の `meson.build` ファイルがある場合があります
- これらのサブディレクトリ ファイルは、そのディレクトリに関連するターゲット、ソース、依存関係、およびビルド手順を定義します。

## 環境変数

Lumi パイプラインの主な変数には次のものがあります。

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
```これらの変数はビルド動作を制御し、さまざまなステージやランナー間での一貫性を確保します。

## 構造例

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

この構造では次のようになります。

- ルート `meson.build` ファイルは、ビルド環境全体を構成します
- サブディレクトリ `meson.build` ファイルは、特定のコンポーネントまたはモジュールのコンパイルの詳細を処理します
- この階層レイアウトにより、ビルド ロジックがモジュール化され、保守可能になります。

## ステージ間のアーティファクト

アーティファクトは、後続のステージで必要となるジョブによって生成されるファイルです。

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## パイプラインのステージと依存関係

Lumi パイプラインは、次の 3 つの主要なステージで構成されます。

1. **依存関係**: 必要なすべてのツールとライブラリを備えたコンテナ化されたビルド環境を作成します。
2. **Lumi のビルド**: 準備された環境で Meson と Ninja を使用して Lumi をコンパイルします
3. **AppImage**: 構築されたアプリケーションを配布可能な AppImage 形式にパッケージ化します。

**ステージの依存関係:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

各ステージは、依存関係が正常に完了した後にのみ実行され、適切なビルド順序とアーティファクトの可用性が保証されます。

## 現在のジョブ名

Lumi `.gitlab-ci.yml` は現在、次のジョブ名を定義しています。

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## 概要

- `.gitlab-ci.yml` はパイプラインの構造とロジックを定義します
- ジョブにはシェル コマンドまたは外部スクリプトが含まれます
- Meson や Ninja などのツールは、ビルド プロセスの一部としてジョブ内で使用されます

Lumi は GitLab CI を使用して、Debian ベースのプラットフォーム用の AppImage を自動的に構築します。パイプラインは依存関係を構築し、Lumi をコンパイルして、AppImage をパッケージ化します。

ソースレベルの詳細については、次を使用します。

- Lumi リポジトリ ルートの `.gitlab-ci.yml`
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

環境セットアップ、スクリプトアーキテクチャ、トラブルシューティングなど、Lumi CI ビルドプロセスに関する包括的な技術詳細については、[README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) を参照してください。