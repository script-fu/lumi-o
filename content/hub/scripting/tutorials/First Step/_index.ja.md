---
title: "最初のステップ"
type: docs
weight: 1
---
First Steps では、Scheme でうまく機能する機能パターンを、短い例と段階的なリファクタリングとともに紹介します。
最終的には、独自のカスタム メッセージング ライブラリを構築し、より高度なプラグインの開発を続けるための強固な基盤が得られるでしょう。

### 必要なもの

始める前に、以下のものがあることを確認してください。

- Lumi がインストールされています (最近のビルドまたは AppImage)。
- `.scm` ファイル用のテキスト エディター (VS Code は適切に機能します): [Visual Studio Code](/hub/scripting/tools/visual-studio-code)
- [Git](/hub/scripting/tools/git/) を使用してコードをバックアップし、バージョンを管理します。

## チュートリアルの手順

{{< cards >}}
  {{< card link="hello-world" title="「こんにちは世界」" icon="sparkles" subtitle="基本構造を備えた最初の Scheme プラグインを作成する" >}}
  {{< card link="refactoring" title="リファクタリング" icon="code" subtitle="動作を変更せずにコード構造を改善する" >}}
  {{< card link="loading" title="読み込み中" icon="upload" subtitle="関数を再利用可能なライブラリに整理する" >}}
  {{< card link="validation" title="検証" icon="badge-check" subtitle="エラー処理と入力検証を追加する" >}}
  {{< card link="return-values" title="戻り値" icon="arrow-circle-left" subtitle="関数の出力とデータフローを理解する" >}}
  {{< card link="messaging-library" title="メッセージング ライブラリ" icon="annotation" subtitle="包括的なメッセージング システムを構築する" >}}
{{< /cards >}}

{{< cards >}}
  {{< card link="reworking" title="手直し" icon="adjustments" subtitle="高度なコード再構築テクニック" >}}
  {{< card link="refactor-again" title="再度リファクタリングする" icon="arrow-sm-right" subtitle="反復的な改善と最適化" >}}
  {{< card link="the-procedure-browser" title="プロシージャブラウザ" icon="book-open" subtitle="Lumi のプロシージャ データベースを探索する" >}}
  {{< card link="final-thoughts" title="最終的な考え" icon="academic-cap" subtitle="開発の概要と次のステップ" >}}
{{< /cards >}}