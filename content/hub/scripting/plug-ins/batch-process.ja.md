---
title: "バッチ処理"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
多くのファイルを一度に処理するための実用的なエンドツーエンドの例。

## ソースコード

- [ソースを表示](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Lumi でのメニュー

- **ファイル → バッチ処理**

## 学べる内容

- ソース/宛先ディレクトリの `SF-DIRNAME` パラメータ
- フォールバックを使用した GUI パスの検証 (`validate-path-and-dir`)
- 再帰的なディレクトリのスキャンと反復
- 長時間実行される操作の進捗レポート
