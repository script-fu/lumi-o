---
title: "バッチ処理"
type: docs
---
多くのファイルを一度に処理するための実用的なエンドツーエンドの例。

## それが住んでいる場所

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Lumi で登場する場所

- **ファイル → バッチ処理**

## それが示すもの

- ソース/宛先ディレクトリの `SF-DIRNAME` パラメータ
- フォールバックを使用した GUI パスの検証 (`validate-path-and-dir`)
- 再帰的なディレクトリのスキャンと反復
- 長時間実行される操作の進捗レポート