---
title: "フィルタープラグイン"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
[最初のステップ](../../first-step/) チュートリアルには _procedure_ プラグインを使用しました。これらのタイプのプラグインは、入力として画像やドローアブルを必要とせずに動作します。通常、画像とそのドローアブルを変更するにはプラグインを使用します。このようなプラグインは、_filter_ プラグインと呼ばれます。

### Drawable とは何ですか?

Lumi の **描画可能** は、レイヤーやチャンネルなど、描画できる画像要素を指します。フィルター プラグインは通常、これらの要素に対して動作します。

### 単純なフィルター プラグインの例

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; let 文を使ってメッセージ変数とコアコードを定義する
  (let ((message "hello, world"))
    ;; メッセージを Lumi の エラーコンソール に表示する
    (lumi-message message)
    ;; 最初に選択した drawable の色を反転する
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; プラグインを登録
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; メインプロシージャ名
  "Simple Filter Plug-in Demo"             ;; Lumi メニューに表示される名前
  "Tests a basic Scheme filter plug-in"    ;; ツールチップの説明
  "Author Name"                            ;; 自分自身を褒めてあげよう
  "License"                                ;; ライセンス
  "Date written"                           ;; 作成日
  "*"                                      ;; このプラグインに画像が必要であることを示す
  SF-ONE-OR-MORE-DRAWABLE)                 ;; 1つ以上の選択された drawable が必要

;; プラグインのメニュー位置を指定する
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

テキストをコピーし、Lumi のプラグイン フォルダの 1 つにある `simple-filter-plug-in` という名前のフォルダに `simple-filter-plug-in.scm` として保存します。 Lumi プラグイン フォルダーは、以下にリストされている_任意の_ フォルダーです。
 **Lumi > 編集 > 環境設定 > フォルダ > プラグイン**

Linux では、`simple-filter-plug-in.scm` ファイルを右クリックし、**プロパティ > アクセス許可** に移動し、**ファイルのプログラムとしての実行を許可する** にチェックを入れます。ファイルが正しい場所に配置され、実行可能で構文エラーがなければ、Lumi を再起動すると、上部のメニューヘッダーバーの **プラグイン** というメニュー内にファイルが表示されます。

### プラグインの実行

1. 画像を開きます (このフィルター プラグインを機能させるには画像が必要です)。
2. **[ツール] > [デバッグ] > [メッセージ コンソール]** を開いてメッセージを確認します。
3. **プラグイン** メニューから **シンプル フィルター プラグイン デモ** を選択します。
4. 選択したレイヤーの 1 つが色反転され、エラー コンソールにメッセージが出力されます。

### プラグインの編集

プラグインをカスタマイズするには、`.scm` ファイルを編集します。たとえば、表示されるメッセージを変更するには、次のようにします。

1. ファイルを開き、`message` を定義する行を見つけます。
2. `"hello, world"` をカスタム テキストに置き換えます。
3. ファイルを保存します。

Lumi バージョン 3 では、保存された変更を有効にするためにプラグインを更新する必要はありません。プラグインを再実行するだけで、更新されたメッセージが表示されます。

### プラグインの検査

#### シバンライン

最初の行は、スクリプトが Lumi 3 のプラグインとして動作することを確認します。

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### プロシージャ定義

このプロシージャは、アクティブなイメージと選択されたドローアブルの 2 つの引数を受け入れます。

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### コアロジック

`let` ステートメントは変数を定義し、ドローアブルに対して操作を実行します。

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Lumi の エラーコンソール にメッセージを表示する
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; 最初に選択した drawable の色を反転する
```

### プラグインの登録

プラグインはフィルター プラグインとして Lumi に登録されます。

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; メインプロシージャを登録
  "Simple Filter Plug-in Demo"             ;; Lumi メニューに表示される名前
  "Tests a basic Scheme filter plug-in"    ;; ツールチップの説明
  "Author Name"                            ;; 作者名
  "License"                                ;; ライセンスタイプ
  "Date written"                           ;; 作成日
  "*"                                      ;; プラグインに画像が必要であることを示す
  SF-ONE-OR-MORE-DRAWABLE)                 ;; 1つ以上の選択された drawable が必要
```

#### メニュー登録

この行は、プラグインのメニューの場所を指定します。

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### トラブルシューティング

プラグインが表示されない場合は、その場所、名前、および実行可能プロパティを確認してください。

場所はプラグイン検索パス内にある必要があります。
ファイル名は、ファイルが含まれるフォルダーの名前と一致する必要があります。
ファイルは実行可能ファイルとして設定する必要があります。


**メッセージ コンソール**は、カスタム プラグインのトラブルシューティングに役立つツールです。プラグインが期待どおりに動作しない場合は、ここでエラー メッセージまたはログを確認してください。 **ターミナル** ウィンドウでは、デバッグ情報や読み込みの問題のレポートも提供できます。