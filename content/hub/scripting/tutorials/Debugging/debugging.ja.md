---
title: "デバッグ"
type: docs
weight: 5
---
スクリプトでは、確実な関数は存在しません。最も信頼性の高いコマンドでも、予期しない入力や条件に直面すると失敗する可能性があります。これを防ぐために、カスタム デバッグ システムを実装し、防御的なプログラミング手法を採用できます。標準関数をエラー処理メカニズムでラップし、有益なフィードバックを提供することで、スクリプトをより堅牢にし、トラブルシューティングを容易にすることができます。

この戦略の重要な部分は、グローバル デバッグ フラグを使用して冗長出力を制御することです。これにより、通常の実行中に出力をクリーンな状態に保ちながら、必要に応じて詳細なデバッグ情報を有効にすることができます。

## グローバル デバッグ フラグ

グローバル デバッグ フラグは、スクリプトの実行中に出力される情報のレベルを制御する簡単かつ効果的な方法です。有効にすると、問題を追跡するのに非常に役立つ詳細なデバッグ メッセージが提供されます。無効にすると、本番環境で使用できるように出力が簡潔に保たれます。

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

デフォルトでは、デバッグはオフになっています。開発中に冗長出力を有効にするには、フラグを `#t` に設定するだけです。

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

ヘルパー関数を使用して、コードの特定のセクションのデバッグを一時的に有効または無効にすることもできます。

### ローカル デバッグ コントロール

より細かく制御するには、ヘルパー関数を使用して、スクリプトの特定の部分内でデバッグをオンまたはオフにすることができます。

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

これにより、デバッグを動的に制御できるようになります。

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## メッセージング システムのデバッグ

Scheme でデバッグ出力を効率的に処理するために、複数のヘルパー関数を含む構造化されたアプローチを使用します。これらの機能により、デバッグ メッセージと警告メッセージが明確で読みやすく、保守しやすくなります。

### デバッグ メッセージング システムの概要

デバッグ メッセージング システムは次のコンポーネントで構成されています。

1. `debug-message` – デバッグが有効な場合にデバッグ メッセージを表示します。
2. `serialize-item` – さまざまな Scheme データ型を文字列表現に変換します。
3. `concat` – 複数の項目を 1 つの文字列に連結します。
4. `list->string` – リストを読み取り可能な文字列にフォーマットします。
5. `message` – Lumi のメッセージ コンソールに出力を表示します。
6. `warning-message` – 警告が有効になっている場合、警告メッセージを表示します。

各関数は、構造化されたメッセージのフォーマットと表示に役割を果たします。

---

### デバッグメッセージ関数

`debug-message` 関数は、デバッグ出力を表示するための中心的なメソッドです。これにより、デバッグが有効な場合にのみメッセージが表示されるようになります。

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug` 条件により、デバッグが有効な場合にのみメッセージが表示されます。
- わかりやすくするために、メッセージには `"> "` という接頭辞が付けられます。
- この関数は `concat` を使用してメッセージの内容をフォーマットします。
- 最後に、`message` を呼び出して、出力を Lumi のメッセージ コンソールに送信します。

使用例:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

デバッグを有効にすると、出力は次のようになります。

```scheme
> item: background-layer has tree position : 3
```

### デバッグ メッセージ用のデータのシリアル化

メッセージには、リスト、ベクトル、数値などのさまざまなデータ型が含まれる場合があります。正しくフォーマットされていることを確認するために、`serialize-item` を使用します。

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

使用例:

```scheme
(serialize-item '(1 2 3))
```

出力:

```scheme
list:
1
2
3
```

### メッセージの連結

複数のメッセージ コンポーネントを 1 つの文字列にマージするには、`concat` を使用します。

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

使用例:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### リストを文字列としてフォーマットする

`list->string` 関数は、リストをフォーマットされた文字列に変換します。

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### 警告メッセージ`warning-message` 関数は `debug-message` と同​​様に動作しますが、デバッグが無効になっている場合でも警告が表示されます。

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- 警告が有効になっている場合にのみメッセージが表示されるようにします (`warning` フラグが `common.scm` に `#t` として設定されています)。
- `concat` を呼び出して、メッセージの内容をフォーマットします。
- `message` を使用して出力を Lumi に送信します。

## 標準機能の強化

デバッグ システムが整ったら、詳細なメッセージを組み込むことで関数ライブラリを強化できます。これにより、項目の状態、変数値、関数呼び出しについての洞察が得られます。

一般的な例は `item-is-valid?` で、これは `lumi-item-id-is-valid` をラップして `#t` または `#f` を返します。 `#f` が返された場合は、呼び出しコードで `warning-message` をトリガーできます。入力が数値でない場合は、関数で警告を与えることができます。

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## 実際の使用法

Scheme プラグインを開発する場合、この方法で関数をラップすると、デバッグ時間が大幅に短縮され、堅牢で保守可能なコードが保証されます。デバッグ システムを導入すると、スイッチを押すだけでエラー コンソールに構造化されたデバッグ ストリームを生成できます。

このデバッグ ストリームでは、関数呼び出しにアスタリスク (*) のマークが付いているため、特に複雑なプラグインでのスクリプトの実行の追跡と障害の特定が容易になります。この可視性は、操作の流れを理解し、予期しない動作を効率的に診断するのに役立ちます。

`*` を使用するメッセージ関数のラッパー

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

実際に使用される `call` の例:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

プラグインが実行されるときのデバッグ ストリームの例:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

この構造化されたログは、関数呼び出しとデータ変更の明確なタイムラインを提供し、デバッグとパフォーマンス分析を大幅に容易にします。

## 結論

構造化されたデバッグ システムを実装することで、実行に関するリアルタイムの洞察を提供する、より安全で保守しやすいスクリプトを作成します。

### 重要なポイント

- **冗長性の制御** – グローバル デバッグ フラグを使用して出力レベルを管理します。
- **明確なフィードバックを提供します** – 標準関数を有益なデバッグ メッセージでラップします。
- **堅牢性の強化** – 予期しない入力を適切に処理して、エラーを防ぎます。
- **トラブルシューティングの簡素化** – 構造化されたデバッグ メッセージにより、問題の診断と修正が容易になります。

このアプローチにより、スクリプトはデータを処理する際に効果的に「説明」するため、フラストレーションが軽減され、ワー​​クフローの効率が向上します。デバッグは事後的な雑事ではなく、プロアクティブなツールとなり、スクリプト作成プロセスがよりスムーズになり、よりやりがいのあるものになります。