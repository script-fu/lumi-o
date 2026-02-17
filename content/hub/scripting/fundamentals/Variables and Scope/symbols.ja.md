---
title: "記号"
type: docs
weight: 6
---
シンボルは Scheme の中核となるデータ型の 1 つであり、一意で不変の識別子を表します。これらは主にプログラム内のキー、マーカー、またはプレースホルダーとして使用され、クリーンで表現力豊かなコードを記述するために不可欠です。

Scheme のシンボルは文字列に似ていますが、シンボルが**一意**で**アトミック**である点が異なります。これは、同じ名前を持つ 2 つのシンボルが同じオブジェクトであることが保証され、高速な等価性チェックとデータ構造での効率的な使用が可能になることを意味します。

### 構文

シンボルは一連の文字として記述されます。

- 文字で始まり、その後に文字、数字、または `-`、`+`、`*` などの特殊文字が続きます。
- デフォルトでは、記号は大文字と小文字が区別されます。

例:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## シンボルの作成

通常、シンボルは **quote** 演算子 (`'`) を使用して作成されます。これは、名前を変数または関数として評価するのではなくシンボルとして扱うように Scheme に指示します。

### 例

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

文字列をシンボルに変換する `string->symbol` プロシージャを使用して、プログラムでシンボルを作成することもできます。

```scheme
(string->symbol "dynamic-symbol")
```

**結果**: `'dynamic-symbol`


## シンボルの比較

シンボルは一意であるため、`eq?` を使用して効率的に比較できます。

### 例

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

これにより、シンボルはデータ構造内のキーやコード内のマーカーとして使用するのに最適になります。

## シンボルの使用

シンボルは、Scheme で次の目的でよく使用されます。

1. **関連リストのキー:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **コード内の識別子:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## シンボルを操作する手順

Scheme には、シンボルを操作するための組み込みプロシージャが用意されています。

|手順 |説明 |
|------------------|-----------------------------------------------------------------------------|
| **`symbol?`** |オブジェクトがシンボルかどうかを確認します。                                            |
| **`eq?`** | 2 つのシンボルを同一性について比較します (高速比較)。                       |
| **`symbol->string`** |シンボルを文字列に変換します (表示またはデバッグに役立ちます)。          |
| **`string->symbol`** |文字列をシンボルに変換します (識別子の動的な作成に役立ちます)。 |

### 例

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## 概要

シンボルは、Scheme で識別子、キー、マーカーを表現するための軽量で効率的な方法です。不変性と高速な ID チェックにより、多くのプログラミング タスクに最適です。シンボルを効果的に使用する方法を理解すると、クリーンで表現力豊かな Scheme コードを作成する能力が向上します。