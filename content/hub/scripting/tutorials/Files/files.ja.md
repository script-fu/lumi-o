---
title: "ファイル"
type: docs
weight: 7
---
ファイルとディレクトリの操作は、Scheme 開発にとって不可欠です。出力の保存、リソースの読み込み、またはプロジェクト構造の整理のいずれの場合でも、ファイル操作を理解すると、スクリプトがより堅牢で使いやすくなります。

このページでは、ファイルとディレクトリの一般的なタスク (パスの読み取り、ディレクトリの作成、GUI パラメーターを介したフォルダー入力の収集) について説明します。

## ユーザーのホーム ディレクトリ

Lumi は Linux 専用であるため、ユーザーのホーム ディレクトリは `HOME` 環境変数から取得されます。

ユーザーのホーム ディレクトリを文字列として取得するには:

```scheme
(getenv "HOME")
```

出力例:

```scheme
"/home/username"
```

## DIR-区切り文字

プラットフォーム固有のパス区切り文字であるグローバル変数 `DIR-SEPARATOR` もあります。 Lumi (Linux) では、常に `/` です。

```scheme
> DIR-SEPARATOR
"/"
```

## ディレクトリの場所の取得

プラグインの [スキーム] ダイアログでユーザーにディレクトリの場所を尋ねることができます。

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME` は、ディレクトリへのブラウザを提供します。

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

ここでは 2 つのディレクトリ入力 (ソースと宛先) を検証し、GUI パスが空または無効の場合はデフォルトに戻ります。

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

実装の詳細に興味がある場合は、プラグイン ソースで `validate-path-and-dir` を検索してください。

## ディレクトリの作成

Scheme には、ディレクトリを作成するための ```dir-make``` コマンドが用意されています。このコマンドは、「/」で区切られたパスを使用し、特権のオプションのパラメータを含む単一のディレクトリを作成します。プラットフォーム固有のパスは与えません。

通常、実際のパスとして複数のディレクトリを作成する必要があります。ここで ```dir-make``` のラッパーを使用すると便利です。

```scheme
;; Purpose: A wrapper for (dir-make) that creates a given path from a platform
;;          supplied path. Always emits Linux style separators for dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Root dir
    ;; Create the rest of the directories step-by-step
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; build the path
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

注: この関数は、組み込みの ```file-exists?``` も使用して、不要な呼び出しをスキップします。指定されたファイルまたはディレクトリが存在する場合は #t を返し、存在しない場合、または要求したユーザーがアクセスできない場合は #f を返します。

## パスの構築

また、Scheme でパスを分解して再構築する必要もあります。

パスを複数の部分に分割するには、```strbreakup``` を使用します。

### Linux パスの例

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> 注: 結果のリストでは、先頭と末尾のスラッシュは空の文字列要素になります。

パスを再構築するには、```string-append``` を使用します。

### Linux パスの構築

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
「」