---
title: "文件"
type: docs
weight: 7
---
使用檔案和目錄對於Scheme 開發至關重要。無論您是儲存輸出、載入資源還是組織專案結構，了解文件操作都將使您的腳本更加健全且使用者友好。

本頁涵蓋常見的檔案和目錄任務：讀取路徑、建立目錄以及透過 GUI 參數收集資料夾輸入。

## 使用者的主目錄

Lumi 僅適用於 Linux，因此使用者的主目錄來自 `HOME` 環境變數。

取得字串形式的使用者主目錄：

```scheme
(getenv "HOME")
```

輸出範例：

```scheme
"/home/username"
```

## 目錄分隔符

還有全域變數`DIR-SEPARATOR`，它是特定於平台的路徑分隔符號。在 Lumi (Linux) 中，它總是`/`。

```scheme
> DIR-SEPARATOR
"/"
```

## 取得目錄位置

我們可以在方案對話框中詢問使用者插件的目錄位置。

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

`SF-DIRNAME` 提供目錄瀏覽器。

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

在這裡，我們驗證兩個目錄輸入（來源和目標），如果 GUI 路徑為空/無效，則回退到預設值。

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

如果您對實現細節感興趣，請搜尋插件來源`validate-path-and-dir`。

## 製作目錄

Scheme 提供```dir-make``` 指令來建立目錄。此命令採用“/”分隔路徑並建立一個帶有可選權限參數的目錄。我們不給它特定於平台的路徑。

通常我們需要為實際路徑建立多個目錄。我們可以使用 ```dir-make``` 的包裝器來幫助我們。

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

注意：函數也使用內建的```file-exists?```來跳過不必要的呼叫。如果指定的檔案或目錄存在，則傳回#t；如果不存在或請求使用者無法存取它，則傳回#f。

## 建置路徑

我們還需要對Scheme中的路徑進行分解和重建。

若要將路徑拆分為多個部分，請使用```strbreakup```：

### Linux 路徑範例

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> 注意：前導斜線和尾隨斜線將成為結果清單中的空字串元素。

若要重建路徑，請使用```string-append```：

### Linux 路徑構建

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
````