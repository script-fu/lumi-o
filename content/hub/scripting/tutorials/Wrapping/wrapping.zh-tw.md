---
title: "包裝"
type: docs
weight: 4
---
方案命令在較低層級上運行，這意味著即使是簡單的任務也可能需要多個步驟。然而，這種粒度提供了靈活性，我們可以將指令捆綁到小的、可重複使用的函數中，從而完全滿足我們的需求。環繞不是一個非黑即白的概念，而是一個非黑即白的概念。它的範圍可以從常用命令的簡單別名到管理整個工作流程的更複雜的功能。有時，包裝器只是一個提高可讀性的便利函數，而在其他情況下，它會演變成封裝多個操作的全功能實用程式。

### 為什麼要包裝函數？

包裝函數有幾個主要好處：

- **簡化重複任務** – 不要重複低階命令，而是將它們包裝在輔助函數中並重複使用它。
- **提高可讀性** – 為我們的包裝函數提供清晰、描述性的名稱，使我們的程式碼更容易一目了然。
- **封裝複雜性** – 我們可以將它們分解為更小的、結構良好的輔助函數，而不是處理長而神秘的命令列表、深層嵌套循環或複雜的訊息語句。
- **增強可維護性** – 如果命令的核心功能發生變化，我們只需更新我們的包裝函數一次，從而使我們的插件不受這些更改的細節影響。
- **鼓勵程式碼重複使用** – 每個助手都成為庫的一部分，使未來的腳本可以更快地編寫和調試。

隨著插件的增長，包裝器可以幫助您保持核心邏輯的可讀性並隔離重複的細節。

包裝函數的另一個優點是將它們整合到語法突出顯示器中，例如 Visual Studio Code。這提高了可讀性和導航性，使腳本更加清晰。在使用自訂函數的插件中，任何綠色突出顯示的函數都確認它已從我們的庫中正確引用。

如果您維護自己的幫助程式庫，請考慮將項目的函數名稱新增至編輯器的語法反白顯示。它使導航和重構更快。

範例：

### 隨機種子

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

雖然我們可以直接在程式碼中使用 ***msrg-rand***，但將其包裝在名為 ***random-seed*** 的函數中可以提高可讀性。透過為函數指定一個清晰且具有描述性的名稱，可以更容易一目了然地理解其用途。

此外，將***隨機種子***定義為獨立函數使我們可以在插件中的任何位置使用它，同時將實現集中在單一位置。如果我們需要更改種子的生成方式，我們只需要更新此函數，而其餘程式碼保持不變。

例如，如果我們決定切換到***隨機***：

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

函數名稱保持不變，確保我們的腳本無需修改即可繼續工作。這種方法使我們的程式碼靈活、可維護且易於閱讀。

### JPEG 匯出

Scheme 中的 JPEG 導出功能帶有許多參數，可以對影像的保存方式進行精細控制。然而，在大多數情況下，我們只關心一些關鍵設置，例如文件名和品質。為了簡化過程，我們可以包裝該函數。

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

在此包裝函數中，大多數匯出選項都是硬編碼的，僅公開我們可能調整的參數：檔案名稱和品質。這種方法提高了可讀性並使保存圖像更簡單。另外，如果Lumi的導出器將來發生變化，我們只需要更新這一個函數，而不需要修改每個導出JPEG的腳本。

### 使用包裝器

要在我們的插件中匯出 JPEG，我們只需包含該庫並調用我們的自訂函數：

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

這使我們的程式碼保持乾淨、可讀和適應性強，同時允許我們以最小的努力有效地導出 JPEG。

### 汽車更換

***car*** 函數可能很神秘且容易出現腳本錯誤。很容易錯誤地將 ***car*** 應用於向量或非列表項，從而導致意外行為。為了讓我們的程式碼更加健壯和可讀，我們可以將此功能包裝在一個更安全的函數中。

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

此函數安全地檢索清單或向量的第一項，同時在遇到無效或空輸入時提供有用的警告。透過使用***first-item***而不是***car***，我們降低了意外錯誤的風險並提高了腳本的清晰度。

#### 為什麼要使用這個包裝器？

- **防止腳本崩潰** – 避免將 ***car*** 應用於非清單而導致的錯誤。
- **支援清單和向量** – 將可用性擴展到清單之外。
- **提供有意義的警告** – 幫助偵錯意外的輸入問題。
- **提高可讀性** – 函數名稱清楚地傳達了其用途。

透過將這個邏輯封裝在第一項中，我們使我們的插件更加健壯並且更易於維護。當然，這取決於個人喜好，直接使用 car、caar、cadr 和類似的Scheme 函數可能會完全舒適。

### 包裝一個包裝函數

包裝已經包裝過的函數可以進一步提高可讀性和可維護性。例如，當使用像***像素座標（列表100 200）***這樣的座標對時，我們可以使用：

```scheme
(first-item pixel-coords)
```

檢索 ***x*** 座標。然而，雖然功能齊全，但表現力並不強。相反，我們可以用更合適的定義來包裝***first-item***，以使我們的意圖更清晰。

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### 為什麼要使用這種方法？

- **增強程式碼清晰度** – 我們不使用通用清單存取函數，而是明確定義描述其用途的函數。
- **提高可維護性** – 如果我們的座標表示發生變化（例如，使用向量而不是列表），我們只需要更新這些小函數。
- **鼓勵一致性** – 使用 ***x-coord*** 和 ***y-coord*** 使腳本更易於閱讀和一目了然。

現在，不要用通用方案寫：

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

我們可以在_our_Scheme中寫：

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

透過將低階函數包裝在有意義的名稱中，我們創建了一種更直觀的資料處理方式，減少了混亂和潛在的錯誤。

### 結論

包裝函數是簡化Scheme開發的有效方法，使腳本更具可讀性、可維護性和健壯性。透過封裝複雜性並僅公開必要的細節，我們創建了一種更結構化的插件編寫方法。

這種方法的主要要點：- **簡化重複性任務** – 我們建立可重複使用的函數，而不是手動重複低階命令。
- **提高程式碼可讀性** – 命名良好的包裝器使腳本更易於理解。
- **封裝複雜性** – 低階細節在包裝器內部處理，保持主腳本乾淨。
- **增強可維護性** – 如果核心功能發生變化，我們只需要更新包裝器，而不是每個依賴它的腳本。
- **鼓勵重複使用和一致性** – 我們的個人函數庫隨著時間的推移而不斷增長，使開發更快、更有效率。

透過一致地使用函數包裝，我們可以改變編寫Scheme插件的方式，創造一個更模組化、更具表現力的腳本環境。牢記這些原則，我們可以繼續完善我們的方法，開發更有效率、更客製化的方案版本，以滿足我們的特定需求。

後續步驟：識別腳本中的重複區塊並提取具有清晰名稱的小助手。