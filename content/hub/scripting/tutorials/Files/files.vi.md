---
title: "Tập tin"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
url: "hub/scripting/tutorials/files"
translation_lock: true
---
Làm việc với các tập tin và thư mục là điều cần thiết để phát triển Scheme. Cho dù bạn đang lưu đầu ra, tải tài nguyên hay sắp xếp cấu trúc dự án của mình, việc hiểu các thao tác trên tệp sẽ giúp tập lệnh của bạn mạnh mẽ hơn và thân thiện với người dùng hơn.

Trang này bao gồm các tác vụ thư mục và tệp phổ biến: đọc đường dẫn, tạo thư mục và thu thập thông tin đầu vào thư mục thông qua các tham số GUI.

## Thư mục chính của người dùng

Lumi chỉ dành cho Linux, vì vậy thư mục chính của người dùng đến từ biến môi trường `HOME`.

Để lấy thư mục chính của người dùng dưới dạng chuỗi:

```scheme
(getenv "HOME")
```

Đầu ra ví dụ:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Ngoài ra còn có biến toàn cục `DIR-SEPARATOR`, là biến phân cách đường dẫn dành riêng cho nền tảng. Trong Lumi (Linux), nó luôn là `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Lấy vị trí thư mục

Chúng ta có thể yêu cầu người dùng cung cấp vị trí thư mục trong hộp thoại Scheme cho một plug-in.

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

`SF-DIRNAME` cung cấp trình duyệt cho một thư mục.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Ở đây, chúng ta xác thực hai đầu vào thư mục (nguồn và đích) và quay lại mặc định nếu đường dẫn GUI trống/không hợp lệ.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Nếu bạn quan tâm đến chi tiết triển khai, hãy tìm kiếm nguồn plugin cho `validate-path-and-dir`.

## Tạo một thư mục

Scheme cung cấp lệnh ```dir-make``` để tạo thư mục. Lệnh này lấy một đường dẫn được phân tách bằng dấu "/" và tạo một thư mục duy nhất có tham số tùy chọn cho các đặc quyền. Chúng ta không cung cấp cho nó đường dẫn dành riêng cho nền tảng.

Thông thường chúng ta cần tạo nhiều thư mục cho một đường dẫn thực tế. Chúng ta có thể sử dụng trình bao bọc cho ```dir-make``` để trợ giúp chúng ta ở đây.

```scheme
;; Mục đích: Trình bao bọc (dir-make) tạo đường dẫn cho nền tảng
;;          đường dẫn được cung cấp. Luôn dùng dấu phân cách kiểu Linux cho dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Thư mục gốc
    ;; Tạo các thư mục còn lại từng bước
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; xây dựng đường dẫn
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Lưu ý: Chức năng này cũng sử dụng ```file-exists?``` tích hợp để bỏ qua các cuộc gọi không cần thiết. Nó trả về #t nếu tệp hoặc thư mục được chỉ định tồn tại và #f nếu nó không tồn tại hoặc nếu người dùng yêu cầu không thể truy cập được.

## Xây dựng đường dẫn

Chúng ta cũng cần chia nhỏ và xây dựng lại các đường dẫn trong Scheme.

Để chia đường dẫn thành nhiều phần, hãy sử dụng ```strbreakup```:

### Ví dụ về đường dẫn Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Lưu ý: Dấu gạch chéo đầu và cuối trở thành thành phần chuỗi trống trong danh sách kết quả.

Để xây dựng lại đường dẫn, hãy sử dụng ```string-append```:

### Xây dựng đường dẫn Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```