---
title: "Plug-in bộ lọc"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
translation_lock: true
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
Chúng ta đã sử dụng plug-in _procedure_ cho hướng dẫn [Bước đầu tiên](../../first-step/). Những loại plugin này hoạt động mà không cần hình ảnh hoặc drawable làm đầu vào. Thông thường, chúng ta sử dụng một plugin để thay đổi hình ảnh và các drawable của nó. Các plug-in như thế này được gọi là plug-in _filter_.

### Drawable là gì?

**drawable** trong Lumi đề cập đến phần tử hình ảnh có thể vẽ lên, chẳng hạn như lớp hoặc kênh. Các plug-in bộ lọc thường hoạt động trên các phần tử này.

### Ví dụ về Plug-in bộ lọc đơn giản

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Dùng let để định nghĩa biến message và mã lõi
  (let ((message "hello, world"))
    ;; Hiển thị message trong bảng điều khiển lỗi của Lumi
    (lumi-message message)
    ;; Đảo màu drawable đã chọn đầu tiên
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Đăng ký plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Tên quy trình chính
  "Simple Filter Plug-in Demo"             ;; Tên hiển thị trong menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Mô tả tooltip
  "Author Name"                            ;; Ghi công cho bản thân
  "License"                                ;; Giấy phép
  "Date written"                           ;; Ngày viết
  "*"                                      ;; Cho biết plug-in này yêu cầu hình ảnh
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Yêu cầu một hoặc nhiều drawable đã chọn

;; Chỉ định vị trí menu cho plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Sao chép văn bản và lưu nó dưới dạng `simple-filter-plug-in.scm` trong thư mục có tên `simple-filter-plug-in` trong một trong các thư mục plug-in của Lumi. Thư mục plug-in của Lumi là thư mục _any_ được liệt kê bên dưới:
 **Lumi > Chỉnh sửa > Tùy chọn > Thư mục > Plug-in**

Trong Linux, nhấp chuột phải vào tệp `simple-filter-plug-in.scm`, đi tới **Thuộc tính > Quyền** và chọn **Cho phép thực thi tệp dưới dạng chương trình**. Khi tệp đã ở đúng vị trí, có thể thực thi được và không có lỗi cú pháp, khi khởi động lại Lumi, nó sẽ xuất hiện ở thanh tiêu đề menu trên cùng, bên trong menu có tên **Plug-in**.

### Chạy Plug-in

1. Mở một hình ảnh (plug-in bộ lọc này yêu cầu hình ảnh hoạt động).
2. Mở **Công cụ > Gỡ lỗi > Bảng điều khiển tin nhắn** để xem thông báo.
3. Chọn **Bản demo plug-in bộ lọc đơn giản** từ trình đơn **Plug-in**.
4. Một trong các lớp được chọn sẽ bị đảo màu và một thông báo sẽ được in ra bảng điều khiển lỗi.

### Chỉnh sửa Plug-in

Bạn có thể tùy chỉnh plug-in bằng cách chỉnh sửa tệp `.scm` của plug-in này. Ví dụ: để thay đổi thông báo được hiển thị:

1. Mở tệp và tìm dòng xác định `message`.
2. Thay thế `"hello, world"` bằng văn bản tùy chỉnh của bạn.
3. Lưu tệp.

Trong phiên bản Lumi 3, các plug-in không cần làm mới để các thay đổi đã lưu có hiệu lực. Chỉ cần chạy lại plug-in để xem thông báo cập nhật.

### Kiểm tra plug-in

#### Tuyến Shebang

Dòng đầu tiên đảm bảo tập lệnh hoạt động như một plug-in trong Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Định nghĩa thủ tục

Quy trình chấp nhận hai đối số: hình ảnh hiện hoạt và các drawable được chọn.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Logic lõi

Câu lệnh `let` xác định một biến và thực hiện các thao tác trên drawable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Hiển thị message trong bảng điều khiển lỗi của Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Đảo màu drawable đã chọn đầu tiên
```

### Đăng ký plug-in

Plug-in được đăng ký với Lumi dưới dạng plug-in bộ lọc:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Đăng ký quy trình chính
  "Simple Filter Plug-in Demo"             ;; Tên hiển thị trong menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Mô tả tooltip
  "Author Name"                            ;; Tên tác giả
  "License"                                ;; Loại giấy phép
  "Date written"                           ;; Ngày viết
  "*"                                      ;; Cho biết plug-in yêu cầu hình ảnh
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Yêu cầu một hoặc nhiều drawable đã chọn
```

#### Đăng ký thực đơn

Dòng này chỉ định vị trí menu cho plugin:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Khắc phục sự cố

Nếu một plug-in không xuất hiện, hãy kiểm tra vị trí, tên và thuộc tính thực thi của nó.

Vị trí phải nằm trong đường dẫn tìm kiếm plug-in.
Tên file phải trùng với tên thư mục chứa.
Tệp phải được đặt là có thể thực thi được.


**Bảng điều khiển tin nhắn** là một công cụ có giá trị để khắc phục sự cố các plug-in tùy chỉnh. Nếu plugin của bạn không hoạt động như mong đợi, hãy kiểm tra tại đây để biết thông báo lỗi hoặc nhật ký. Cửa sổ **Terminal** cũng có thể cung cấp thông tin gỡ lỗi và báo cáo sự cố tải.