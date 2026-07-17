---
title: "Xin chào thế giới!"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: c250d07dff926c7b51434efc644786f35b5189e03449dcdf4ec5916c1c151886
translation_lock: true
url: "hub/scripting/tutorials/First Step/hello-world"
---
Hướng dẫn này hướng dẫn cấu trúc tối thiểu của plug-in _procedure_ Scheme. Một số dòng là "bản soạn sẵn": chúng được yêu cầu để Lumi tải tệp, ngay cả khi bạn chưa hiểu đầy đủ về chúng.

```bash
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

Ở cấp độ cao, bạn sẽ:

1. Xác định hàm
2. Đăng ký nó để nó xuất hiện trong Cơ sở dữ liệu thủ tục
3. (Tùy chọn) Thêm mục menu
4. Cài đặt file vào thư mục plug-ins

### Xác định hàm

Hàm, còn được gọi là _procedure_, là một đoạn mã có tên và mục đích, nó nhận đầu vào và tạo ra đầu ra.

**Đầu vào** > **_Function_** > **Đầu ra**

### Đăng ký hàm

Đăng ký là việc đưa tên chức năng vào danh sách để Lumi biết về nó.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Liên kết tới Menu

Điều này cho Lumi biết nơi tìm thấy chức năng của bạn trong hệ thống menu của nó.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Thao tác này sẽ hiển thị menu "Funky" trên thanh menu chính. Thay đổi đường dẫn để đặt plug-in ở nơi khác. Đường dẫn `<Image>/Funky` có nghĩa là plugin sẽ xuất hiện trong danh mục menu **Hình ảnh**. Bạn có thể thay đổi `<Image>` thành `<Tools>`, `<Filters>`, v.v., tùy thuộc vào nơi bạn muốn plugin xuất hiện.

### Bình luận

Trong Scheme, ngôn ngữ cơ sở của Scheme, chú thích thường được thực hiện bằng cách đặt `;;` trước một dòng văn bản hữu ích. Việc bạn dùng chú thích sẽ phụ thuộc vào mức độ trôi chảy của bạn với tư cách là một lập trình viên—nếu thỉnh thoảng bạn viết mã, nhiều chú thích hơn sẽ hữu ích. Nếu bạn viết mã mọi lúc, mã sẽ dễ đọc như chú thích. Ngoài ra, khi lập trình theo chức năng, mã có xu hướng mang tính mô tả đủ để đọc giống như một tập lệnh.

### Cú pháp

Mã có xu hướng có ít quy tắc về cách đặt các mục trong một dòng để chúng ta có thể đọc dòng đó một cách dễ dàng. Ví dụ: một câu có thể có dấu cách sau dấu phẩy hoặc dấu chấm. Nó giúp dễ đọc.

Mã có thể sắp xếp mọi thứ theo cách tương tự, thoạt nhìn có thể trông kỳ quặc:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Mã ví dụ

Đây là ví dụ đầy đủ. Hầu hết các quy trình của Lumi đều có tiền tố `lumi-`. Ví dụ: `lumi-message` in một chuỗi tới trình xử lý tin nhắn đã định cấu hình.

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Đặt trình xử lý message để xuất ra hộp thoại GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Đặt trình xử lý message để xuất ra Bảng điều khiển lỗi
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Gửi message tới terminal, cửa sổ OS đã khởi chạy Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Cài đặt Plug-in

1. Đi tới **Lumi -> Chỉnh sửa -> Tùy chọn -> Thư mục -> Phần bổ sung**.
2. Thêm thư mục plug-in [repo](/hub/scripting/tools/git) của bạn vào danh sách.
3. Tạo một thư mục cho plug-in và lưu mã ví dụ ở trên dưới dạng `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Nhấp chuột phải vào tệp `hello-world.scm`.
5. Đi tới **Thuộc tính -> Quyền -> Cho phép thực thi tệp dưới dạng chương trình**.
6. Khởi động lại Lumi.

### Hãy thử Plug-in

Plug-in bây giờ sẽ xuất hiện trong menu "Funky" trong cửa sổ chính của Lumi. Nhấp vào nó và nó sẽ hiển thị "Xin chào thế giới!" tin nhắn. Hãy thử sửa đổi mã, chẳng hạn như thay đổi nội dung tin nhắn và lưu tệp. Khi bạn chạy lại plug-in, những thay đổi của bạn sẽ được phản ánh mà không cần khởi động lại Lumi.

Hãy thử trải nghiệm bằng cách thay đổi đường dẫn menu. Ví dụ: `"<Image>/File"` sẽ đặt nó bên trong menu Tệp và `"<Image>/File/Funky"` sẽ tạo một phần mới trong menu Tệp. Đây là một cách tuyệt vời để tùy chỉnh vị trí plug-in của bạn xuất hiện và sắp xếp các công cụ của bạn.