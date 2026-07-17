---
title: "Trình duyệt quy trình"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: f2ea095c0407f9641d28803e937a992e044584f6bcbed960239d0c0df4b430d2
url: "hub/scripting/tutorials/first-step/the-procedure-browser"
translation_lock: true
---
**Trình duyệt quy trình Lumi** cho phép bạn tìm kiếm các quy trình có sẵn (được tích hợp sẵn và cung cấp plug-in) cũng như kiểm tra các tham số và giá trị trả về của chúng.

### Tìm trình duyệt quy trình Lumi ở đâu

Bạn có thể truy cập Trình duyệt quy trình trong Lumi thông qua menu **Trợ giúp**:

- **Trợ giúp** -> **Trình duyệt quy trình**

### Trình duyệt quy trình làm gì

Trình duyệt Quy trình liệt kê tất cả các quy trình nội bộ của Lumi, cùng với những quy trình được bổ sung bởi các plug-in, bao gồm cả quy trình bạn vừa cài đặt. Mỗi mục thủ tục cung cấp thông tin hữu ích, bao gồm:

- Tên thủ tục.
- Một mô tả về những gì nó làm.
- Các tham số nó chấp nhận (giá trị đầu vào).
- Các giá trị trả về (đầu ra).

Tìm kiếm theo từ khóa hoặc tên thủ tục khi bạn cần xác minh chữ ký cuộc gọi hoặc xác nhận tên thủ tục chính xác.

#### (lumi-message) trong Trình duyệt quy trình

Tìm kiếm `lumi-message` để xem các tham số và giá trị trả về của nó.

### Tìm plugin của bạn

Sau khi bạn đã cài đặt "Xin chào thế giới!" plug-in, bạn có thể tìm thấy nó được liệt kê trong Trình duyệt quy trình. Chỉ cần tìm kiếm tên hàm mà bạn đã đăng ký với Lumi, trong trường hợp này là "scheme-hello-world". Mục nhập sẽ hiển thị các tham số và mọi giá trị trả về được liên kết với plug-in, cùng với mô tả ngắn gọn. Bạn cũng sẽ thấy vị trí một số dòng văn bản bạn đã nhập làm tham số đầu vào trong quá trình đăng ký được hiển thị trong phần **Thông tin bổ sung**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Tên quy trình
  "Hello world!"                                        ;; Tên mục menu
  "A Scheme procedure plug-in"                       ;; Tooltip và mô tả
  "Your Name"                                           ;; Tác giả
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; Giấy phép
  "2024")                                               ;; Ngày bản quyền
```

Điều này giúp bạn dễ dàng xác minh rằng plugin của bạn đã được đăng ký đúng cách và cung cấp cho bạn cách nhanh chóng để xem lại cách nó tương tác với các quy trình khác trong Lumi. Trình duyệt quy trình là một công cụ mạnh mẽ để gỡ lỗi và mở rộng các plug-in của bạn bằng cách khám phá tất cả các quy trình có sẵn trong Lumi.