---
title: "Thư viện tin nhắn"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
translation_lock: true
url: "hub/scripting/tutorials/First Step/messaging-library"
---
Theo thời gian, chức năng ban đầu là gửi tin nhắn đã phát triển thành một tập hợp các chức năng liên quan. Các chức năng này hiện tạo thành nền tảng của **Thư viện nhắn tin**, được thiết kế để xử lý đầu ra đến các đích khác nhau, chẳng hạn như GUI, Bảng điều khiển tin nhắn và thiết bị đầu cuối hệ điều hành.

### Tại sao lại là Thư viện Tin nhắn?

Khi nhu cầu của chúng ta tăng lên, việc xử lý tin nhắn trên nhiều đầu ra đòi hỏi một cách tiếp cận có tính mô-đun và có thể mở rộng hơn. Thay vì một chức năng duy nhất thực hiện mọi việc, chúng ta đã chia quy trình thành các thành phần có thể tái sử dụng, mang lại sự linh hoạt cao hơn. Thư viện này hiện có thể được sử dụng như một công cụ nhắn tin có mục đích chung mà các plugin hoặc chức năng khác có thể mượn.

### Thư viện nhắn tin làm gì?

Thư viện Tin nhắn hiện bao gồm các chức năng sau:

- **send-to-gui**: Gửi tin nhắn tới hộp thoại Lumi GUI.
- **send-to-error-console**: Gửi tin nhắn đến bảng điều khiển Lumi Message.
- **send-to-terminal**: Gửi tin nhắn đến cửa sổ terminal.
- **send-message**: Chức năng điều phối hướng các tin nhắn đến đầu ra thích hợp.
- **validate-message**: Đảm bảo rằng tin nhắn và đầu ra hợp lệ trước khi gửi.

### Mở rộng Thư viện

**Thư viện tin nhắn** có thể dễ dàng được mở rộng để hỗ trợ các đầu ra bổ sung. Ví dụ:

- **send-to-file**: Lưu tin nhắn vào một tập tin nhật ký.
- **send-to-logger**: Tích hợp với hệ thống ghi nhật ký bên ngoài.
- **gửi đến thông báo**: Hiển thị tin nhắn dưới dạng thông báo hệ thống.

Bằng cách tuân theo cùng một mẫu thiết kế mô-đun và các chức năng có thể tái sử dụng, thư viện này có thể phát triển thành một công cụ toàn diện để xử lý tất cả các loại tác vụ nhắn tin.

## Lợi ích của Thư viện Tin nhắn

- **Khả năng sử dụng lại**: Các chức năng có thể được sử dụng lại trên các plugin hoặc dự án khác nhau.
- **Tính mô-đun**: Mỗi hàm xử lý một tác vụ cụ thể, giúp việc duy trì và mở rộng mã dễ dàng hơn.
- **Tính nhất quán**: Việc sử dụng cùng các chức năng xác thực và xử lý thông báo sẽ đảm bảo hành vi nhất quán trên toàn ứng dụng.

**Thư viện tin nhắn** là sự khởi đầu của một khuôn khổ rộng hơn có thể đơn giản hóa cách quản lý tin nhắn trong dự án của bạn. Khi thư viện phát triển, các plug-in mới có thể dễ dàng sử dụng để gửi tin nhắn đến bất cứ nơi nào chúng cần.

Chúng ta có thể điều chỉnh cấu trúc tập tin:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Và nhớ điều chỉnh `load` trong plug-in chính:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```