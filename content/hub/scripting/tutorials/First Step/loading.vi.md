---
title: "Đang tải"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
Ngay khi hàm trợ giúp phát triển, hãy chuyển nó vào một tệp thư viện nhỏ. Điều đó giữ cho plug-in luôn tập trung và giúp trình trợ giúp có thể sử dụng lại được trên nhiều plug-in.

### Tạo chức năng thư viện

Chúng ta có thể lấy chức năng gửi tin nhắn và tạo một tệp mới với nội dung đó. Lưu tệp vào thư mục repo của bạn, không phải plug-in, có thể ở gần cấp cao nhất;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Đây là thư mục chính để lưu trữ mã Scheme của bạn.
  - **library/**: Đây là nơi hoạt động của các chức năng được chia sẻ như `send-message.scm`.
  - **plug-ins/**: Đây là nơi lưu trữ các plug-in riêng lẻ của bạn.
    - **hello-world/**: Thư mục cho plug-in "Hello World!" cụ thể.
      - **hello-world.scm**: Tệp script của plug-in.

Ví dụ về hàm thư viện send-message.scm

```scheme
;; Hàm xử lý đầu ra message tới nhiều đích
(define (send-message message output)
  (cond
    ;; Gửi tới Bảng điều khiển tin nhắn
    ((eq? output 'error-console)
       ;; Đặt trình xử lý cho Bảng điều khiển tin nhắn
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Gửi tới hộp thoại GUI
    ((eq? output 'gui)
       ;; Đặt trình xử lý cho hộp thoại GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Gửi tới cửa sổ terminal
    ((eq? output 'terminal)
       ;; Đầu ra terminal được xử lý bằng display
       (display message)))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))
```

### Tải chức năng thư viện

Chúng ta có thể tải chức năng thư viện đó bằng lệnh Scheme `load`;

Đang tải một tập tin thư viện:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

Này! Bây giờ chúng ta đã có thứ gì đó đơn giản hơn và ngắn hơn để đọc, kiểu đó tự mô tả mà không cần bình luận. Đây là kết luận thỏa đáng của việc tái cấu trúc.