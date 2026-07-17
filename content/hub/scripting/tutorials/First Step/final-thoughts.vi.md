---
title: "suy nghĩ cuối cùng"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
translation_lock: true
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
Bây giờ bạn đã có một plug-in quy trình làm việc và một thư viện trợ giúp nhỏ. Loạt bài này đã giới thiệu các mẫu cốt lõi mà bạn sẽ sử dụng trong hầu hết các tập lệnh Lumi:

- Chức năng: Các khối xây dựng của các plug-in của chúng ta.
- Tái cấu trúc: Cải thiện cấu trúc mã trong khi vẫn duy trì chức năng.
- Thư viện mã: Tập trung các chức năng có thể tái sử dụng để giữ cho mã của chúng ta sạch sẽ và theo mô-đun.
- Kỹ thuật xác thực: Đảm bảo rằng đầu vào hợp lệ trước khi thực hiện logic cốt lõi của chúng ta.

Bạn cũng đã biết những kiến thức cơ bản về cách sử dụng Git để theo dõi các thay đổi và duy trì cấu trúc dự án rõ ràng. Quy trình làm việc đó giúp việc lặp lại dễ dàng hơn mà không làm mất các phiên bản đang hoạt động.

Đây là phiên bản cuối cùng của mã plug-in chính của chúng ta:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Mã thư viện:

```scheme
;; Mục đích: Gửi message tới thanh trạng thái, trả về #t nếu thành công
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Mục đích: Gửi message tới hộp thoại, trả về #t nếu thành công
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Mục đích: Gửi message tới bảng điều khiển lỗi, trả về #t nếu thành công
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Mục đích: Gửi message tới terminal, trả về #t nếu thành công
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Mục đích: Gửi message tới đích phù hợp, trả về #t nếu thành công
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Mục đích: Kiểm tra message là chuỗi không rỗng, trả về #t nếu hợp lệ
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Mục đích: Kiểm tra đầu ra là đích hợp lệ, trả về #t nếu hợp lệ
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Kết luận

Bằng cách tái cấu trúc các trình trợ giúp nhắn tin thành một thư viện nhỏ, plug-in vẫn tập trung vào mục đích và thư viện chứa các chi tiết triển khai. Việc xác thực và định tuyến thông báo nhất quán giúp dự đoán được lỗi.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Các bước tiếp theo:

- Di chuyển những người trợ giúp có thể tái sử dụng vào một tệp thư viện chuyên dụng.
- Giữ các plug-in nhỏ gọn và đặt tên cho những gì chúng thực hiện.
- Thêm xác nhận tại các ranh giới (đầu vào, đường dẫn tệp, tùy chọn menu).

Giữ kết quả cuối cùng dưới dạng hai tệp trong kho lưu trữ plug-in của bạn:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`