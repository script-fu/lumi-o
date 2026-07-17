---
title: "Làm lại"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: af1b2f3233ef50936b24aa195d3a7da50529a4fff3109b087be2f861e15496d1
translation_lock: true
url: "hub/scripting/tutorials/First Step/reworking"
---
Bước này khắc phục một hành vi tinh vi trong ví dụ về nhắn tin.

Chúng ta đã chuyển chuỗi "Xin chào thế giới!\n" làm tin nhắn. "\n" là một loại ký tự đặc biệt, ký tự "thoát". Nó báo cho quá trình in đầu ra bắt đầu một dòng mới. Trong Scheme, nó cũng sẽ buộc một thông báo được gửi đến Thanh trạng thái bật lên dưới dạng hộp GUI.

Hàm trợ giúp `send-to-gui` gửi tin nhắn đến hộp thoại Lumi.

Cập nhật nội dung thư và đích đến để ví dụ hoạt động nhất quán.

Loại bỏ ký tự thoát và mở rộng các chức năng:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

Thay thế các số ma thuật bằng các hằng số do Lumi cung cấp (ví dụ: `MESSAGE-BOX` và `ERROR-CONSOLE`).

Sau đó chia xác thực thành hai chức năng để có thể sử dụng lại từ nhiều trang gọi.

- (is-valid-string?) Để kiểm tra một chuỗi có phải là một chuỗi chứ không phải chuỗi rỗng, trong hàm send-to*.
- (is-valid-output-display?) Để kiểm tra đích đầu ra đã cho có hợp lệ hay không, trong chức năng gửi tin nhắn.

Làm lại thư viện:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Thêm ký tự xuống dòng để buộc hộp thoại hiển thị message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Mục đích: Gửi message tới đích đầu ra phù hợp
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Mục đích: Kiểm tra message là chuỗi không rỗng
(define (is-valid-string? message)
  ;; Kiểm tra message là chuỗi không rỗng
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Mục đích: Kiểm tra message được gửi tới đầu ra hợp lệ
(define (is-valid-output-display? output)
  ;; Kiểm tra đầu ra là một trong các đích hiển thị mong đợi
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Kết luận

Bằng cách làm lại thư viện nhắn tin, chúng ta đã làm cho nó trở nên mạnh mẽ và đáng tin cậy hơn. Chúng ta đã khắc phục sự cố ẩn với ký tự dòng mới, giới thiệu các hằng số để rõ ràng hơn và mở rộng chức năng bằng cách thêm hỗ trợ cho đầu ra của thanh trạng thái và hộp thoại. Ngoài ra, việc tách logic xác thực thành các hàm tập trung, nhỏ hơn sẽ đảm bảo rằng mã của chúng ta dễ bảo trì và mở rộng hơn trong tương lai.

Việc làm lại này cho thấy những thay đổi nhỏ có thể nâng cao cấu trúc và chức năng tổng thể của thư viện của chúng ta như thế nào, mở đường cho sự linh hoạt và khả năng sử dụng lại cao hơn khi dự án của chúng ta phát triển.