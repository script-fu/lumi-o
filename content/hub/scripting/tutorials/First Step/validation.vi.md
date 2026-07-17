---
title: "Xác thực"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
translation_lock: true
url: "hub/scripting/tutorials/First Step/validation"
---
Khi xây dựng các plug-in mạnh mẽ, điều quan trọng là phải đảm bảo rằng các chức năng của chúng ta xử lý lỗi một cách khéo léo và hoạt động như mong đợi, ngay cả trong trường hợp sử dụng sai mục đích hoặc đầu vào không mong muốn. Việc xác thực giúp bảo vệ tính toàn vẹn của chức năng và ngăn ngừa sự cố hoặc hành vi ngoài ý muốn.

Hãy xem cách chúng ta có thể cải thiện hàm `send-message` bằng cách thêm các bước kiểm tra xác thực để đảm bảo hàm này xử lý dữ liệu đầu vào chính xác.

### Xác thực đầu vào

Trước khi gửi tin nhắn, chúng ta nên đảm bảo đối số `output` được truyền cho hàm `send-message` là hợp lệ. Chúng ta có thể thêm kiểm tra để xác nhận rằng đích đầu ra là một trong các giá trị mong đợi (gui, bảng điều khiển lỗi hoặc thiết bị đầu cuối).

Ví dụ:

```scheme
(define (send-message message output)
  ;; Kiểm tra đối số đầu ra
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Gửi tới Bảng điều khiển tin nhắn
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Gửi tới hộp thoại GUI
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Gửi tới cửa sổ terminal
      ((eq? output 'terminal)
         (display message))))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))
```

Trong ví dụ này, chúng ta sử dụng `member` để kiểm tra xem đối số `output` có hợp lệ hay không. Nếu không, hàm này sẽ báo lỗi kèm theo thông báo rõ ràng, ngăn chặn các giá trị không hợp lệ gây ra sự cố.

### Xử lý tin nhắn trống

Việc đảm bảo rằng đối số `message` cũng hữu ích. Ví dụ: nếu một chuỗi trống hoặc #f (false) được truyền dưới dạng thông báo thì hàm sẽ xử lý việc này một cách khéo léo.

Ví dụ về xử lý một tin nhắn trống:

```scheme
(define (send-message message output)
  ;; Kiểm tra message rỗng
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Cách tiếp cận này đảm bảo rằng hàm luôn nhận được đầu vào hợp lệ, cải thiện độ tin cậy và ngăn chặn hành vi không mong muốn.

### Ví dụ xác thực kết hợp

```scheme
;; Hàm xử lý đầu ra message tới nhiều đích
(define (send-message message output)

  ;; Kiểm tra đối số message và đầu ra
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Gửi tới Bảng điều khiển tin nhắn
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Gửi tới hộp thoại GUI
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Gửi tới cửa sổ terminal
        ((eq? output 'terminal)
           (display message)))))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))
```

Trong phiên bản này:
- Trước tiên, hàm sẽ kiểm tra xem `message` có trống hay không hợp lệ. Nếu thông báo hợp lệ, nó sẽ chuyển sang kiểm tra xem `output` có phải là một trong những giá trị được chấp nhận hay không (`gui`, `error-console` hoặc `terminal`).
- Nếu cả hai bước kiểm tra đều đạt, thông báo sẽ được gửi đến đầu ra thích hợp. Nếu không, một thông báo lỗi sẽ xuất hiện kèm theo lời giải thích rõ ràng.
- Một bước kiểm tra bổ sung được thực hiện để đảm bảo tin nhắn cũng là một chuỗi.

Chức năng xác thực kết hợp này giúp mã sạch hơn và đảm bảo rằng cả hai đầu vào đều được xác thực trước khi thực hiện bất kỳ hành động nào, giúp chức năng này trở nên mạnh mẽ hơn. Lưu ý rằng chúng ta cũng đang xây dựng một hệ thống nhắn tin gỡ lỗi. Khi
mã không thành công, chúng ta có lý do, lý do chúng ta tự viết.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```