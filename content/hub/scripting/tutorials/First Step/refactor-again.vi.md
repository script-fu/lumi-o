---
title: "Tái cấu trúc lần nữa"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactor-again"
---
Khi thư viện trợ giúp phát triển, việc theo dõi trong nháy mắt sẽ trở nên khó khăn hơn. Tái cấu trúc một lần nữa để giữ cho mỗi hàm nhỏ và có một mục đích duy nhất.

### Phá vỡ sự phức tạp

Để làm cho chức năng này dễ theo dõi và bảo trì hơn, hãy chia nó thành các chức năng nhỏ hơn và tập trung hơn. Bắt đầu bằng cách tách xác thực khỏi định tuyến tin nhắn.

### Tạo hàm xác thực

Chúng ta có thể lấy phần của hàm xác thực các đối số `message` và `output` và chuyển nó thành một hàm riêng biệt. Bằng cách này, hàm `send-message` cốt lõi không cần phải lo lắng về việc xác thực, giúp việc theo dõi trở nên dễ dàng hơn.

```scheme
(define (validate-message message output)
  ;; Kiểm tra message là chuỗi không rỗng
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Kiểm tra đầu ra là một trong các đích mong đợi
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Đơn giản hóa việc gửi tin nhắn

Giờ đây, quá trình xác thực đã được chuyển sang một hàm riêng biệt, hàm `send-message` có thể chỉ tập trung vào việc gửi tin nhắn. Nó sẽ đơn giản hơn nhiều vì nó chỉ xử lý nhiệm vụ cụ thể là hướng tin nhắn đến đúng đích.

```scheme
(define (send-message message output)
  ;; Gọi hàm kiểm tra trước khi tiếp tục
  (validate-message message output)

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
       (display message)))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))
```

### Chia nhỏ hơn nữa: Tách từng bộ xử lý đầu ra

Mỗi loại đầu ra tin nhắn (GUI, bảng điều khiển thông báo, Terminal) có thể được chuyển sang chức năng riêng. Điều này cho phép thử nghiệm, sửa đổi và mở rộng tiềm năng dễ dàng hơn trong tương lai.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Gửi tới đầu ra phù hợp
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))
```

### Tái sử dụng xác thực trong mỗi chức năng gửi

Vì xác thực là một phần quan trọng để đảm bảo rằng cả thông báo và đầu ra đều chính xác, nên mỗi hàm `send-*` sẽ thực hiện xác thực riêng của nó. Điều này đảm bảo rằng bất kể đầu ra nào được gọi, chúng ta luôn kiểm tra đầu vào trước.

```scheme
(define (send-to-gui message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'terminal)
  (display message))
```

Hãy thấy rằng chúng ta đã xóa xác thực khỏi chức năng gửi tin nhắn và chuyển trách nhiệm sang từng chức năng đầu ra riêng lẻ. Thay đổi này đảm bảo rằng mỗi đích đến (GUI, Bảng điều khiển tin nhắn, Terminal) xử lý quá trình xác thực riêng, hợp lý hóa chức năng gửi tin nhắn và giữ logic xác thực gần hơn với nơi cần thiết.

Cách tiếp cận này có thể đơn giản hóa chức năng gửi tin nhắn, biến nó thành _dispatcher_, đồng thời đảm bảo rằng mỗi chức năng gửi đến* sẽ xác thực chính xác tin nhắn trước khi xử lý.

Bằng cách di chuyển quá trình xác thực vào từng hàm send-to-*, chúng ta đã làm cho chúng có thể tái sử dụng được dưới dạng các hàm độc lập. Điều này có nghĩa là chúng ta có thể gọi trực tiếp bất kỳ hàm send-to-gui, send-to-error-console hoặc send-to-terminal nào mà không cần dựa vào hàm gửi tin nhắn gửi. Mỗi hàm này hiện xử lý hoàn toàn logic riêng và có thể được sử dụng độc lập trong các phần khác của mã hoặc trong các plug-in khác, giúp mã của bạn trở nên mô-đun và linh hoạt hơn.

## Lợi ích của việc tái cấu trúc

- **Tách biệt rõ ràng các mối quan tâm**: Mỗi chức năng hiện chỉ xử lý một trách nhiệm, giúp mã dễ hiểu hơn.
- **Khả năng mở rộng**: Việc thêm các loại đầu ra mới rất đơn giản. Bạn chỉ cần xác định một hàm mới như `send-to-file` hoặc `send-to-logger`, sau đó thêm trường hợp trong câu lệnh `cond`.
- **Khả năng sử dụng lại**: Mỗi chức năng xử lý đầu ra này có thể được sử dụng lại ở nơi khác trong dự án của bạn hoặc được chia sẻ giữa nhiều plugin.
- **Tính nhất quán**: Bằng cách sử dụng lại hàm xác thực trong mỗi hàm `send-to-*`, bạn đảm bảo rằng tất cả kết quả đầu ra đều được xác thực chính xác, giúp mã trở nên mạnh mẽ hơn.

Phiên bản thư viện được tái cấu trúc:

```scheme
;; Mục đích: Gửi message tới hộp thoại GUI
(define (send-to-gui message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Mục đích: Gửi message tới Bảng điều khiển tin nhắn
(define (send-to-error-console message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Mục đích: Gửi message tới cửa sổ terminal
(define (send-to-terminal message)
  ;; Kiểm tra message trước khi tiếp tục
  (validate-message message 'terminal)
  (display message))

;; Mục đích: Gửi message tới đích đầu ra phù hợp
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển tin nhắn
  (lumi-message-set-handler 2))

;; Mục đích: Kiểm tra message là chuỗi không rỗng và đầu ra hợp lệ
(define (validate-message message output)
  ;; Kiểm tra message là chuỗi không rỗng
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Kiểm tra đầu ra là một trong các đích mong đợi
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Đó là tất cả những gì chúng ta có thể làm à? KHÔNG! còn nhiều việc phải làm, vui lòng đọc tiếp.