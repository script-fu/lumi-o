---
title: "Giá trị trả về"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
translation_lock: true
url: "hub/scripting/tutorials/First Step/return-values"
---
Giá trị trả về quan trọng vì chúng cho phép bạn kiểm soát luồng mà không cần trạng thái bổ sung. Trong Scheme, biểu thức được đánh giá cuối cùng sẽ trở thành giá trị trả về.

Trang này sử dụng các trình trợ giúp xác thực từ ví dụ về thông báo để cho thấy các giá trị trả về rõ ràng giúp việc soạn mã dễ dàng hơn như thế nào.

### Giá trị trả về là gì?

Trong Scheme, giá trị trả về của hàm được xác định bởi biểu thức cuối cùng mà hàm đánh giá. Điều này có nghĩa là bất kỳ dòng mã cuối cùng nào trong hàm đánh giá sẽ được trả về dưới dạng kết quả của hàm. Nếu không có giá trị nào được trả về rõ ràng thì hàm sẽ trả về `#f` (false) hoặc `undefined`.

Hãy xem lại hàm xác thực, (is-valid-string?)

```scheme
;; Mục đích: Kiểm tra message là chuỗi không rỗng
(define (is-valid-string? message)
  ;; Kiểm tra message là chuỗi không rỗng
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Trong chức năng này, nếu thông báo không hợp lệ thì sẽ xảy ra lỗi. Tuy nhiên, nếu thông báo hợp lệ thì không có giá trị trả về rõ ràng nào được đưa ra và hàm trả về `#f` theo mặc định.

### Làm cho giá trị trả về rõ ràng

Chúng ta có thể cải thiện điều này bằng cách làm cho giá trị trả về rõ ràng hơn. Ví dụ: chúng ta có thể trả về `#t` (true) nếu tin nhắn hợp lệ:

```scheme
;; Mục đích: Kiểm tra message được gửi tới đầu ra hợp lệ
(define (is-valid-output-display? output)
  ;; Kiểm tra đầu ra là một trong các đích hiển thị mong đợi
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

Trong phiên bản này, hàm sẽ trả về `#t` khi thông báo hợp lệ, mang lại kết quả rõ ràng. Điều này cho phép sử dụng hàm linh hoạt hơn trong các bối cảnh khác cần có kết quả boolean.

### Sử dụng giá trị trả về một cách hiệu quả

Bằng cách quyết định hàm của chúng ta trả về cái gì, chúng ta có thể làm cho chúng dễ dự đoán và hữu ích hơn. Trả về các giá trị như `#t`, `#f` hoặc một kết quả cụ thể giúp chúng ta kiểm soát nhiều hơn cách hàm tương tác với phần còn lại của mã. Ví dụ: bạn có thể sử dụng giá trị trả về để đưa ra các quyết định tiếp theo trong hàm gọi hoặc chuyển nó làm đối số cho hàm khác.

Đây là một ví dụ đơn giản về việc sử dụng giá trị trả về để kiểm soát luồng logic:

```scheme
;; Mục đích: Gửi message tới đích đầu ra phù hợp
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

Trong trường hợp này, (gửi tin nhắn) dựa vào giá trị trả về của (is-valid-output-display?) để quyết định có tiếp tục hay không.
Câu lệnh có điều kiện `cond` sẽ bị bỏ qua nếu lần kiểm tra đầu tiên thất bại. Ngoài ra, hãy chú ý cách nó đọc một cách khá tự nhiên, liệu đầu ra có hiển thị hợp lệ không?

## Logic câu lệnh if trong Scheme

Trước ví dụ về thư viện được tái cấu trúc, đây là phần đánh giá nhanh về logic có điều kiện. Scheme sử dụng `if` để chọn giữa hai đường dẫn.

Đây là một dạng đơn giản của câu lệnh `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Cấu trúc này kiểm tra điều kiện và nếu điều kiện đúng thì nó sẽ thực hiện hành động đầu tiên. Nếu điều kiện sai, nó sẽ thực hiện hành động thứ hai.

Trong trường hợp bạn cần thực hiện nhiều hành động khi điều kiện đúng hoặc sai, bạn có thể sử dụng `begin` để nhóm chúng lại với nhau:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Điều này cho phép bạn xử lý các tình huống phức tạp hơn, trong đó nhiều biểu thức hoặc câu lệnh cần được thực thi tùy thuộc vào kết quả của điều kiện có điều kiện.

Được rồi, đây là mã thư viện có chứa các giá trị trả về được nhúng và sử dụng để kiểm soát quá trình thực thi.

### Được tái cấu trúc với các giá trị trả về

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

Giá trị trả về là một phần cơ bản giúp hàm trở nên linh hoạt và có thể tái sử dụng. Bằng cách quyết định cẩn thận mỗi hàm sẽ trả về cái gì, chúng ta có thể đảm bảo các hàm của chúng ta tương tác tốt với nhau và cung cấp thông tin hữu ích cho phần còn lại của mã. Cho dù đó là trả về `#t` hay `#f` hay thứ gì đó cụ thể hơn, các giá trị trả về sẽ cho chúng ta cách kiểm soát luồng chương trình của mình và xử lý các kết quả khác nhau.