---
title: "Tái cấu trúc"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 730a20920b8e93d463bfb01f5d729e5ea84a548cc4b846e6e888ee751d095cf1
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactoring"
---
Sau khi một hàm hoạt động, chúng ta có thể lùi lại một bước và suy nghĩ về cách tốt nhất để cấu trúc mã của mình. Mục tiêu là làm cho plug-in của chúng ta rõ ràng, dễ hiểu và dễ bảo trì nhất có thể. Quá trình cải thiện và tinh chỉnh cấu trúc mã hiện có mà không thay đổi hành vi của nó được gọi là tái cấu trúc.

Đây là chức năng ban đầu một lần nữa:

```scheme
(define (scheme-hello-world)
  ;; Đặt trình xử lý message để xuất ra hộp thoại GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Đặt trình xử lý message để xuất ra Bảng điều khiển lỗi
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Gửi message tới terminal, cửa sổ OS đã khởi chạy Lumi
  (display "Hello world!\n"))
```

Tên hàm là tên của hàm và tham số là những gì hàm chấp nhận làm đầu vào. Phần thân là khối mã chạy khi hàm được gọi.

Hình thức trừu tượng:

```scheme
(define (function-name parameter)
  body)
```

### Lặp lại mã

Loại bỏ sự lặp lại sớm. `(lumi-message "Hello world!\n")` được lặp lại hai lần và chuỗi thông báo được lặp lại ba lần. Một biến giải quyết chuỗi lặp lại.

### Biến

Trong Scheme, một biến có một "phạm vi", trong đó biến đó được biết đến và phạm vi đó được đặt bằng cách sử dụng câu lệnh `let`. Biến được liên kết với một giá trị trong phần liên kết và biến có phạm vi trong phần thân let. Biến chỉ được biết bên trong khối let và không thể truy cập được bên ngoài nó.

```scheme
(let ((variable value))
  body)
```

Giới thiệu một biến gọi là "tin nhắn":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Đặt trình xử lý message để xuất ra hộp thoại GUI
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Đặt trình xử lý message để xuất ra Bảng điều khiển lỗi
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Gửi message tới terminal, cửa sổ OS đã khởi chạy Lumi
    (display message)))
```

Trong ví dụ của chúng ta, chúng ta đã sử dụng một biến có tên là "message" được liên kết với một chuỗi "Xin chào thế giới!\n". Điều này cho phép chúng ta thay đổi nội dung tin nhắn một lần thay vì ba lần, giảm khả năng xảy ra lỗi và làm cho mã linh hoạt hơn.

### Hàm trích xuất

Trong lập trình hàm, việc tái cấu trúc mã để trích xuất logic có thể tái sử dụng thành các hàm riêng biệt là một cách làm phổ biến. Bằng cách này, **hàm chính** trở nên đơn giản hơn nhiều và tập trung hơn vào mục tiêu cấp cao của nó, trong khi **hàm được trích xuất** có vẻ phức tạp hơn vì nó xử lý logic chi tiết. Điều này là có chủ ý và phù hợp với các nguyên tắc cốt lõi của lập trình chức năng, như tính mô đun, phân tách các mối quan tâm và khả năng đọc. Đây là bản tái cấu trúc
Xin chào thế giới! sau khi khai thác.

Trích xuất logic:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; Hàm chính
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Hàm xử lý đầu ra message tới nhiều đích
(define (send-message message output)
  (cond
    ;; Gửi tới Bảng điều khiển lỗi
    ((eq? output 'error-console)
       ;; Đặt trình xử lý cho Bảng điều khiển lỗi
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

  ;; Khôi phục trình xử lý message mặc định cho Bảng điều khiển lỗi
  (lumi-message-set-handler 2))

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

#### Ký hiệu

Trong ví dụ trên, loại dữ liệu được gọi là ký hiệu được sử dụng, chẳng hạn như 'gui. Các ký hiệu được truyền dưới dạng tham số cho hàm gửi tin nhắn và có thể được sử dụng để đưa ra các quyết định có điều kiện đơn giản. Giống như các khóa tượng trưng, chúng là những mã định danh duy nhất. Để biết thêm thông tin về các biểu tượng, hãy truy cập [trang này.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Đơn giản hóa chức năng chính

Trong hàm ban đầu (lược đồ-hello-world), tất cả logic gửi tin nhắn đến các đầu ra khác nhau (GUI, Bảng điều khiển lỗi, Terminal) đã được trộn vào hàm chính. Sau khi tái cấu trúc, chức năng chính chỉ tập trung vào **những việc cần làm**, gửi thông báo đến các đích khác nhau.

Chức năng chính được tái cấu trúc đơn giản hơn:

- Nó nêu rõ mục đích: gửi cùng một thông điệp tới nhiều đầu ra.
- Nó tránh làm lộn xộn logic chính với mã lặp đi lặp lại như cài đặt trình xử lý thông báo cho các đầu ra khác nhau.
- Nhìn thoáng qua sẽ dễ đọc và dễ hiểu hơn.

### Độ phức tạp của hàm được trích xuất

Ngược lại, hàm **(gửi tin nhắn)** là nơi chứa logic chi tiết. Bây giờ nó xử lý các biến thể trong hành vi cho từng đầu ra (GUI, Bảng điều khiển lỗi, Terminal). Chức năng này phức tạp hơn một chút so với trước đây nhưng giờ đây nó được **tập trung** và **tách biệt**.

## Liên hệ điều này với lập trình chức năng

Trong lập trình hàm, các hàm được coi là **công dân hạng nhất**, nghĩa là chúng có thể được tái sử dụng, truyền đi và kết hợp để tạo thành hành vi phức tạp hơn. Mục tiêu là:- **Phân tách vấn đề** thành các phần nhỏ hơn, độc lập.
- **Tách biệt độ phức tạp** thành các hàm nhỏ hơn để xử lý các tác vụ cụ thể, như `send-message`.
- **Giữ cho các hàm cấp cao trở nên đơn giản** để chúng có thể tập trung vào việc điều phối luồng dữ liệu và hành động mà không cần biết chi tiết về cách hoàn thành từng nhiệm vụ.
- **Tách các mối quan tâm**: Hàm này sẽ xử lý cách gửi tin nhắn dựa trên loại đầu ra, giúp tách logic này khỏi hàm chính.
- **Tính mô-đun**: Bằng cách xử lý tất cả logic gửi tin nhắn ở một nơi, chúng ta có thể dễ dàng thực hiện các thay đổi (chẳng hạn như thêm tùy chọn đầu ra mới) mà không làm thay đổi chức năng chính.
- **Khả năng sử dụng lại**: Hàm `send-message` có thể tái sử dụng, nghĩa là nếu cần gửi thông báo đến nhiều đầu ra ở nơi khác trong mã của mình, chúng ta có thể chỉ cần gọi hàm này thay vì viết lại logic tương tự.

Bằng cách tái cấu trúc, hàm chính trong ví dụ này trở thành một câu lệnh **khai báo** về những gì đang xảy ra ("gửi tin nhắn đến ba nơi"), trong khi sự phức tạp của cách gửi những tin nhắn đó được tóm tắt thành hàm `send-message`.