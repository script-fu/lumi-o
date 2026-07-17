---
title: "Biến và phạm vi"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: 82a033dab5a3f8e3bacc73cde3d2f965fda6cd1b8957877e29da8cfcb547abdd
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/_index"
---
Trong Scheme, quản lý các biến và phạm vi của chúng là khái niệm cốt lõi để viết các tập lệnh hiệu quả và có thể bảo trì. Các biến lưu trữ các giá trị dữ liệu mà tập lệnh của bạn có thể thao tác, trong khi phạm vi xác định nơi các biến đó có thể truy cập được. Hiểu cách xác định và sử dụng các biến một cách hiệu quả cho phép bạn tạo mã có cấu trúc, có thể tái sử dụng và không có lỗi.

### Gõ động

Scheme được nhập động: bạn không khai báo trước các loại và một biến có thể chứa các giá trị thuộc các loại khác nhau theo thời gian.

```scheme
(define x 42)       ; x là số
(set! x "hello")    ; bây giờ x là chuỗi
```

### Vai trò của Định nghĩa và Phạm vi Biến trong Scheme

Việc xác định các biến và quản lý phạm vi của chúng phục vụ một số mục đích:
- **Sắp xếp dữ liệu:** Biến lưu trữ thông tin, giúp tập lệnh của bạn dễ đọc và dễ quản lý hơn.
- **Cải thiện khả năng sử dụng lại:** Bằng cách sử dụng các biến có phạm vi, bạn có thể sử dụng lại các phần mã mà không bị xung đột.
- **Đóng gói:** Phạm vi được bản địa hóa ngăn chặn các tương tác ngoài ý muốn giữa các biến trong các phần khác nhau của tập lệnh của bạn.
- **Đơn giản hóa logic:** Các biến tạm thời trong phạm vi giới hạn giúp giảm độ phức tạp trong các phép tính hoặc quy trình công việc lớn hơn.

### Các loại định nghĩa và phạm vi biến

Scheme cung cấp một số cấu trúc để xác định và xác định phạm vi các biến:
- **`let`:** Tạo liên kết cục bộ cho các biến trong một khối mã cụ thể.
- **`let*`:** Phiên bản tuần tự của `let` trong đó mỗi liên kết có thể phụ thuộc vào liên kết trước đó.
- **Được đặt tên `let`:** Một cấu trúc mạnh mẽ để xác định các vòng lặp hoặc thủ tục cục bộ đệ quy.
- **`define`:** Tạo các biến hoặc hàm toàn cục có thể truy cập được trong toàn bộ tập lệnh của bạn.

### Định nghĩa và phạm vi biến hoạt động như thế nào

Các định nghĩa và phạm vi biến thường liên quan đến:
1. **Khai báo biến:** Gán giá trị cho biến trong ngữ cảnh cụ thể.
2. **Phạm vi giới hạn:** Kiểm soát nơi có thể truy cập biến (ví dụ: trong khối `let` hoặc trên toàn cầu).
3. **Sử dụng Biến:** Truy cập và sửa đổi các giá trị biến để thực hiện các phép tính, logic hoặc thủ tục.

### Ví dụ: Sử dụng `let` cho Biến cục bộ

Cấu trúc `let` cho phép bạn xác định các biến tạm thời chỉ khả dụng trong một khối cụ thể:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Ví dụ này khai báo `x` và `y` bằng các giá trị cục bộ và tính tổng của chúng.

### Ví dụ: Sử dụng `define` cho Biến toàn cục

Cấu trúc `define` tạo các biến hoặc hàm có phạm vi toàn cục:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Tập lệnh này xác định hằng số toàn cục `pi` và hàm `circle-area` sử dụng hằng số đó.

### So sánh phạm vi: Địa phương và toàn cầu

| Tính năng | Phạm vi địa phương (`let`, `let*`) | Phạm vi toàn cầu (`define`) |
|-------------------|---------------------------------------------------|-------------------------------------------------------|
| **Khả năng tiếp cận** | Giới hạn ở khối được xác định | Có thể truy cập trong toàn bộ tập lệnh |
| **Đóng gói** | Ngăn chặn các tương tác ngoài ý muốn | Có thể xung đột với các biến được xác định toàn cục khác |
| **Trường hợp sử dụng** | Biến tạm thời cho các nhiệm vụ cụ thể | Các biến hoặc hàm được chia sẻ được sử dụng xuyên suốt |

### Bản tóm tắt

- **Định nghĩa và phạm vi biến** là nền tảng để tổ chức và quản lý dữ liệu trong tập lệnh Scheme của bạn.

- Sử dụng **phạm vi cục bộ** (`let`, `let*`, có tên `let`) để đóng gói các biến tạm thời và tránh xung đột.
- Sử dụng **phạm vi toàn cầu** (`define`) cho các hàm hoặc hằng số có thể sử dụng lại được chia sẻ trên tập lệnh của bạn.
- Sự hiểu biết rõ ràng về các cấu trúc này sẽ cải thiện khả năng đọc, khả năng bảo trì và độ tin cậy của mã của bạn.