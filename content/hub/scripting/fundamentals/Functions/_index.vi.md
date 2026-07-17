---
title: "Hàm"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a1808e88698d7f38626bf136806af5388132ed2799927b899141c749dac679a3
translation_lock: true
url: "hub/scripting/fundamentals/Functions/_index"
---
Hàm là khái niệm cốt lõi trong Scheme, cung cấp phương tiện để đóng gói logic, cho phép tái sử dụng mã và cấu trúc tập lệnh của bạn một cách hiệu quả. Với các chức năng, bạn có thể tạo các tập lệnh mô-đun, có thể bảo trì để xử lý nhiều tác vụ khác nhau, từ các thao tác cơ bản đến quy trình công việc nâng cao trong Lumi.

Phần này phục vụ như phần giới thiệu về các hàm trong Scheme và đặt nền tảng để hiểu các loại, định nghĩa và cách sử dụng của chúng. Các phần tiếp theo sẽ đi sâu hơn vào các loại chức năng cụ thể và khả năng độc đáo của chúng.

## Cú pháp và biểu thức tối thiểu

Mã Scheme được tạo từ **biểu thức**. Một biểu thức đánh giá một giá trị. Cú pháp thống nhất: dấu ngoặc đơn tạo thành lệnh gọi, trước tiên là tên toán tử hoặc hàm.

```scheme
(+ 1 2)         ; Cộng 1 và 2, kết quả 3
(if #t 1 0)     ; Cho kết quả 1 vì điều kiện đúng
(list 1 2 3)    ; Tạo danh sách: (1 2 3)
```

Bởi vì mọi thứ đều là một biểu thức nên luồng điều khiển tự nhiên có cùng kiểu với các lệnh gọi hàm.

## Tại sao hàm lại quan trọng

Các hàm đóng vai trò then chốt trong Scheme vì một số lý do:

- **Khả năng sử dụng lại mã:** Tránh lặp lại bằng cách đóng gói logic vào các thành phần có thể tái sử dụng.
- **Tính mô-đun:** Chia các nhiệm vụ phức tạp thành các phần nhỏ hơn, dễ quản lý hơn.
- **Hành vi động:** Chấp nhận các tham số để xử lý các đầu vào khác nhau hoặc thích ứng với các tình huống khác nhau.
- **Tính trừu tượng cao hơn:** Đơn giản hóa logic bằng cách tập trung vào "cái gì" mà hàm thực hiện thay vì "nó thực hiện" như thế nào.

## Tổng quan về các loại hàm

Scheme cung cấp nhiều cấu trúc chức năng khác nhau, mỗi cấu trúc phù hợp với các trường hợp sử dụng cụ thể:

1. **Hàm được đặt tên**
   Đây là các hàm tiêu chuẩn được xác định bằng `define`. Chúng tạo thành xương sống của hầu hết các tập lệnh.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Hàm ẩn danh**
   Còn được gọi là **hàm lambda**, đây là những hàm chưa được đặt tên được xác định nội tuyến để sử dụng một lần.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Hàm bậc cao hơn**
   Các hàm lấy các hàm khác làm đối số hoặc trả về các hàm làm kết quả, cho phép thực hiện các phép trừu tượng hóa mạnh mẽ như ánh xạ, lọc và thu gọn.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Trả về (1 4 9 16)
   ```

## Cú pháp chung cho hàm

Các hàm trong Scheme có cú pháp đơn giản và nhất quán:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Tên hàm.
- **`parameter1, parameter2, ...`:** Các đối số mà hàm lấy.
- **`body-expression`:** Logic được thực thi khi hàm được gọi.

Ví dụ:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Trả về 8
```

## Tác dụng phụ và trạng thái toàn cầu

Trong Lumi, nhiều quy trình hữu ích có **tác dụng phụ**: chúng sửa đổi hình ảnh, thay đổi drawable, ghi tệp hoặc hiển thị đầu ra.

- Cô lập các tác dụng phụ trong các quy trình nhỏ, có tên rõ ràng.
- Tránh thay đổi bối cảnh toàn cầu trừ khi bạn cần.
- Khi bạn thay đổi ngữ cảnh (màu sắc, cọ vẽ, v.v.), hãy bao bọc tác phẩm bằng `lumi-context-push` và `lumi-context-pop` để trạng thái của người dùng được khôi phục.