---
title: "Hàm Lambda"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: bbc5db329f2db333e2133fe248611e30afe266325f05b4209ae197517d068186
translation_lock: true
url: "hub/scripting/fundamentals/Functions/lambda"
---
**Hàm Lambda** trong Scheme là các hàm ẩn danh, nghĩa là chúng là các hàm không có tên. Các hàm này được xác định nội tuyến và thường được sử dụng cho các hoạt động ngắn, một lần. Cấu trúc `lambda` là một công cụ mạnh mẽ trong lập trình hàm, cho phép bạn tạo logic ngắn gọn và linh hoạt một cách nhanh chóng.

Hàm Lambda đặc biệt hữu ích khi:

- Bạn cần một chức năng nhỏ cho một mục đích cụ thể, tạm thời.
- Truyền các hàm làm đối số cho các hàm bậc cao hơn như `map`, `filter` hoặc `fold`.
- Trả về hàm từ hàm khác.

### Cú pháp của hàm Lambda

Các hàm Lambda có thể được xác định riêng...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...hoặc được gọi ngay lập tức:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Các tham số mà hàm chấp nhận.
- **`body-expression`:** Logic được thực thi khi hàm được gọi.
- **Gọi ngay lập tức:** Biểu mẫu thứ hai hiển thị một lambda được gọi ngay lập tức kèm theo các đối số.

### Ví dụ về Hàm Lambda

#### Sử dụng Lambda để tính toán đơn giản

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Trả về 8
```

Đây:

- Hàm lambda được tạo để cộng hai số (`x` và `y`).
- Hàm được gọi ngay lập tức với các đối số `3` và `5`.

#### Hàm Lambda nội tuyến

Ví dụ sau đây minh họa cách sử dụng `for-each` với cả hàm được đặt tên và hàm lambda:

**Sử dụng hàm được đặt tên:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Giải thích**:
  - `print-item` là hàm được đặt tên để chuyển đổi một số thành chuỗi (`number->string`) và in nó bằng `lumi-message`.
  - `for-each` áp dụng `print-item` cho từng thành phần trong danh sách `(1 2 3 4)`.

**Đầu ra**: 1 2 3 4

**Sử dụng Hàm Lambda:**

Logic tương tự có thể được viết nội tuyến bằng hàm lambda, tránh sự cần thiết của một hàm được đặt tên riêng:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Giải thích**:
  - `(lambda (x) (lumi-message (number->string x)))` xác định hàm ẩn danh.
  - Hàm này được áp dụng cho từng phần tử của danh sách `(1 2 3 4)` bởi `for-each`.

**Đầu ra**: 1 2 3 4

#### Lambda hoạt động như đối số

Các hàm Lambda thường được chuyển trực tiếp đến các hàm bậc cao hơn như `map` hoặc `filter`.

#### Bình phương một danh sách các số

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Trả về (1 4 9 16)
```

- Hàm `lambda` bình phương từng phần tử của danh sách.
- Hàm `map` áp dụng `lambda` cho từng phần tử.

#### Lambda hoạt động như giá trị trả về

Bạn có thể trả về hàm lambda từ một hàm khác để tạo hành vi động.

#### Tạo hàm cộng

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Trả về 15
```

- `make-adder` tạo hàm lambda mới để thêm một số cụ thể (`n`).
- Lambda trả về được lưu trữ trong `add5`, bổ sung thêm `5` vào đầu vào của nó.

#### Sử dụng Lambda với `let`

Lambda thường được sử dụng với `let` để tạo các hàm tạm thời, có phạm vi cục bộ.

#### Lambda cục bộ để bổ sung

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Trả về 7
```

- `let` liên kết hàm lambda với tên `add`.
- Sau đó, lambda được sử dụng như một hàm bình thường trong phạm vi `let`.

#### Kết hợp Lambda với các hàm bậc cao hơn

Lambda tỏa sáng khi được kết hợp với các hàm bậc cao hơn để thực hiện các phép biến đổi dữ liệu phức tạp.

#### Lọc số chẵn

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Trả về (2 4 6)
```- `lambda` kiểm tra xem một số có phải là số chẵn hay không.
- Hàm `filter` sử dụng lambda để chỉ giữ lại các số chẵn trong danh sách.

### Lợi ích của Hàm Lambda

- **Sự chính xác:** Lambdas giảm mã soạn sẵn bằng cách loại bỏ nhu cầu xác định các hàm được đặt tên riêng biệt.
- **Tính linh hoạt:** Bạn có thể xác định và sử dụng chúng ở bất cứ nơi nào cần thiết, làm cho mã trở nên mô-đun hơn.
- **Cải thiện khả năng đọc:** Đối với các tác vụ ngắn, cụ thể, lambda làm cho ý định rõ ràng mà không làm lộn xộn mã với các hàm được đặt tên bổ sung.

### Khi nào nên sử dụng Hàm Lambda

Sử dụng hàm lambda khi:

- Logic ngắn gọn, khép kín.
- Chức năng chỉ cần thiết tạm thời hoặc trong một phạm vi cụ thể.
- Bạn đang làm việc với các hàm bậc cao hơn như `map`, `filter` hoặc `reduce`.

Tránh sử dụng lambda cho logic phức tạp, nhiều dòng vì điều này có thể làm giảm khả năng đọc. Để có các thao tác mở rộng hơn, hãy sử dụng hàm được đặt tên thay thế.

### Kết luận

Các hàm Lambda trong Scheme cung cấp một cách ngắn gọn và mạnh mẽ để xác định các hàm ẩn danh cho các tác vụ cụ thể. Tính linh hoạt và dễ sử dụng khiến chúng trở thành công cụ thiết yếu cho bất kỳ lập trình viên Scheme nào. Hiểu cách sử dụng `lambda` một cách hiệu quả sẽ giúp bạn viết các tập lệnh sạch hơn, mô-đun hơn và hiệu quả hơn.