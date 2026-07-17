---
title: "cho phép"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 6e768f3feb8a1873423841338e92494ebd2b4ac0af5b6e27253f3cf2c2ba455f
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/let"
---
Tên `let` được sử dụng vì nó phản ánh nguồn gốc toán học của việc đưa ra các ràng buộc tạm thời, như trong _"Let \( x = 2 \) và \( y = 3 \)"_.

Câu lệnh `let` trong Scheme là **cấu trúc liên kết** được sử dụng để xác định các biến trong phạm vi cục bộ. Nó cho phép bạn tạo các liên kết tạm thời cho các biến và sau đó thực thi một khối mã bằng cách sử dụng các liên kết đó. Điều này đặc biệt hữu ích để giữ mã theo mô-đun và tránh ô nhiễm biến đổi toàn cầu.

Có ba dạng chính của `let` trong Scheme:

- **`let`**: Let chuẩn để tạo các liên kết cục bộ đơn giản.
- **`let*`**: Cho phép tuần tự, trong đó các ràng buộc có thể phụ thuộc vào kết quả của các ràng buộc trước đó.
- **Được đặt tên `let`**: Một dạng đặc biệt của `let` tạo ra các vòng lặp đệ quy hoặc các thủ tục được đặt tên.

Ở dạng đơn giản nhất, `let` tạo các liên kết biến cục bộ và đánh giá một biểu thức với các liên kết đó.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Ràng buộc**: Danh sách các cặp trong đó mỗi cặp gán `value` cho `variable`.
- **Biểu thức**: Phần thân của `let`, có thể sử dụng các biến được xác định cục bộ.

### Ví dụ

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Điều này xác định hai biến cục bộ, `x` (10) và `y` (20).
- Sau đó nó tính toán `(+ x y)` bằng cách sử dụng các biến này.

**Kết quả**: `30`

---

## Cấu trúc `let*`

Cấu trúc `let*` tương tự như `let`, nhưng các liên kết được đánh giá **tuần tự**. Điều này có nghĩa là các ràng buộc sau này có thể phụ thuộc vào các ràng buộc trước đó.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Ví dụ

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- Liên kết đầu tiên gán `10` cho `x`.
- Liên kết thứ hai tính toán `y` là `(+ x 5)`, sử dụng giá trị của `x`.
- Phần thân tính toán `(* x y)`.

**Kết quả**: `150`

---

## Được đặt tên `let`

Tên `let` là một dạng đặc biệt của `let` cung cấp tên cho chính khối `let`, biến nó thành một thủ tục đệ quy. Điều này rất hữu ích cho việc tạo vòng lặp hoặc tính toán đệ quy.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Tên**: Khối `let` được đặt tên, xác định một hàm một cách hiệu quả.
- **Ràng buộc**: Giá trị ban đầu cho các biến, tương tự như `let` tiêu chuẩn.
- **Body**: Biểu thức có thể gọi đệ quy `let` có tên.

### Ví dụ: Vòng lặp có tên `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- Hàm `loop` bắt đầu bằng `n = 5` và `result = 1`.
- Nếu `n` là `0` thì trả về `result`.
- Ngược lại, nó sẽ tự gọi đệ quy với `n - 1` và `result * n`.

**Kết quả**: `120` (Giai thừa của 5)

---

## Bảng tóm tắt

| Xây dựng | Mô tả | Trường hợp sử dụng |
|-------------|---------------------------------------------------|--------------------------------------------------------------------------|
| **`let`** | Xác định các ràng buộc cục bộ cho các biến.    | Sử dụng khi tất cả các liên kết đều độc lập và không phụ thuộc lẫn nhau.     |
| **`let*`** | Xác định các ràng buộc cục bộ tuần tự.       | Sử dụng khi các ràng buộc sau này phụ thuộc vào kết quả của các ràng buộc trước đó.           |
| **Được đặt tên `let`** | Xác định các thủ tục cục bộ đệ quy. | Sử dụng vòng lặp for, tính toán lặp hoặc đệ quy trong ngữ cảnh cục bộ. |

---

## Ví dụ

### Sử dụng `let` để tính toán cục bộ

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Kết quả**: `13` (Tính `x² + y²`)

---

### Sử dụng `let*` cho các phần phụ thuộc tuần tự

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Kết quả**: `8` (Tính `x³`)

---

### Sử dụng Named `let` để tính toán đệ quy

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Kết quả**: `120` (Giai thừa của 5)

---

Bằng cách sử dụng `let`, `let*` và được đặt tên là `let`, Scheme cho phép lập trình mô-đun, đệ quy và tuần tự với các quy tắc phạm vi rõ ràng.