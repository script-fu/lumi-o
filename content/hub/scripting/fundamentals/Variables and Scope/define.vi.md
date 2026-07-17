---
title: "định nghĩa"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: 1a62a7c4204b91c6988f897e400b577ef45a01ca57b06789e5f72591c5196144
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/define"
---
Câu lệnh `define` trong Scheme là một cấu trúc linh hoạt được sử dụng để tạo các liên kết toàn cục hoặc cục bộ. Nó được sử dụng phổ biến nhất để xác định các biến và hàm, giúp chúng có thể tái sử dụng và truy cập được trong toàn bộ tập lệnh hoặc trong một phạm vi cụ thể. Hiểu `define` là rất quan trọng để viết các chương trình Scheme mô-đun, có thể tái sử dụng và có thể đọc được.

### Mục đích của `define`

Cấu trúc `define` phục vụ nhiều mục đích:
- **Xác định biến**: Gán giá trị cho tên biến, giúp chúng có thể sử dụng sau này.
- **Xác định hàm**: Tạo các quy trình có thể tái sử dụng gói gọn logic cụ thể.
- **Định nghĩa cục bộ**: Khi được sử dụng trong một hàm, `define` tạo ra các liên kết cục bộ không ảnh hưởng đến không gian tên chung.

---

### Xác định biến bằng `define`

Cách sử dụng cơ bản của `define` là tạo các biến chứa giá trị không đổi hoặc được tính toán.

#### Cú pháp

```scheme
(define variable-name value)
```

#### Ví dụ: Định nghĩa một hằng số

```scheme
(define pi 3.14159)
(* pi 2) ;; Tính 2π
```

**Kết quả**: `6.28318`

---

### Xác định hàm với `define`

Bạn có thể sử dụng `define` để tạo các quy trình có thể sử dụng lại.

#### Cú pháp

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Ví dụ: Định nghĩa một hàm đơn giản

```scheme
(define (square x)
  (* x x))
(square 4) ;; Tính 4²
```

**Kết quả**: `16`

---

### Định nghĩa cục bộ với `define`

Khi được sử dụng bên trong một hàm, `define` tạo ra các liên kết cục bộ chỉ có thể truy cập được trong hàm kèm theo. Điều này tránh làm ô nhiễm không gian tên chung và giúp tổ chức mã của bạn.

#### Ví dụ: Hàm trợ giúp cục bộ

```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Hàm trợ giúp cục bộ
  (define (cube x) (* x x x))  ;; Hàm trợ giúp cục bộ
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Kết quả**: `41` (Tính \(2^2 + 3^3 + 4^2\))

---

### Tính năng chính của `define`

1. **Phạm vi toàn cầu hoặc địa phương**:
   - Khi được sử dụng ở cấp cao nhất, `define` tạo ra các biến hoặc hàm toàn cục.
   - Khi được sử dụng bên trong một hàm khác, `define` sẽ tạo ra các liên kết cục bộ.

2. **Khả năng tái sử dụng**:
   - Các hàm được xác định bằng `define` có thể được sử dụng lại nhiều lần trong các ngữ cảnh khác nhau.

3. **Khả năng đọc được cải thiện**:
   - Việc chia logic thành các hàm nhỏ hơn, được đặt tên rõ ràng sẽ cải thiện độ rõ ràng và khả năng bảo trì mã của bạn.

---

### Sự khác biệt giữa `define` và `let`

| **Khía cạnh** | **`define`** | **`let`** |
|--------------------------|--------------------------------------------------|------------------------------------------------|
| **Mục đích** | Tạo các liên kết toàn cục hoặc cục bộ cho các biến hoặc hàm. | Tạo các ràng buộc tạm thời trong phạm vi bản địa hóa. |
| **Phạm vi** | Toàn cầu khi ở cấp cao nhất; local khi ở trong một hàm khác. | Luôn cục bộ trong khối `let`.       |
| **Khả năng tái sử dụng** | Các hàm và biến có thể được sử dụng lại ở nhiều nơi. | Các biến được ràng buộc tạm thời cho một khối duy nhất. |
| **Cú pháp** | Xác định rõ ràng các biến hoặc hàm.       | Kết hợp liên kết biến với đánh giá biểu thức. |