---
title: "Hàm biến thiên"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 514c81d0058b436609f97d1177e349ad6d7685ad6ccef15afaaa8ef9f137d852
translation_lock: true
url: "hub/scripting/fundamentals/Functions/variadic"
---
**Hàm biến thiên** trong Scheme là các hàm chấp nhận số lượng đối số thay đổi. Các hàm này rất linh hoạt và cho phép bạn tạo mã linh hoạt và có thể tái sử dụng. Trong lập trình hàm, các hàm biến đổi đơn giản hóa các thao tác cần xử lý số lượng đầu vào tùy ý, chẳng hạn như tính tổng một danh sách các số hoặc nối chuỗi.

Hàm biến phân đặc biệt hữu ích khi:

- Số lượng đối số không thể được xác định trước.
- Bạn cần áp dụng thao tác tương tự cho danh sách đầu vào động.
- Viết các tiện ích để tổng hợp hoặc chuyển đổi dữ liệu.

### Cú pháp của hàm biến phân

Các hàm biến thiên được xác định bằng ký hiệu `.` trước tên tham số cuối cùng. Tham số cuối cùng này thu thập tất cả các đối số còn lại vào một danh sách.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Bất kỳ đối số cố định, bắt buộc nào mà hàm chấp nhận.
- **`variadic-parameter`:** Một tham số đặc biệt đứng trước `.` thu thập các đối số bổ sung dưới dạng danh sách.
- **`body-expression`:** Logic được thực thi khi hàm được gọi.

### Ví dụ về hàm biến phân

#### Hàm biến thiên cơ bản

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Giải thích**:
  - `numbers` tập hợp tất cả các đối số vào một danh sách.
  - `apply` áp dụng hàm `+` cho tất cả các thành phần của danh sách.

**Cách sử dụng**:
```scheme
(sum 1 2 3 4 5)  ; Trả về 15
```

#### Hàm biến thiên có tham số cố định

Bạn có thể kết hợp các tham số cố định với tham số biến thiên để tạo ra các hàm linh hoạt hơn.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Giải thích**:
  - `prefix` là đối số cố định.
  - `names` tập hợp các đối số còn lại vào danh sách.
  - Mỗi tên được bắt đầu bằng chuỗi đã cho bằng cách sử dụng `map` và `lambda`.

**Cách sử dụng**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Trả về ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Kết hợp logic cố định và logic biến đổi

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Giải thích**:
  - `collection-name` là tham số cố định.
  - `items` thu thập các đối số bổ sung vào danh sách.
  - Hàm ghép tên bộ sưu tập và các mục thành một chuỗi duy nhất.

**Cách sử dụng**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Trả về "Fruits: Apple, Banana, Cherry"
```

### Các trường hợp sử dụng nâng cao

#### Xử lý đầu vào tùy ý

Các hàm biến phân vượt trội trong việc xử lý dữ liệu tùy ý. Đây là một ví dụ để chỉ tính tổng các số dương:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Lọc các số không dương trước khi tính tổng.

**Cách sử dụng**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Trả về 18
```

#### Hàm biến thiên với logic đệ quy

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Giải thích**:
  - `first` xử lý đối số đầu tiên.
  - `rest` tập hợp các đối số còn lại vào danh sách.
  - Tính đệ quy giá trị lớn nhất.

**Cách sử dụng**:
```scheme
(max-value 10 20 5 40 15)  ; Trả về 40
```

### Lợi ích của hàm biến phân

- **Tính linh hoạt:** Chúng xử lý nhiều trường hợp đầu vào.
- **Sự chính xác:** Giảm nhu cầu sử dụng nhiều chức năng bị quá tải.
- **Hoạt động động:** Cho phép xử lý dữ liệu thời gian chạy mà không cần biết trước số lượng đối số.

### Khi nào nên sử dụng hàm biến phân

Sử dụng hàm biến thiên khi:

- Hàm cần xử lý số lượng đối số không xác định.
- Một thao tác duy nhất áp dụng cho tất cả đầu vào (ví dụ: tính tổng, nối hoặc ánh xạ).
- Đơn giản hóa logic bậc cao với các đối số động.

Tránh các hàm biến đổi khi:

- Việc xác thực đầu vào hoặc kiểm tra kiểu rất phức tạp.
- Đã sửa các đối số đủ logic theo yêu cầu.
- Khả năng đọc bị tổn hại do các hoạt động quá phức tạp.

### Phần kết luận

Các hàm biến đổi trong Scheme cung cấp một cơ chế mạnh mẽ để xử lý các đầu vào động. Bằng cách hiểu cú pháp và cách sử dụng của chúng, bạn có thể tạo các tập lệnh linh hoạt và mạnh mẽ để thích ứng với nhiều tình huống khác nhau. Kết hợp với các hàm bậc cao hơn, các hàm biến đổi làm cho mã của bạn ngắn gọn và biểu cảm hơn.