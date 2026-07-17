---
title: "bản đồ"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/map"
---
Hàm `map` trong Scheme được sử dụng để áp dụng quy trình cho từng thành phần của danh sách (hoặc nhiều danh sách) và **trả về một danh sách mới** chứa kết quả. Điều này làm cho nó trở nên lý tưởng cho việc chuyển đổi dữ liệu.

Dạng đơn giản nhất của `map` trông như thế này:

```scheme
(map procedure list)
```

- **Thủ tục**: Hàm áp dụng cho từng thành phần của danh sách.
- **Danh sách**: Danh sách có các phần tử sẽ được chuyển đổi.

---

### Ví dụ: Nhân đôi mỗi phần tử

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Ở đây, hàm `double` được áp dụng cho từng phần tử của danh sách `(1 2 3 4)`.
- Kết quả là một danh sách mới với mỗi phần tử được nhân đôi.

**Đầu ra**: `(2 4 6 8)`

---

### Cách thức hoạt động

1. **Tạo danh sách mới**:
   - `map` áp dụng quy trình được cung cấp cho từng thành phần của danh sách và thu thập kết quả vào danh sách mới.

2. **Chuyển đổi dữ liệu**:
   - Nó chủ yếu được sử dụng để chuyển đổi dữ liệu hơn là thực hiện các tác dụng phụ.

---

#### Ví dụ: Sử dụng với nhiều danh sách

Nếu nhiều danh sách được cung cấp, `map` sẽ xử lý các phần tử tương ứng từ mỗi danh sách.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Hàm `sum` thêm các phần tử tương ứng từ 2 danh sách và trả về kết quả là danh sách mới.

**Đầu ra**: `(5 7 9)`

---

### Tóm tắt

- Hàm `map` là một công cụ mạnh mẽ để chuyển đổi danh sách bằng cách áp dụng một quy trình cho từng phần tử.
- Không giống như `for-each`, `map` **tạo ra một danh sách mới** chứa kết quả của việc áp dụng quy trình.
- Nó hỗ trợ nhiều danh sách, cho phép các hoạt động theo từng phần tử trên chúng.

Bằng cách sử dụng `map`, bạn có thể tạo các phiên bản đã chuyển đổi của dữ liệu một cách hiệu quả trong khi vẫn giữ nguyên danh sách ban đầu.