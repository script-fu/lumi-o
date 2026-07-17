---
title: "cho mỗi"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/for-each"
---
Hàm `for-each` trong Scheme được sử dụng để áp dụng một quy trình cho từng thành phần của danh sách (hoặc nhiều danh sách). Không giống như `map`, trả về một danh sách mới kèm theo kết quả, `for-each` được sử dụng cho **tác dụng phụ** của nó, chẳng hạn như in hoặc cập nhật các biến.

Dạng đơn giản nhất của `for-each` trông như thế này:

```scheme
(for-each procedure list)
```

- **Thủ tục**: Hàm áp dụng cho từng thành phần của danh sách.
- **Danh sách**: Danh sách có các phần tử sẽ được xử lý.

---

### Ví dụ: In danh sách

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Ở đây, hàm `print-item` được áp dụng cho từng phần tử của danh sách `(1 2 3 4)`.
- Điều này làm cho mỗi số được in tuần tự.

**Đầu ra**: `1 2 3 4`

---

### Cách thức hoạt động

1. **Lặp lại từng phần tử**:
   - Quy trình đã cung cấp được thực hiện cho mọi phần tử trong danh sách theo thứ tự.

2. **Thực hiện tác dụng phụ**:
   - Các tác dụng phụ thường gặp bao gồm in, ghi nhật ký hoặc sửa đổi các biến bên ngoài. Không giống như `map`, `for-each` không trả về danh sách mới.

---

#### Ví dụ: Sử dụng với nhiều danh sách

Nếu nhiều danh sách được cung cấp, `for-each` sẽ xử lý các phần tử tương ứng từ mỗi danh sách.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Hàm `sum-and-print` tính tổng các phần tử tương ứng từ 2 danh sách và in kết quả.

**Đầu ra**: `5 7 9`

---

### Tóm tắt

- Hàm `for-each` rất hữu ích để thực hiện các tác dụng phụ đối với từng thành phần của danh sách.
- Không giống như `map`, `for-each` không tạo danh sách mới—nó chỉ tập trung vào các tác dụng phụ của quy trình.
- Có thể xử lý đồng thời nhiều danh sách, áp dụng thủ tục cho các phần tử tương ứng.

Bằng cách sử dụng `for-each`, bạn có thể xử lý danh sách một cách hiệu quả khi mục tiêu là thực hiện hành động thay vì chuyển đổi dữ liệu.