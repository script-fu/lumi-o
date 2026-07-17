---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
Trong Scheme, điều kiện `cond` được sử dụng để chọn một trong một số khối mã có thể thực thi, dựa trên nhiều thử nghiệm. Nó giống như một nhánh đa nhánh `if`, trong đó mỗi nhánh được kiểm tra theo thứ tự cho đến khi tìm thấy kết quả khớp.

### Cú pháp

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Mỗi điều kiện được đánh giá theo thứ tự chúng được viết.
- Khi một điều kiện được đánh giá là đúng (`#t`), **kết quả** tương ứng của nó sẽ được thực thi và biểu thức `cond` sẽ ngừng đánh giá các điều kiện tiếp theo.
- Mệnh đề `else` là tùy chọn và đóng vai trò dự phòng nếu không có thử nghiệm nào đánh giá là đúng.

### Cách thức hoạt động

1. **Kiểm tra từng điều kiện**:
   - `cond` đánh giá các điều kiện theo thứ tự được liệt kê.

2. **Thực hiện kết quả so khớp**:
   - Khi tìm thấy thử nghiệm đầu tiên được đánh giá là đúng (`#t`), **kết quả** của nó sẽ được thực thi.
   - Nếu không có kiểm tra nào đánh giá là đúng và có mệnh đề `else` thì **fallback-consequent** sẽ được thực thi.

### Ví dụ

#### Ví dụ 1: Hệ quả biểu thức đơn

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Lần kiểm tra đầu tiên `(< 3 2)` có kết quả sai (`#f`).
- Bài kiểm tra thứ hai `(= 3 3)` đánh giá là đúng (`#t`), do đó `"This will run"` được trả về.
- Mệnh đề `else` không được thực thi vì đã tìm thấy kết quả khớp.

Kết quả: **"Điều này sẽ chạy"**

#### Ví dụ 2: Nhiều hành động sử dụng `begin`

Khi một hệ quả liên quan đến nhiều hành động, hãy sử dụng `begin` để nhóm chúng:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- Lần kiểm tra đầu tiên `(< 5 3)` có kết quả sai (`#f`).
- Bài kiểm tra thứ hai `(> 5 3)` đánh giá là đúng (`#t`):
  - Nó in `"Condition met"`.
  - Sau đó nó tính toán `(* 5 5)` và trả về `25`.

Kết quả: **In "Điều kiện đáp ứng" và trả về 25.**

#### Ví dụ 3: Sử dụng khối `let` trong kết quả

Khi bạn cần giới thiệu các biến cục bộ, hãy sử dụng khối `let`:

```scheme
(cond
  ;; Trường hợp 1: Nếu 0 nhỏ hơn -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Trường hợp 2: Nếu 0 lớn hơn -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Trường hợp mặc định: Nếu không điều kiện nào ở trên thỏa mãn
  (else
    (let ((z 0))
      z)))
```

- Kiểm tra đầu tiên `(< 0 -1)` là sai.
- Phép thử thứ hai `(> 0 -1)` là đúng, vì vậy:
  - Khối `let` được thực thi, liên kết `y` với `20`.
  - Nó in `"Positive condition met"`.
  - Sau đó nó tính toán `(+ y y)` và trả về `40`.

Kết quả: **In "Đã đáp ứng điều kiện dương" và trả về 40.**

#### Ví dụ 4: Dự phòng với `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Cả hai phép thử đầu tiên đều không có giá trị đúng.
- Mệnh đề `else` được thực thi và trả về `"Fallback value"`.

Kết quả: **"Giá trị dự phòng"**

### Tóm tắt

- Sử dụng `cond` để xử lý nhiều điều kiện một cách rõ ràng và ngắn gọn.
- Kết quả có thể là các biểu thức đơn lẻ hoặc các hành động được nhóm bằng cách sử dụng `begin`.
- Sử dụng `let` để khai báo các biến cục bộ phục vụ tính toán.
- Luôn bao gồm mệnh đề `else` làm phương án dự phòng để xử lý các trường hợp không mong muốn.

Tính linh hoạt này làm cho `cond` trở thành một công cụ mạnh mẽ và dễ đọc để xử lý logic phân nhánh phức tạp.