---
title: "Danh sách hiệp hội (Alists)"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/alists"
---
**danh sách liên kết** (hoặc **alist**) là cấu trúc dữ liệu cơ bản trong Scheme dùng để biểu thị các tập hợp các cặp khóa-giá trị. Nó được triển khai dưới dạng danh sách các cặp, trong đó mỗi cặp liên kết một khóa (thường là ký hiệu) với một giá trị. Alist đơn giản, linh hoạt và rất phù hợp cho các tập dữ liệu vừa và nhỏ.

### Cấu trúc danh sách liên kết

Một alist là một danh sách trong đó mỗi phần tử là một **cặp** (được tạo bằng `cons`). Mỗi cặp bao gồm:

- **Khóa**: Phần tử đầu tiên (thường là ký hiệu).
- **Giá trị**: Phần tử thứ hai, có thể thuộc bất kỳ loại dữ liệu nào.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Khóa**: `'name`, `'age`, `'city`
- **Giá trị**: `"Alice"`, `30`, `"Paris"`
- **Cấu trúc**: Danh sách các cặp:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Tạo danh sách

Bạn có thể tạo một danh sách bằng cách xây dựng các cặp theo cách thủ công hoặc xây dựng danh sách đó theo chương trình bằng cách sử dụng `cons`.

#### Sử dụng trích dẫn đơn (`'`)

Trích dẫn duy nhất (`'`) là cách viết tắt của **quoting**, điều này ngăn Scheme đánh giá biểu thức. Điều này lý tưởng cho việc tạo các danh sách tĩnh trong đó tất cả các khóa và giá trị đều được mã hóa cứng.

```scheme
;; Định nghĩa alist thủ công
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Thêm cặp mới theo chương trình
(define updated-alist (cons '(country . "France") alist))
```

**Kết quả**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Sử dụng backquote (`` ` ``) và dấu phẩy (`,`)

Toán tử trích dẫn ngược (`` ` ``) tương tự dấu nháy đơn, nhưng cho phép chèn động các biểu thức đã được đánh giá bằng dấu phẩy (`,`). Điều này hữu ích cho việc tạo các danh sách trong đó các khóa hoặc giá trị được tính toán trong thời gian chạy.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Kết quả**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### So sánh ví dụ

Danh sách tĩnh sử dụng `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Danh sách động sử dụng `` ` `` và `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Truy cập dữ liệu trong Alist

Để truy xuất một giá trị từ một danh sách, bạn có thể sử dụng hàm `assoc` để tra cứu một cặp theo khóa của nó.

```scheme
(assoc 'name alist)   ; Trả về (name . "Alice")
(assoc 'country alist) ; Trả về #f (không tìm thấy khóa)
```

### Trích xuất giá trị

Sau khi bạn truy xuất một cặp bằng `assoc`, hãy sử dụng `cdr` để trích xuất giá trị:

```scheme
(cdr (assoc 'name alist))   ; Trả về "Alice"
```

### Tóm tắt các tính năng chính

- **Trích dẫn đơn (`'`)**: Tạo một danh sách tĩnh trong đó tất cả các thành phần đều là dữ liệu bằng chữ.
- **Trích dẫn ngược (`` ` ``)**: Cho phép tạo alist động bằng cách trộn phần tử tĩnh với biểu thức đã được đánh giá (dùng `,`).
- **Ký hiệu dấu chấm (`.`)**: Dùng để tạo cặp, liên kết khóa với giá trị trong alist.