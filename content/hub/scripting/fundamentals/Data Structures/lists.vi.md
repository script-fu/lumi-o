---
title: "Danh sách"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: caf60dbd4ddbab418dd6779d9efba0217982d37086ed8d485680b96142d5ef6f
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/lists"
---
Trong Scheme, **danh sách** là cấu trúc dữ liệu cơ bản được sử dụng để nhóm các giá trị. Danh sách là tập hợp các phần tử được sắp xếp theo thứ tự trong đó mỗi phần tử có thể thuộc bất kỳ loại nào, kể cả danh sách khác. Danh sách được sử dụng rộng rãi trong Scheme cho cả việc lưu trữ dữ liệu và cấu trúc chương trình.

### Ví dụ 1: Danh sách đơn giản

```scheme
(list 1 2 3)
```

- Tạo danh sách gồm ba phần tử: `1`, `2`, và `3`.

Kết quả: **`(1 2 3)`**

---

#### Truy cập các phần tử danh sách

Các phần tử trong danh sách được truy cập bằng quy trình `car` và `cdr`:

- `car` truy xuất phần tử đầu tiên của danh sách.
- `cdr` truy xuất phần còn lại của danh sách (mọi thứ ngoại trừ phần tử đầu tiên).

#### Ví dụ

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Lấy phần tử đầu tiên
(cdr my-list)  ; Lấy phần còn lại của danh sách
```

Kết quả:

- `(car my-list)` trả về `1`
- `(cdr my-list)` trả về `(2 3)`

---

#### Đệ quy đơn giản: Lặp qua danh sách

Bằng cách gọi đệ quy `car` trên `cdr` của danh sách, bạn có thể xử lý từng phần tử một cho đến khi duyệt qua danh sách. Điều này tạo thành cơ sở của nhiều thuật toán xử lý danh sách.

#### Ví dụ: In từng phần tử của danh sách

Đây là một hàm đệ quy đơn giản để in mọi phần tử trong danh sách:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; In phần tử đầu tiên
      (print-elements (cdr lst)))))             ;; Xử lý phần còn lại của danh sách
```

- **Trường hợp cơ sở:** Nếu danh sách trống (`null? lst`), hãy dừng đệ quy.
- **Trường hợp đệ quy:** In phần tử đầu tiên (`car lst`), sau đó gọi hàm trên phần còn lại của danh sách (`cdr lst`).

#### Ví dụ cách sử dụng

```scheme
(print-elements (list 1 2 3))
```

Đầu ra:

- `"1"`
- `"2"`
- `"3"`

Kết quả: "xong"

---

#### Cách thức hoạt động

1. Hàm truy xuất phần tử đầu tiên của danh sách bằng cách sử dụng `car` và xử lý nó.
2. Sau đó, nó sẽ tự gọi chính nó với phần còn lại của danh sách (`cdr`).
3. Quá trình này lặp lại cho đến khi danh sách trống (`null? lst`).

---

### Ví dụ 2: Kiểu hỗn hợp

Danh sách có thể bao gồm các phần tử thuộc nhiều loại khác nhau, bao gồm chuỗi, boolean, số, danh sách khác hoặc thậm chí là kết quả của biểu thức:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Điều này tạo ra một danh sách với:
  - Một số (`42`)
  - Một chuỗi (`"hello"`)
  - Một boolean (`#t`)
  - Một danh sách khác (`(1 2)`)
  - Kết quả của một biểu thức (`(+ 3 4)`, có giá trị là `7`)

Kết quả: **`(42 "hello" #t (1 2) 7)`**

---

Những ví dụ này thể hiện tính linh hoạt của danh sách trong Scheme, khiến chúng trở thành công cụ mạnh mẽ để tổ chức và thao tác dữ liệu.

### Xây dựng danh sách

Quy trình `cons` được sử dụng để xây dựng danh sách mới bằng cách kết hợp một phần tử với danh sách hiện có.

```scheme
(cons new-element existing-list)
```

#### Ví dụ

```scheme
(cons 0 (list 1 2 3))
```

- Thêm `0` vào đầu danh sách `(1 2 3)`.

Kết quả: **`(0 1 2 3)`**

---

### Kiểm tra danh sách

Quy trình `list?` kiểm tra xem giá trị đã cho có phải là danh sách hay không.

```scheme
(list? value)
```

#### Ví dụ: danh sách?

```scheme
(list? (list 1 2 3))  ; Kiểm tra (list 1 2 3) có phải danh sách
(list? 42)            ; Kiểm tra 42 có phải danh sách
```

Kết quả:

- `(list? (list 1 2 3))` trả về `#t` (đúng)
- `(list? 42)` trả về `#f` (sai)

---

### Các thao tác trên danh sách

Scheme cung cấp một số quy trình dựng sẵn để làm việc với danh sách, bao gồm:

- `length`: Trả về số phần tử trong một danh sách.
- `append`: Kết hợp hai hoặc nhiều danh sách thành một.
- `reverse`: Trả về danh sách mới có các phần tử theo thứ tự ngược lại.

```scheme
(length (list 1 2 3))          ; Trả về 3
(append (list 1 2) (list 3 4)) ; Trả về (1 2 3 4)
(reverse (list 1 2 3))         ; Trả về (3 2 1)
```

Kết quả:

- `(length (list 1 2 3))` trả về `3`
- `(append (list 1 2) (list 3 4))` trả về `(1 2 3 4)`
- `(reverse (list 1 2 3))` trả về `(3 2 1)`

#### Sử dụng `list-ref`

Quy trình `list-ref` truy xuất phần tử tại chỉ mục được chỉ định của danh sách (chỉ mục dựa trên 0).

```scheme
(list-ref lst index)
```

- **`lst`**: Danh sách cần lấy phần tử.
- **`index`**: Chỉ mục dựa trên 0 cho biết phần tử nào sẽ trả về.

##### Ví dụ: list-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Lấy phần tử tại chỉ số 2
```

Kết quả: `30`

---

### Danh sách lồng nhau

Danh sách trong Scheme có thể chứa các danh sách khác dưới dạng thành phần, tạo cấu trúc lồng nhau.

#### Ví dụ: Tạo danh sách lồng nhau

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Tạo một danh sách gồm ba phần tử, mỗi phần tử chính là một danh sách.

Kết quả: **`((1 2) (3 4) (5))`**

---

#### Truy cập dữ liệu lồng nhau

Để truy cập các phần tử trong danh sách lồng nhau, bạn có thể sử dụng kết hợp `car` và `cdr` để điều hướng trong cấu trúc.

#### Ví dụ: Truy cập phần tử

```scheme
(car nested-list)              ; Lấy phần tử đầu tiên: (1 2)
(car (car nested-list))        ; Lấy phần tử đầu tiên của danh sách con đầu tiên: 1
(cdr (car nested-list))        ; Lấy phần còn lại của danh sách con đầu tiên: (2)
(car (cdr (car nested-list)))  ; Lấy phần tử thứ hai của danh sách con đầu tiên: 2
```

---

#### Giải thích

1. **`car nested-list`**:
   - Truy xuất phần tử đầu tiên của `nested-list`, là `(1 2)`.

2. **`car (car nested-list)`**:
   - Truy xuất phần tử đầu tiên của `(1 2)`, là `1`.

3. **`cdr (car nested-list)`**:
   - Truy xuất phần còn lại của `(1 2)`, tức là `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Truy xuất phần tử đầu tiên của `(2)`, là `2`.

---

#### Ví dụ: Truy cập các phần tử từ danh sách con khác

```scheme
(car (cdr nested-list))        ; Lấy danh sách con thứ hai: (3 4)
(car (car (cdr nested-list)))  ; Lấy phần tử đầu tiên của danh sách con thứ hai: 3
```

---

Cách tiếp cận này cho phép bạn điều hướng và truy cập một cách có hệ thống các phần tử cụ thể trong danh sách lồng nhau, mang lại sự linh hoạt mạnh mẽ khi làm việc với dữ liệu phân cấp.

### Tóm tắt

- **Danh sách** trong Scheme là các cấu trúc dữ liệu linh hoạt và cần thiết.
- Sử dụng `list` để tạo danh sách, `car` và `cdr` để truy cập các phần tử và `cons` để xây dựng danh sách.
- Các quy trình tích hợp sẵn như `length`, `append`, `reverse` và `list-ref` giúp thao tác trên danh sách trở nên dễ dàng và hiệu quả.
- Danh sách có thể được lồng vào nhau, cho phép cấu trúc dữ liệu phức tạp cho các trường hợp sử dụng nâng cao.