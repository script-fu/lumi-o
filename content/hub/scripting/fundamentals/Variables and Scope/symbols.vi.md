---
title: "Ký hiệu"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 4153c94fca6fa6c5e1e98ac9449a9e7bd9cdc5b9e5dc4b96da5d8d1e8de3df43
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/symbols"
---
Ký hiệu là một trong những kiểu dữ liệu cốt lõi trong Scheme, đại diện cho các mã định danh duy nhất, không thể thay đổi. Chúng chủ yếu được sử dụng làm khóa, điểm đánh dấu hoặc phần giữ chỗ trong các chương trình, khiến chúng trở nên cần thiết để viết mã rõ ràng và biểu cảm.

Một ký hiệu trong Scheme tương tự như một chuỗi nhưng khác ở chỗ các ký hiệu là **duy nhất** và **nguyên tử**. Điều này có nghĩa là hai ký hiệu có cùng tên được đảm bảo là cùng một đối tượng, cho phép kiểm tra tính bằng nhau nhanh chóng và sử dụng hiệu quả trong cấu trúc dữ liệu.

### Cú pháp

Một biểu tượng được viết dưới dạng một chuỗi các ký tự:

- Bắt đầu bằng một chữ cái, theo sau là các chữ cái, chữ số hoặc ký tự đặc biệt như `-`, `+` hoặc `*`.
- Các ký hiệu có phân biệt chữ hoa chữ thường theo mặc định.

Ví dụ:

```scheme
'hello       ; Ký hiệu tên `hello`
'foo-bar     ; Ký hiệu tên `foo-bar`
'*special*   ; Ký hiệu tên `*special*`
```

## Tạo biểu tượng

Các ký hiệu thường được tạo bằng toán tử **quote** (`'`), toán tử này yêu cầu Scheme coi tên đó như một ký hiệu thay vì đánh giá nó dưới dạng một biến hoặc hàm.

### Ví dụ

```scheme
'my-symbol   ; Tạo ký hiệu `my-symbol`
```

Bạn cũng có thể tạo ký hiệu theo chương trình bằng cách sử dụng quy trình `string->symbol` để chuyển đổi một chuỗi thành ký hiệu.

```scheme
(string->symbol "dynamic-symbol")
```

**Kết quả**: `'dynamic-symbol`


## So sánh các ký hiệu

Vì các ký hiệu là duy nhất nên bạn có thể so sánh chúng một cách hiệu quả bằng cách sử dụng `eq?`.

### Ví dụ

```scheme
(eq? 'apple 'apple)   ; #t (cùng ký hiệu)
(eq? 'apple 'orange)  ; #f (các ký hiệu khác nhau)
```

Điều này làm cho các ký hiệu trở nên lý tưởng để sử dụng làm khóa trong cấu trúc dữ liệu hoặc điểm đánh dấu trong mã của bạn.

## Sử dụng ký hiệu

Các ký hiệu thường được sử dụng trong Scheme để:

1. **Khóa trong Danh sách kết hợp:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Trả về (name . "Alice")
```

2. **Số nhận dạng trong Mã:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Quy trình làm việc với ký hiệu

Scheme cung cấp các quy trình tích hợp để làm việc với các ký hiệu:

| Thủ tục | Mô tả |
|-------------------|-----------------------------------------------------------------------------|
| **`symbol?`** | Kiểm tra xem một đối tượng có phải là một biểu tượng hay không.                                            |
| **`eq?`** | So sánh hai biểu tượng để nhận dạng (so sánh nhanh).                       |
| **`symbol->string`** | Chuyển đổi một ký hiệu thành một chuỗi (hữu ích cho việc hiển thị hoặc gỡ lỗi).          |
| **`string->symbol`** | Chuyển đổi một chuỗi thành ký hiệu (hữu ích cho việc tạo động các mã định danh). |

### Ví dụ

```scheme
(symbol? 'example)            ; #t (đúng: đó là một biểu tượng)
(symbol->string 'example)     ; "ví dụ"
(string->symbol "new-symbol") ; 'biểu tượng mới
```

## Tóm tắt

Ký hiệu là một cách nhẹ nhàng, hiệu quả để thể hiện mã định danh, khóa và điểm đánh dấu trong Scheme. Tính bất biến và khả năng kiểm tra danh tính nhanh chóng khiến chúng trở nên lý tưởng cho nhiều tác vụ lập trình. Hiểu cách sử dụng các ký hiệu một cách hiệu quả sẽ nâng cao khả năng viết mã Scheme rõ ràng và biểu cảm của bạn.