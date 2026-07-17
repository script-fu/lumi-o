---
title: "Vector"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 23911f048f43dea4e07f47834a477d10f6eaebd9c9bd1b975db79ed1442deaaf
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/vectors"
---
Trong Scheme, vector là một cấu trúc dữ liệu cơ bản khác được sử dụng để nhóm các giá trị. Không giống như danh sách, vector là tập hợp các phần tử được lập chỉ mục, có kích thước cố định, cung cấp khả năng truy cập và cập nhật ngẫu nhiên nhanh hơn. Mỗi phần tử trong một vector có thể thuộc bất kỳ loại nào, kể cả một vector khác. Các vector được biểu diễn bằng dấu # theo sau là dấu ngoặc đơn. `#(1 2 3)`

Mặc dù vector và danh sách có thể trông giống nhau nhưng chúng phục vụ các mục đích khác nhau trong lập trình Scheme:

- Danh sách được sử dụng phổ biến hơn cho các hoạt động đệ quy và cấu trúc động, vì việc triển khai nút liên kết của chúng cho phép thao tác hiệu quả việc bắt đầu và truyền tải của chúng thông qua phân rã đệ quy.

- Mặt khác, vector được tối ưu hóa cho các tình huống yêu cầu truy cập ngẫu nhiên vào các phần tử hoặc cập nhật tại các chỉ mục cụ thể, khiến chúng phù hợp hơn với các trường hợp sử dụng như bảng tra cứu, cấu hình kích thước cố định hoặc các hoạt động được lập chỉ mục quan trọng về hiệu suất.

Về bản chất, danh sách là sự lựa chọn tự nhiên cho các thuật toán đệ quy và dữ liệu có kích thước động, trong khi vector tỏa sáng khi các mẫu truy cập có kích thước cố định hoặc được lập chỉ mục là tối quan trọng.

### Vector đơn giản

```scheme
(vector 1 2 3)
```

- Tạo một vector gồm ba phần tử: `1`, `2`, và `3`.

Kết quả: **`#(1 2 3)`**

#### Truy cập các phần tử Vector

Các phần tử trong vector được truy cập bằng thủ tục `vector-ref`, nó truy xuất phần tử tại một chỉ mục được chỉ định (bắt đầu từ `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Lấy phần tử tại chỉ số 0
(vector-ref my-vector 1)  ; Lấy phần tử tại chỉ số 1
```

#### Lặp lại: Xử lý từng phần tử trong một Vector

Bạn có thể lặp qua một vector bằng vòng lặp hoặc đệ quy. Scheme cung cấp `vector-length` để xác định kích thước của vector. Đây là một vòng lặp đơn giản để in mọi phần tử trong một vector:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; In phần tử
        (loop (+ i 1)))                                    ; Xử lý chỉ số tiếp theo
      (lumi-message "done"))))                             ; Kết thúc vòng lặp
```

- **Trường hợp cơ sở:** Nếu chỉ số `i` đạt đến độ dài của vector, hãy dừng vòng lặp.
- **Trường hợp đệ quy:** In phần tử tại chỉ mục `i`, sau đó tăng `i`.

#### Ví dụ cách sử dụng

```scheme
(print-elements (vector 1 2 3))
```

Kết quả:

- `"1"`
- `"2"`
- `"3"`

Kết quả: "xong"

### Vector hỗn hợp

Các vector có thể bao gồm các phần tử thuộc nhiều loại khác nhau, bao gồm chuỗi, boolean, số, các vector khác hoặc thậm chí là kết quả của các biểu thức:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Điều này tạo ra một vector với:
  - Một số (`42`)
  - Một chuỗi (`"hello"`)
  - Một boolean (`#t`)
  - Một vector khác (`#(1 2)`)
  - Kết quả của một biểu thức (`(+ 3 4)`, có giá trị là `7`)

Kết quả: **`#(42 "hello" #t #(1 2) 7)`**

### Xây dựng vector

Các vector được tạo bằng cách sử dụng `vector` hoặc bằng cách sử dụng `make-vector` để tạo vector có kích thước cố định với giá trị ban đầu.

```scheme
(make-vector 5 0)
```

Tạo một vector có kích thước `5` với tất cả các phần tử được khởi tạo thành `0`.

Kết quả: `#(0 0 0 0 0)`

### Cập nhật vector

Quy trình `vector-set!` cập nhật một phần tử trong vector tại một chỉ mục được chỉ định.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Đặt phần tử thứ hai thành 42
my-vector
```

Kết quả: `#(1 42 3)`

### Kiểm tra vector

Quy trình `vector?` kiểm tra xem giá trị đã cho có phải là vector hay không.

```scheme
(vector? (vector 1 2 3))  ; Kiểm tra #(1 2 3) có phải vector
(vector? 42)              ; Kiểm tra 42 có phải vector
```

Kết quả:

- `(vector? (vector 1 2 3))` trả về `#t` (đúng)
- `(vector? 42)` trả về `#f` (sai)

### Vector và hành vi truyền qua tham chiếu

Trong Scheme, vector có thể thay đổi và được truyền bằng tham chiếu. Điều này có nghĩa là khi bạn truyền một vector cho một hàm, hàm đó có thể sửa đổi trực tiếp vector gốc. Mọi thay đổi được thực hiện đối với vector bên trong hàm cũng sẽ được phản ánh bên ngoài hàm. Hành vi này hữu ích để chia sẻ và cập nhật dữ liệu một cách hiệu quả trên nhiều chức năng, nhưng cũng cần thận trọng để tránh các tác dụng phụ ngoài ý muốn.

#### Ví dụ: Sửa đổi Vector trong Hàm

Đây là một ví dụ minh họa cách các vector được truyền bằng tham chiếu và được sửa đổi:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Cập nhật vector tại chỉ số chỉ định

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Sửa phần tử thứ hai thành 99
my-vector                              ; Vector gốc đã được cập nhật
```

Kết quả: `#(10 99 30)`

#### Giải thích từng bước

1. **Tạo một Vector:** `my-vector` được khởi tạo với các giá trị `10`, `20` và `30`.
2. **Chuyển tới một hàm:** `my-vector` được chuyển tới `modify-vector` cùng với chỉ mục và giá trị mới cần cập nhật.
3. **Sửa đổi trong Hàm:** Quy trình `vector-set!` cập nhật giá trị tại chỉ mục đã chỉ định trực tiếp trong vector gốc.
4. **Phản ánh các thay đổi:** Vì vector được truyền theo tham chiếu nên các thay đổi được thực hiện trong hàm sẽ được phản ánh trong vector gốc.

#### Ý nghĩa của việc truyền qua tham chiếu

- **Hiệu suất:** Truyền vector theo tham chiếu hiệu quả vì nó tránh được việc sao chép các cấu trúc lớn.
- **Tác dụng phụ:** Hãy thận trọng khi chia sẻ vector giữa các hàm để tránh những sửa đổi ngoài ý muốn đối với dữ liệu được chia sẻ.

### Các thao tác trên vector

Scheme cung cấp một số quy trình dựng sẵn để làm việc với vector, bao gồm:

- `vector-length`: Trả về số phần tử trong một vector.
- `vector->list`: Chuyển đổi một vector thành một danh sách.
- `list->vector`: Chuyển đổi danh sách thành vector.

```scheme
(vector-length (vector 1 2 3))         ; Trả về 3
(vector->list (vector 1 2 3))          ; Chuyển vector thành danh sách: (1 2 3)
(list->vector (list 1 2 3))            ; Chuyển danh sách thành vector: #(1 2 3)
```

Kết quả:

- `(vector-length (vector 1 2 3))` trả về `3`
- `(vector->list (vector 1 2 3))` trả về `(1 2 3)`
- `(list->vector (list 1 2 3))` trả về `#(1 2 3)`

### Các vector lồng nhau

Các vector trong Scheme có thể chứa các vector khác làm phần tử, tạo ra một cấu trúc lồng nhau.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Tạo một vector gồm ba phần tử, mỗi phần tử chính là một vector.

Kết quả: **`#(#(1 2) #(3 4) #(5))`**

#### Truy cập dữ liệu lồng nhau

Để truy cập các phần tử trong một vector lồng nhau, hãy sử dụng `vector-ref` nhiều lần để điều hướng qua cấu trúc.

#### Ví dụ: Truy cập phần tử

```scheme
(vector-ref nested-vector 0)              ; Lấy phần tử đầu tiên: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Lấy phần tử thứ hai của vector đầu tiên: 2
```

### Tóm tắt

- **Vector** trong Scheme là các cấu trúc dữ liệu được lập chỉ mục, có kích thước cố định.
- Sử dụng `vector` để tạo vector, `vector-ref` để truy cập các phần tử và `vector-set!` để cập nhật các phần tử.
- Các quy trình tích hợp sẵn như `vector-length`, `vector->list` và `list->vector` cho phép vận hành linh hoạt.
- Các vector lồng nhau cho phép cấu trúc dữ liệu phức tạp, phân cấp.