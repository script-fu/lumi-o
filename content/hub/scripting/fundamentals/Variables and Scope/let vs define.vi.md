---
title: "Được đặt tên là let hoặc Local xác định"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bee02ac4fd1ab5ba61ffb50b49dbbba7fc473b141bd88a9cdf6d02aef3ca3a18
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/let vs define"
---
Cả **có tên `let`** và **local `define`** đều là những công cụ mạnh mẽ trong Scheme để cấu trúc mã của bạn nhưng chúng phục vụ các mục đích khác nhau. Hiểu thời điểm sử dụng từng loại sẽ giúp tạo các tập lệnh rõ ràng, mô-đun và hiệu quả.

### Tổng quan

- **Được đặt tên `let`**: Một cấu trúc kết hợp liên kết biến và đệ quy trong phạm vi cục bộ, thường được sử dụng cho các phép tính lặp hoặc đệ quy.
- **Local `define`**: Một cách để xác định các hàm hoặc biến trợ giúp trong phạm vi của hàm kèm theo, giúp chúng có thể tái sử dụng trên các phần khác nhau của hàm đó.

---

### Được đặt tên `let`

#### Đặc điểm:

1. Kết hợp các liên kết biến và đệ quy thành một cấu trúc duy nhất.
2. Nằm trong phạm vi phần thân của khối `let`.
3. Lý tưởng cho **đệ quy cục bộ** hoặc các quy trình lặp cụ thể cho một tác vụ duy nhất.

#### Cú pháp

```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Ví dụ: Tính tổng các phần tử của một danh sách

```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Kết quả**: `10`

- **Cách hoạt động**: Hàm `loop` được xác định trong `let`, cho phép các lệnh gọi đệ quy có liên kết được cập nhật.

---

### Địa phương `define`

#### Đặc điểm:

1. Cho phép tạo các hàm trợ giúp hoặc các biến có thể tái sử dụng trong hàm kèm theo.
2. Phạm vi chức năng bao quanh nhưng có thể nhìn thấy khắp cơ thể của nó.
3. Lý tưởng cho việc mô-đun hóa mã với nhiều bước hoặc logic có thể tái sử dụng.

#### Cú pháp

```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Ví dụ: Xử lý nhiều giá trị

```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Hàm trợ giúp cục bộ
  (define (cube x) (* x x x))  ;; Hàm trợ giúp cục bộ
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Kết quả**: `41` (Tính \(2^2 + 3^3 + 4^2\))

- **Cách hoạt động**: Các hàm trợ giúp `square` và `cube` có thể tái sử dụng trong hàm `process-values`, cho phép logic mô-đun.

---

### Điểm khác biệt chính

| **Khía cạnh** | **Được đặt tên `let`** | **Địa phương `define`** |
|--------------------------|---------------------------------------------------|------------------------------------------------|
| **Mục đích** | Kết hợp đệ quy và lặp lại theo cách cục bộ. | Xác định các hàm hoặc biến trợ giúp có thể tái sử dụng. |
| **Phạm vi** | Giới hạn trong phần thân của khối `let`.           | Hiển thị trong suốt chức năng kèm theo.      |
| **Khả năng tái sử dụng** | Không thể tái sử dụng bên ngoài khối `let`.             | Có thể tái sử dụng nhiều lần trong hàm.    |
| **Trường hợp sử dụng tốt nhất** | Đệ quy cục bộ hoặc lặp lại gắn liền với một nhiệm vụ duy nhất. | Mô-đun hóa mã với nhiều bước có thể tái sử dụng. |
| **Cú pháp** | Kết hợp ràng buộc và đệ quy trong một cấu trúc.  | Xác định rõ ràng các hàm hoặc biến.      |

---

### Khi nào nên sử dụng Được đặt tên `let`

1. **Logic sử dụng một lần**: Khi phép đệ quy hoặc phép lặp dành riêng cho một phép tính đơn lẻ.
2. **Đóng gói**: Để tránh thêm tên hàm bổ sung vào không gian tên của hàm kèm theo.
3. **Lặp lại**: Khi quản lý các biến trung gian trong cấu trúc vòng lặp.

**Ví dụ: Tính giai thừa**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Kết quả**: `120`

---

### Khi nào nên sử dụng cục bộ `define`

1. **Trình trợ giúp có thể tái sử dụng**: Khi logic cần được sử dụng lại trong nhiều phần của hàm.
2. **Thiết kế mô-đun**: Để chia các tính toán phức tạp thành các nhiệm vụ phụ nhỏ hơn, được đặt tên.
3. **Nhiều bước**: Khi cần nhiều hàm trợ giúp cho các phần khác nhau của phép tính.**Ví dụ: Xử lý đầu vào**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Kết quả**: `(13 36)` (Tính \(2^2 + 3^2\) và \(2^2 \cdot 3^2\))

---

### Kết hợp khai báo và nhập liệu trong Named `let`

Một trong những tính năng mạnh mẽ nhất của `let` có tên là khả năng kết hợp **khai báo biến cục bộ** và **tham số đầu vào** để đệ quy thành một cấu trúc duy nhất. Điều này làm cho cái tên `let` vừa ngắn gọn vừa mang tính biểu cảm cho các tác vụ lặp lại hoặc đệ quy.

#### Khai báo biến cục bộ

Trong `let` có tên, các liên kết trong dấu ngoặc đơn đóng vai trò là **biến cục bộ** được khởi tạo với các giá trị cụ thể. Các biến này nằm trong phạm vi nội dung của `let`.

```scheme
(let loop ((x 1)   ;; Khai báo x với giá trị ban đầu 1
           (y 2))  ;; Khai báo y với giá trị ban đầu 2
  (+ x y))         ;; Dùng x và y trong thân hàm
```

- **`x` và `y`** là các biến cục bộ được xác định và khởi tạo như một phần của `let`.

---

#### Tham số đầu vào cho đệ quy

Các biến tương tự cũng đóng vai trò là **tham số đầu vào** cho lệnh gọi đệ quy đến địa chỉ có tên `let`. Khi tên `let` gọi chính nó, nó sẽ cập nhật các biến này bằng các giá trị mới.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Gọi đệ quy với x và y mới
```

- **Lần lặp đầu tiên**: `x = 1`, `y = 2`
- **Lần thứ hai**: `x = 2`, `y = 4`
- **Lần thứ ba**: `x = 3`, `y = 8`, v.v....

---

#### Tương đương với việc sử dụng cục bộ `define`

Tên `let` bao gồm việc khởi tạo biến như một phần cú pháp của nó. Điều này giúp loại bỏ sự cần thiết phải thực hiện một bước riêng để thiết lập các giá trị ban đầu. Hai ví dụ sau là tương đương:

##### Sử dụng Được đặt tên `let`

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Sử dụng cục bộ `define`

```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Lần gọi ban đầu với x = 1, y = 2
```

Cả hai đều thực hiện cùng một tính toán, nhưng `let` có tên kết hợp khai báo biến và thiết lập đệ quy thành một cấu trúc ngắn gọn.

---

#### Ưu điểm của việc kết hợp khai báo và nhập liệu

1. **Tính chính xác**: Được đặt tên là `let` giảm bớt bản mẫu bằng cách hợp nhất việc khởi tạo biến và đệ quy thành một cấu trúc duy nhất.
2. **Rõ ràng**: Làm rõ rằng đệ quy là cục bộ của `let` và được gắn với một tác vụ cụ thể.
3. **Đóng gói**: Logic đệ quy vẫn độc lập và không làm ô nhiễm không gian tên của hàm kèm theo.

Bản chất hai mục đích này của `let`—vừa là khai báo biến vừa là cơ chế nhập đệ quy—là điều khiến nó trở thành một tính năng mạnh mẽ và độc đáo trong lập trình Scheme.

### Tóm tắt

- Sử dụng **có tên `let`** cho **đệ quy cục bộ** hoặc **lặp**, đặc biệt khi logic được liên kết chặt chẽ với một tác vụ duy nhất.
- Sử dụng **local `define`** để **mô-đun hóa mã** với các hàm hoặc biến trợ giúp có thể tái sử dụng.

Bằng cách hiểu được sự khác biệt của chúng, bạn có thể viết các chương trình Scheme ngắn gọn, có tổ chức và dễ bảo trì hơn.