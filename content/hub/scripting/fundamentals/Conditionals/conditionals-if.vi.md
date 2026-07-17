---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
Ở dạng đơn giản nhất, điều kiện `if` trong Scheme đánh giá một điều kiện và dựa trên kết quả, thực thi một trong hai khối mã có thể có. Hình thức đơn giản nhất trông như thế này:

```scheme
(if test-is-true
  do-this)
```

- Nếu `test` được đánh giá là đúng (`#t`), **khối mã trong hệ quả** sẽ được thực thi. Khối có thể trả về một giá trị hoặc thực hiện các hành động khác, chẳng hạn như gán một biến hoặc in kết quả.

### Ví dụ

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Trong trường hợp này, `test` là `(< 0 1)` (kiểm tra xem 0 có nhỏ hơn 1 không).
- Vì thử nghiệm cho kết quả là đúng (`#t`), khối mã `(lumi-message "True!")` được thực thi, in ra `"True!"`.

### Thêm điều kiện khác: `if-else`

Khi sử dụng điều kiện `if` với khối mã thay thế (trường hợp `else`), cấu trúc trông như thế này:

```scheme
(if test
  do-this
  else-do-this)
```

- Nếu `test` được đánh giá là đúng (`#t`), khối mã **kết quả** sẽ được thực thi.
- Nếu `test` được đánh giá là sai (`#f`), khối mã **thay thế** sẽ được thực thi.

```scheme
(if test
  consequent
  alternative)
```

### Cách thức hoạt động

1. **Biểu thức kiểm tra**:
   - Biểu thức `test` được đánh giá đầu tiên.

2. **Kết quả dựa trên điều kiện**:
   - Nếu `test` được đánh giá là đúng (`#t`), **khối mã hệ quả** sẽ được thực thi.
   - Nếu `test` được đánh giá là sai (`#f`), **khối mã thay thế** sẽ được thực thi.

Cả hai khối mã `consequent` và `alternative` đều có thể thực hiện bất kỳ thao tác Scheme hợp lệ nào, bao gồm trả về giá trị, sửa đổi biến hoặc quy trình đang chạy.

### Ví dụ

#### Ví dụ 1: Trả về một giá trị

```scheme
(if (< 0 1)
  1
  0)
```

- Ở đây `test` là `(< 0 1)` (kiểm tra xem 0 có nhỏ hơn 1 không).
- Vì thử nghiệm cho kết quả là đúng (`#t`), khối **consequent** (`1`) được thực thi và giá trị của nó được trả về.

Kết quả: **1**

#### Ví dụ 2: Đánh giá khối bắt đầu

Trong trường hợp bạn cần thực hiện nhiều hành động khi điều kiện đúng hoặc sai, bạn có thể sử dụng `begin` hoặc `let` để nhóm chúng lại với nhau.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Trong ví dụ này, `test` là `(= 0 1)` (kiểm tra xem 0 có bằng 1 không).
- Vì thử nghiệm cho kết quả sai (`#f`), nên khối **thay thế** được thực thi:
  - Đầu tiên nó in ra `"False condition met, calculating..."`.
  - Sau đó, nó tính toán `(* 3 4)` và trả về `12`.

Kết quả: **In "Điều kiện sai, đang tính..." và trả về 12.**

#### Ví dụ 3: Đánh giá câu lệnh let

Việc sử dụng `let` cho phép chúng ta khai báo các biến phạm vi cục bộ với khối mã.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Trong ví dụ này, `test` là `(= 1 1)` (kiểm tra xem 1 có bằng 1 không).
- Vì thử nghiệm cho kết quả là đúng (`#t`), nên khối **consequent** được thực thi:
  - Đầu tiên nó in ra `"True condition met, calculating..."`.
  - Sau đó, nó tính toán `(* -1 10)` và trả về `-10`.

Kết quả: **In "Điều kiện đúng, đang tính..." và trả về -10.**

### Tóm tắt

- Điều kiện `if` là một công cụ mạnh mẽ trong Scheme để đánh giá các điều kiện và thực thi các khối mã tương ứng.

- Nó có thể xử lý cả các biểu thức đơn giản và các khối mã phức tạp trả về giá trị, sửa đổi các biến hoặc thực hiện các tác dụng phụ.
- Hãy nhớ: Nếu không có khối `else` rõ ràng thì `if` chỉ đánh giá và thực thi **consequent** nếu kiểm tra là đúng. Nếu không, nó sẽ đánh giá và thực thi **lựa chọn thay thế**.