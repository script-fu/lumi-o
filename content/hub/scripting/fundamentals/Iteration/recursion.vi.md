---
title: "Đệ quy đơn giản"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/recursion"
---
Đệ quy là một khái niệm mạnh mẽ trong Scheme, trong đó một hàm gọi chính nó để giải các bài toán con nhỏ hơn của bài toán ban đầu. Mẫu **đệ quy đơn giản** bao gồm trường hợp cơ bản để dừng đệ quy và trường hợp đệ quy để giảm thiểu vấn đề.

Cấu trúc chung của hàm đệ quy trông như thế này:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Điều kiện cơ sở**: Dừng đệ quy.
- **Kết quả cơ sở**: Giá trị trả về khi đáp ứng điều kiện cơ bản.
- **Gọi đệ quy**: Lệnh gọi đến chính hàm với các đối số được sửa đổi để đưa phép tính gần hơn với trường hợp cơ sở.

---

### Ví dụ: Tổng các số (1 đến n)

Hàm đệ quy đơn giản để tính tổng các số từ 1 đến n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Trường hợp cơ sở: dừng khi n = 0
    0                          ; Kết quả cơ sở: tổng là 0
    (+ n (sum-to-n (- n 1))))) ; Gọi đệ quy: cộng n hiện tại với kết quả bài toán nhỏ hơn
```

---

#### Cách thức hoạt động: Phá vỡ và lắp ráp lại

Đệ quy hoạt động bằng cách chia nhỏ vấn đề ban đầu thành các phần nhỏ hơn. Mỗi lệnh gọi hàm xử lý một phần và chuyển phần còn lại. Sau khi đạt được trường hợp đơn giản nhất, các kết quả sẽ được tập hợp lại khi quá trình tính toán hoàn tất.

#### Theo dõi từng bước của tổng thành n 3

1. **Cuộc gọi ban đầu**: *tổng-n 3*
   → *(+ 3 (tổng với n 2))*

2. **Cuộc gọi thứ hai**: *tổng-n 2*
   → *(+ 2 (tổng với n 1))*

3. **Cuộc gọi thứ ba**: *tổng-n 1*
   → *(+ 1 (tổng với n 0))*

4. **Trường hợp cơ bản**: *tổng-n 0*
   → *0*

---

#### Tập hợp lại kết quả cuối cùng

Khi trường hợp đơn giản nhất được giải quyết, từng tầng tính toán sẽ hoàn thành:

1. *sum-to-n 0* cho ra *0*
2. *tổng-n 1* trở thành *(+ 1 0) = 1*
3. *tổng-n 2* trở thành *(+ 2 1) = 3*
4. *tổng-n 3* trở thành *(+ 3 3) = 6*

---

### Ví dụ: In từng phần tử của danh sách

Đây là một hàm đệ quy đơn giản để in mọi phần tử trong danh sách:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; In phần tử đầu tiên
      (print-elements (cdr lst)))))             ; Xử lý phần còn lại của danh sách
```

- **Trường hợp cơ sở:** Nếu danh sách trống (*null? lst*), dừng đệ quy.
- **Trường hợp đệ quy:** In phần tử đầu tiên (*car lst*), sau đó gọi hàm trên phần còn lại của danh sách (*cdr lst*).

#### Ví dụ cách sử dụng

```scheme
(print-elements (list 1 2 3))
```

Đầu ra:

- *"1"*
- *"2"*
- *"3"*

Kết quả: *"xong"*

---

#### Cách thức hoạt động

1. Hàm truy xuất phần tử đầu tiên của danh sách bằng cách sử dụng *car* và xử lý nó.
2. Sau đó, nó sẽ tự gọi chính nó với phần còn lại của danh sách (*cdr*).
3. Quá trình này lặp lại cho đến khi danh sách trống (*null? lst*).

---

### Tóm tắt

- Đệ quy đơn giản bao gồm:
  1. **Trường hợp cơ sở**: Dừng đệ quy.
  2. **Trường hợp đệ quy**: Giảm vấn đề đối với trường hợp cơ sở.
- Mỗi lệnh gọi đệ quy sẽ tiến hành tính toán cho đến khi hoàn thành.
- Sau khi đạt được trường hợp cơ sở, các kết quả sẽ được kết hợp khi quá trình đệ quy hoàn tất.

Đệ quy phản ánh cấu trúc của vấn đề và cung cấp một luồng logic, rõ ràng. Luôn đảm bảo trường hợp cơ sở để tránh đệ quy vô hạn.