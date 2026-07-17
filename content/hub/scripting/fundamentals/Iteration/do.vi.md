---
title: "LÀM"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/do"
---
Hàm `do` trong Scheme là một cơ chế lặp cho phép lặp với các điều kiện khởi tạo, cập nhật và kết thúc. Nó đặc biệt hữu ích khi bạn cần thực hiện một chuỗi thao tác với số lần cụ thể hoặc cho đến khi đáp ứng một điều kiện.

Dạng tổng quát của `do` là:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Biến**: (Các) biến vòng lặp.
- **Giá trị ban đầu**: Giá trị bắt đầu của mỗi biến vòng lặp.
- **Cập nhật-biểu thức**: Biểu thức để cập nhật (các) biến vòng lặp ở cuối mỗi lần lặp.
- **Điều kiện kết thúc**: Điều kiện dừng vòng lặp.
- **Biểu thức kết quả**: Giá trị trả về khi vòng lặp kết thúc.
- **Nội dung**: Đoạn mã thực thi trong mỗi lần lặp.

---

### Ví dụ: Tính tổng các số từ 1 đến 5

```scheme
(do ((i 1 (+ i 1))      ; Khởi tạo i = 1, tăng 1 mỗi bước
     (sum 0 (+ sum i))) ; Khởi tạo sum = 0, cộng i vào sum
    ((> i 5) sum)       ; Dừng khi i > 5, trả về sum
  (lumi-message (number->string sum))) ; In sum ở mỗi bước
```

- Biến vòng lặp `i` bắt đầu từ 1 và tăng dần 1 trong mỗi lần lặp.
- Biến `sum` tính tổng của `i`.
- Vòng lặp kết thúc khi `i > 5`, trả về giá trị cuối cùng là `sum`.

**Đầu ra**: `15`

---

### Cách thức hoạt động

1. **Khởi tạo**:
   - Mỗi biến vòng lặp được gán giá trị ban đầu.

2. **Kiểm tra chấm dứt**:
   - Khi bắt đầu mỗi lần lặp, điều kiện kết thúc được kiểm tra. Nếu đúng, vòng lặp sẽ dừng và biểu thức kết quả được đánh giá.

3. **Lặp lại**:
   - Nếu điều kiện kết thúc là sai, phần thân sẽ được thực thi và các biến vòng lặp được cập nhật bằng cách sử dụng các biểu thức cập nhật tương ứng của chúng.

---

### Tóm tắt

- Cấu trúc `do` cung cấp một cách linh hoạt để triển khai các vòng lặp có nhiều biến và điều kiện kết thúc phức tạp.
- Hữu ích cho các tác vụ yêu cầu cập nhật trạng thái qua các lần lặp.
- Điều kiện kết thúc xác định thời điểm vòng lặp kết thúc và có thể trả về kết quả cuối cùng.

Bằng cách sử dụng `do`, bạn có thể triển khai các thuật toán lặp trong Scheme với khả năng kiểm soát chính xác việc khởi tạo, cập nhật và chấm dứt. Điều này làm cho `do` trở thành sự kết hợp giữa **cơ chế liên kết có phạm vi** (như `let`) và **cấu trúc điều khiển lặp**, cho phép cơ chế này xử lý vòng lặp và trạng thái tạm thời một cách rõ ràng, ngắn gọn.