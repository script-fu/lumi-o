---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
Trong Scheme, mặc dù `if` thanh lịch và linh hoạt nhưng nó có thể trở nên khó hiểu khi được sử dụng mà không có `else` rõ ràng. Điều này đặc biệt đúng khi mục đích chỉ thực thi một nhánh mã khi điều kiện đúng mà không có hành động thay thế nào cho trường hợp `false`. Trong những trường hợp như vậy, cấu trúc `when` cung cấp giải pháp thay thế rõ ràng và ngắn gọn hơn.

Dạng cơ bản của `when` trông như thế này:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Nếu `test` được đánh giá là đúng (`#t`), thì tất cả các biểu thức trong nội dung của cấu trúc `when` sẽ được thực thi tuần tự.
- Nếu `test` được đánh giá là sai (`#f`), sẽ không có gì xảy ra và không có giá trị nào được trả về.

### Ví dụ

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Tương phản `if` và `when`

Để hiểu rõ hơn sự khác biệt giữa `if` và `when`, hãy xem xét ví dụ sau trong đó cả hai được sử dụng cùng nhau:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Giải thích:

1. **Điều kiện `if`**:
   - Bài kiểm tra `(= 0 1)` kiểm tra xem 0 có bằng 1 hay không.
   - Vì điều này là sai (`#f`), nhánh `else` của `if` được thực thi.

2. **Công trình `when` trong Chi nhánh `else`**:
   - Kiểm tra `when` `(< 0 1)` kiểm tra xem 0 có nhỏ hơn 1 hay không.
   - Vì điều này đúng (`#t`), tất cả các biểu thức trong phần thân của `when` được thực thi tuần tự:
     - Trước tiên, nó in `"The 'when' condition is true!"`.
     - Sau đó, nó in `"Executing multiple actions within 'when'."`.

#### Tại sao nên sử dụng `when` tại đây?

- Việc sử dụng `when` thay vì một `if` khác giúp đơn giản hóa logic khi không cần nhánh `else` rõ ràng cho điều kiện.
- `when` nêu rõ rằng chỉ nhánh thực sự mới có liên quan, giúp giảm khả năng nhầm lẫn.

### Tóm tắt

- Sử dụng `if` khi bạn cần cả nhánh đúng và nhánh sai.
- Sử dụng `when` khi chỉ có một nhánh duy nhất cho trường hợp đúng, đặc biệt khi cần thực thi nhiều hành động.
- Việc kết hợp `if` và `when` có thể giúp cấu trúc các điều kiện phức tạp hơn một cách rõ ràng và chính xác.