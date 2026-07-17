---
title: "Lặp lại"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: df3e2118b9a580de4eed6ac56d9717aa3cbf555ab66bb49fabb4164b2994af91
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/_index"
---
Lặp lại là nền tảng của lập trình, cho phép các tập lệnh lặp lại các hành động và xử lý việc thu thập dữ liệu một cách hiệu quả. Trong Scheme, dựa trên ngôn ngữ lập trình Scheme, phép lặp cung cấp các công cụ để tự động hóa các tác vụ lặp đi lặp lại, thao tác với cấu trúc dữ liệu và tạo ra các mẫu thực thi phức tạp.

### Vai trò của phép lặp trong Scheme

Việc lặp lại đáp ứng một số mục đích thiết yếu trong tập lệnh của bạn:
- **Tự động lặp lại:** Nó cho phép bạn thực hiện cùng một hành động hoặc một loạt hành động nhiều lần mà không cần sao chép mã.
- **Nâng cao hiệu quả:** Bằng cách xử lý lặp đi lặp lại các cấu trúc dữ liệu, các tập lệnh có thể xử lý các hoạt động quy mô lớn một cách có hệ thống.
- **Tinh giản mã:** Việc lặp lại loại bỏ sự dư thừa, làm cho mã ngắn gọn hơn, dễ đọc và dễ bảo trì hơn.

### Các loại lặp lại có sẵn

Scheme cung cấp một số cấu trúc để lặp lại, mỗi cấu trúc được điều chỉnh cho phù hợp với nhu cầu cụ thể:
- **map:** Áp dụng một hàm cho từng thành phần của danh sách, trả về một danh sách mới kèm theo kết quả.
- **for-each:** Tương tự như `map`, nhưng được sử dụng để thực thi một hàm trên từng phần tử mà không trả về kết quả.
- **do:** Cấu trúc vòng lặp có mục đích chung xử lý nhiều quy trình lặp lại.
- **đệ quy:** Một kỹ thuật mạnh mẽ trong đó các hàm tự gọi chính mình để giải quyết vấn đề theo từng bước.

### Cách lặp hoạt động

Việc lặp lại thường bao gồm:
1. **Xác định sự lặp lại:** Chỉ định hành động lặp lại và dữ liệu hoặc phạm vi cần xử lý.
2. **Thực hiện theo trình tự:** Lặp lại hành động cho từng phần tử, bước hoặc điều kiện cho đến khi hoàn thành.
3. **Trả về kết quả (Tùy chọn):** Tùy thuộc vào cấu trúc, việc lặp lại có thể mang lại kết quả hoặc trạng thái sửa đổi.

Những cấu trúc này cho phép bạn viết các tập lệnh có khả năng thích ứng, hiệu quả và tinh tế, có thể xử lý các tác vụ phức tạp một cách dễ dàng.