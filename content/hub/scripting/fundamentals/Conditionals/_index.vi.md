---
title: "Câu điều kiện"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/_index"
---
Điều kiện là một phần cơ bản của lập trình, cho phép các tập lệnh đưa ra quyết định và kiểm soát luồng của chúng dựa trên các tiêu chí cụ thể. Trong Scheme, dựa trên ngôn ngữ lập trình Scheme, các điều kiện cho phép bạn tạo các tập lệnh động và thông minh thích ứng với việc thay đổi đầu vào, môi trường hoặc hành động của người dùng.

### Vai trò của điều kiện trong Scheme

Điều kiện phục vụ một số mục đích chính trong tập lệnh của bạn:
- **Logic chỉ đạo:** Chúng cho phép bạn chạy các đoạn mã khác nhau tùy thuộc vào điều kiện nhất định là đúng hay sai.
- **Cải thiện tính linh hoạt:** Bằng cách phản hồi linh hoạt với các đầu vào hoặc trạng thái, các điều kiện giúp tập lệnh của bạn xử lý nhiều tình huống khác nhau.
- **Đơn giản hóa độ phức tạp:** Chúng chia việc ra quyết định thành các cấu trúc có thể quản lý được, giúp mã dễ đọc, gỡ lỗi và bảo trì hơn.

### Các loại điều kiện có sẵn

Scheme cung cấp một số cấu trúc có điều kiện, mỗi cấu trúc phù hợp với các nhu cầu logic khác nhau:
- **`if`:** Để đưa ra các quyết định nhị phân đơn giản, thực thi một khối mã nếu điều kiện là đúng và khối mã khác nếu điều kiện đó sai.
- **`cond`:** Cấu trúc đa nhánh mạnh mẽ để xử lý nhiều điều kiện một cách rõ ràng, có cấu trúc.
- **`and` / `or`:** Toán tử logic đánh giá sự kết hợp của các điều kiện, cho phép đưa ra quyết định phức tạp hơn.
- **`else`:** Một công cụ tổng hợp xác định hành vi dự phòng khi không đáp ứng được điều kiện đã chỉ định nào.

### Cách hoạt động của điều kiện

Câu điều kiện thường bao gồm:
1. **Đánh giá một điều kiện:** Biểu thức kiểm tra xác định xem điều kiện đó là đúng hay sai.
2. **Thực thi phân nhánh:** Dựa trên đánh giá, tập lệnh sẽ chọn khối mã nào sẽ thực thi.
3. **Trả về một giá trị (Tùy chọn):** Trong một số trường hợp, các điều kiện cũng có thể tạo ra một giá trị mà các phần khác của tập lệnh có thể sử dụng.