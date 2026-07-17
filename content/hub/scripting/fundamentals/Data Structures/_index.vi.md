---
title: "Cấu trúc dữ liệu"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: 352594bbda9977488d773240c50663f63fd432a17483772a9cbf8d59dab378be
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/_index"
---
Trong Scheme, **cấu trúc dữ liệu** là công cụ thiết yếu để tổ chức, lưu trữ và thao tác dữ liệu. Chúng cho phép các nhà phát triển xây dựng các tập lệnh hiệu quả, dễ đọc và có thể tái sử dụng. Bằng cách chọn cấu trúc dữ liệu phù hợp cho một vấn đề cụ thể, bạn có thể tối ưu hóa cả hiệu suất và độ rõ ràng của mã.

## Cấu trúc dữ liệu chính trong Scheme

Scheme cung cấp một số cấu trúc dữ liệu mạnh mẽ và linh hoạt, mỗi cấu trúc phù hợp cho các nhiệm vụ cụ thể. Cấu trúc dữ liệu sơ cấp bao gồm:

### Danh sách

Danh sách là tập hợp các phần tử có thứ tự có thể tăng hoặc thu nhỏ một cách linh hoạt. Chúng lý tưởng cho dữ liệu tuần tự hoặc phân cấp và được sử dụng rộng rãi trong lập trình chức năng.

Các tính năng chính:
- Kích thước động.
- Các phần tử có thể có nhiều loại khác nhau.
- Thường dùng cho các thuật toán đệ quy và biểu diễn cấu trúc dạng cây.

Ví dụ về sử dụng:
- Quản lý bộ sưu tập các mặt hàng.
- Trình bày trình tự hoặc thứ bậc.

---

### Vector

Vector là tập hợp các phần tử có kích thước cố định, được lập chỉ mục để truy cập nhanh. Chúng phù hợp nhất cho các tình huống trong đó hiệu suất và quyền truy cập vị trí là rất quan trọng.

Các tính năng chính:
- Cố định kích thước khi tạo.
- Các phần tử được truy cập theo chỉ mục của chúng.
- Nhanh hơn danh sách đối với một số hoạt động nhất định như truy cập ngẫu nhiên.

Ví dụ về sử dụng:
- Lưu trữ cấu hình hoặc dữ liệu có kích thước cố định.
- Tra cứu và cập nhật nhanh chóng dựa trên vị trí.

---

### Chọn cấu trúc dữ liệu phù hợp

Quyết định sử dụng **danh sách** hoặc **vector** tùy thuộc vào nhu cầu cụ thể của tập lệnh của bạn. Dưới đây là một số hướng dẫn:

| Tính năng | Danh sách | Vector |
|--------------------------|---------------------------------------|--------------------------------|
| **Kích thước linh hoạt** | Năng động | Đã sửa |
| **Tốc độ truy cập** | Chậm hơn (truy cập tuần tự) | Nhanh hơn (truy cập được lập chỉ mục) |
| **Dễ sửa đổi**| Dễ dàng hơn | Khó hơn (cần phân bổ lại)|
| **Trường hợp sử dụng** | Dữ liệu động, đệ quy | Dữ liệu tĩnh, tra cứu nhanh |

---