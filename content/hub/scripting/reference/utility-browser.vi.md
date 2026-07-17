---
title: "Trình duyệt tiện ích"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 99abaafdc68cf3433959e5db87130b22c51cfbd5a98697fa807732b9fdae9ff0
url: "hub/scripting/reference/utility-browser"
translation_lock: true
---
Trình duyệt tiện ích cho phép bạn khám phá stdlib tiện ích Scheme tích hợp sẵn đi kèm với Lumi mà không cần phải rời khỏi ứng dụng hoặc tìm hiểu các tệp nguồn.

## Mở trình duyệt tiện ích

Đi tới **Trợ giúp → Lập trình → Trình duyệt tiện ích**.

Cửa sổ mở ngay lập tức; không cần tải trước plug-in.

## Nó hiển thị những gì

Trình duyệt liệt kê mọi thủ tục, biến và biểu mẫu cú pháp được xuất bởi bảy thư viện tiện ích mà Lumi tải tự động khi khởi động:

| Thư viện | Nó bao gồm những gì |
|---|---|
| `common.scm` | Trình trợ giúp có mục đích chung (chuỗi, số, danh sách tiện ích) |
| `files.scm` | Hàm trợ giúp tệp và đường dẫn |
| `gegl.scm` | Bộ đệm GEGL và trợ giúp màu |
| `images.scm` | Hàm trợ giúp ở cấp độ hình ảnh (`image-get-open-list`, v.v.) |
| `layers.scm` | Trình trợ giúp lớp và drawable |
| `parasites.scm` | Hàm trợ giúp đọc/ghi parasite |
| `paths.scm` | Hàm trợ giúp đường dẫn và vector |

Tất cả những thứ này đều có sẵn trong bất kỳ plug-in Scheme nào hoặc trong Scheme Console.

## Tìm kiếm và lọc

- **Hộp tìm kiếm**: lọc theo tên khi bạn nhập (khớp chuỗi con không phân biệt chữ hoa chữ thường).
- **Bộ lọc loại**: thu hẹp kết quả thành `procedure`, `variable` hoặc `syntax`.

Nhấp vào một mục sẽ hiển thị chuỗi tài liệu đầy đủ của nó và thư viện chứa nó.

## Stdlib dưới dạng trình bao bọc

Các thư viện tiện ích là một ứng dụng thực tế của mẫu gói: mỗi trình trợ giúp đặt tên rõ ràng cho một hoạt động cấp thấp, ẩn bản soạn sẵn và cung cấp một nơi duy nhất để cập nhật nếu lệnh cơ bản thay đổi. Nếu bạn muốn hiểu phương pháp thiết kế đằng sau chúng, hãy xem hướng dẫn **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})**.

## Mối quan hệ với trình duyệt thủ tục

Trình duyệt tiện ích tách biệt với **Bộ lọc → Script-Fu → Bảng điều khiển → Duyệt** (Trình duyệt quy trình). Trình duyệt quy trình liệt kê các thủ tục đã đăng ký PDB. Trình duyệt tiện ích liệt kê các định nghĩa của trình trợ giúp có chủ ý tồn tại *bên ngoài* PDB: chúng chỉ ở dạng Scheme và không có ràng buộc C.