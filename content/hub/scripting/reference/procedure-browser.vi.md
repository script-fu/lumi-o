---
title: "Trình duyệt quy trình"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: f2593585be79d09f94dee166e7003ceddc532b4d2f7c1060222fe5f5c758ef27
url: "hub/scripting/reference/procedure-browser"
translation_lock: true
---
Trình duyệt quy trình là công cụ tham khảo chính để khám phá hàng trăm chức năng có sẵn trong Cơ sở dữ liệu thủ tục (PDB) của Lumi. Bởi vì mọi công cụ, bộ lọc và tập lệnh trong Lumi phải được đăng ký trong PDB để có thể gọi được nên trình duyệt này thực sự là một trình khám phá PDB hoàn chỉnh.

## Mở trình duyệt thủ tục

Đi tới **Trợ giúp → Lập trình → Trình duyệt quy trình**.

Bạn cũng có thể truy cập nó từ Bảng điều khiển Scheme thông qua **Duyệt**.

## Nó hiển thị những gì

Trình duyệt quy trình có thể liệt kê tất cả các thủ tục hiện được đăng ký trong PDB, bất kể nguồn gốc của chúng. Nó mặc định tìm kiếm "nội bộ", để hiển thị các thủ tục cốt lõi đã đăng ký nội bộ.

- **Quy trình nội bộ**: Các chức năng cốt lõi để xử lý hình ảnh, quản lý lớp và kiểm soát công cụ.
- **Plugin bên ngoài**: Các quy trình được cung cấp bởi các plug-in C/C++ được biên dịch hoặc các tiện ích mở rộng liên tục.

## Tìm kiếm và lọc

- **Hộp tìm kiếm**: Lọc quy trình theo tên, mô tả hoặc tác giả. Xóa trường tìm kiếm sẽ hiển thị tất cả các thủ tục có sẵn.
- **Loại tìm kiếm**: Danh sách thả xuống tìm kiếm cho phép bạn lọc theo các trường cụ thể. Nếu bạn đặt thành **theo loại** và tìm kiếm "nội bộ", danh sách sẽ thu hẹp để chỉ hiển thị các quy trình cốt lõi đã đăng ký nội bộ.
- **Chế độ xem chi tiết**: Nhấp vào một quy trình sẽ hiển thị các tham số, giá trị trả về, tác giả, ngày tháng và mô tả về những gì nó thực hiện.

Điều này rất cần thiết để tìm tên chính xác và chữ ký đối số của hàm bạn muốn gọi từ tập lệnh của mình.