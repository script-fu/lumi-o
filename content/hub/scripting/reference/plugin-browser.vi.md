---
title: "Trình duyệt plug-in"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ffbf087ea102e00b7057bf6bad9b6e2cb8f75ad05c7f26f0f2818d10f34392ce
url: "hub/scripting/reference/plugin-browser"
translation_lock: true
---
Trình duyệt Plug-In cho phép bạn khám phá hệ thống menu và xem nơi cài đặt các plug-in cụ thể.

## Mở trình duyệt plug-in

Đi tới **Trợ giúp → Lập trình → Trình duyệt plug-in**.

## Nó hiển thị những gì

Trong khi Trình duyệt quy trình tập trung vào các *chức năng* thô trong PDB thì Trình duyệt plug-in là chế độ xem tập hợp con tập trung vào việc khám phá giao diện người dùng. Nó đặc biệt lọc PDB để hiển thị "những thứ trông giống như các plugin được cài đặt trong menu".

Trong nội bộ, điều này sử dụng một truy vấn chỉ trả về các thủ tục có cả tệp được liên kết trên đĩa và đường dẫn menu đã đăng ký.

- **Cây menu**: Hiển thị hình đại diện dạng cây của cấu trúc menu Lumi.
- **Vị trí plug-in**: Giúp bạn tìm vị trí của plug-in mới được cài đặt trong các menu.
- **Siêu dữ liệu**: Hiển thị thông tin về tác giả, phiên bản và ngày của plugin.

## Cách sử dụng

Sử dụng Trình duyệt plug-in khi bạn biết một tính năng tồn tại nhưng không thể tìm thấy nó trong menu hoặc khi bạn đang thiết kế plug-in của riêng mình và muốn xem các công cụ tương tự nằm ở đâu.