---
title: "AppImage"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
url: "hub/technical-guides/AppImage"
translation_lock: true
---
AppImage là gói ứng dụng Linux chỉ gồm một tệp. Bạn tải tệp đó, đánh dấu có thể thực thi và chạy mà không cần cài phần mềm toàn hệ thống.

Trang AppImage chính thức: https://appimage.org/

AppImage cung cấp phiên bản di động của Lumi chạy mà không cần cài đặt hay sửa hệ thống. Phù hợp với nghệ sĩ muốn dùng phần mềm ngay mà không quản lý phụ thuộc, biên dịch mã nguồn hay cấu hình môi trường phát triển.

Là tệp thực thi tự chứa, AppImage có thể đặt ở bất kỳ đâu trên hệ thống. Điều này giúp dễ thử bản phát hành mới, giữ nhiều phiên bản hoặc chuyển phần mềm giữa các máy.

Trong quy trình phát triển Lumi, AppImage hoạt động như bản build thử nghiệm di động, khớp sát với đầu ra CI. Nhờ vậy bạn có thể thử nghiệm đáng tin cậy trong môi trường nhất quán, trong khi build từ nguồn cục bộ vẫn tập trung vào phát triển.

Lưu ý: CI build AppImage bằng nguồn phụ thuộc tích hợp trong kho Lumi (BABL/GEGL/GTK3), nên ngăn xếp phụ thuộc nhất quán với quy trình `lumi-build-script.sh` cục bộ.

## AppImage phát hành và phát triển

- **AppImage phát hành**: chưa có (Lumi chưa phát hành chính thức).
- **AppImage phát triển (artifact CI)**: được tạo tự động từ các commit phát triển để thử nghiệm.

Hướng dẫn này chủ yếu mô tả quy trình **AppImage phát triển**.

Trang artifact hiện tại:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Cơ bản về tải AppImage CI

CI tạo các tệp zip artifact (ví dụ `lumi-appimage*.zip`).

Quy trình thủ công cơ bản:

1. Tải gói zip artifact CI mới nhất.
2. Giải nén.
3. Chạy tệp `Lumi*.AppImage` đi kèm.

Các tập lệnh bên dưới là trợ giúp tùy chọn để tự động hóa các bước này.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Giải nén zip CI mới nhất từ ~/Downloads

bash lumi-appimage-unpack-zip.sh

# Khởi chạy AppImage với đầu ra terminal

bash lumi-appimage-launch.sh
```

## Tập lệnh trợ giúp tùy chọn

- `lumi-appimage-unpack-zip.sh`
  - tìm `lumi-appimage*.zip` mới nhất trong `~/Downloads`
  - đặt AppImage vào `~/AppImage/Lumi/Lumi_CI.AppImage`
  - cài tài nguyên desktop vào `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - khởi chạy AppImage trong terminal
  - bật đầu ra runtime (`APPIMAGE_DEBUG=1`)

## Ghi chú chung

- Nếu chạy AppImage thủ công (không qua tập lệnh trợ giúp), trước tiên hãy cấp quyền thực thi:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` đã tự áp dụng quyền thực thi.

- Nếu Lumi đang chạy từ build khác, hãy đóng trước khi khởi chạy AppImage.
