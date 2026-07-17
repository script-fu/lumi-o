---
title: "Cài đặt Debian"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
url: "hub/install-linux/Installing-Debian"
translation_lock: true
---
Tài liệu này phác thảo quy trình được sử dụng để cài đặt Debian Stable làm hệ điều hành phát triển Lumi-o. Nó có thể hữu ích cho những người khác đang thiết lập một môi trường tương tự.

Debian Stable được chọn vì Lumi hướng tới biên dịch đáng tin cậy trên nền tảng dài hạn, có thể dự đoán. Phát triển GIMP nhắm tới Debian Testing, nên Debian Stable là hệ nền gần nhất.

Lumi hoạt động tốt nhất trên Debian với Cinnamon (X11) và được phát triển và thử nghiệm trong môi trường đó. Cinnamon cung cấp quy trình làm việc trên máy tính để bàn quen thuộc giống Windows, trong khi X11 cung cấp môi trường ổn định nhất để phát triển Lumi.

Nếu bạn đến từ Windows, thay đổi khái niệm chính là hầu hết việc cài đặt và cấu hình phần mềm diễn ra thông qua trình quản lý gói và các lệnh đầu cuối đơn giản thay vì trình cài đặt có thể tải xuống.

## Hướng dẫn này dành cho ai

Hướng dẫn này ghi lại quá trình thiết lập Debian Stable đang hoạt động được sử dụng để phát triển Lumi. Đây không phải là hướng dẫn cài đặt Linux chung.

Nó hữu ích nhất cho:

- những nghệ sĩ chuyển từ Windows muốn có một thiết lập Linux có thể dự đoán được
- nhà phát triển xây dựng Lumi từ nguồn
- người dùng thích tái tạo môi trường làm việc đã biết hơn là thiết kế cấu hình hệ thống của riêng họ

Giả sử có sự quen thuộc cơ bản với việc phân vùng đĩa và cách sử dụng dòng lệnh đơn giản.

## Sao lưu dữ liệu của bạn

Trước khi cài đặt Debian, hãy tạo một bản sao lưu hoàn chỉnh cho thư mục Home của bạn trên ổ đĩa ngoài. Bao gồm mọi thư mục dữ liệu bổ sung mà bạn muốn bảo tồn.

Lưu ý: Trong Linux, `~` đại diện cho thư mục Home của bạn.

Nếu bạn dùng kho Git, hãy push mọi thay đổi quan trọng lên remote tương ứng để dễ khôi phục sau khi cài đặt. Bước này chỉ áp dụng nếu bạn đã dùng Git.

## Tạo phân vùng

Tạo dung lượng trên ổ đĩa chính cho Debian. Có nhiều hướng dẫn và công cụ cho bước này, bao gồm cả GParted. Tùy thuộc vào thiết lập của bạn, bạn có thể:

- thu nhỏ phân vùng Windows hiện có để khởi động kép
- sử dụng lại phân vùng Linux hiện có
- chuẩn bị Linux mới và phân vùng trao đổi

Nếu bạn không chắc chắn, hãy tham khảo hướng dẫn dành riêng cho phần cứng trước khi thực hiện thay đổi, vì các bước phân vùng khác nhau đáng kể giữa các hệ thống.


## Tạo USB cài đặt Debian

Giả sử phân vùng đích và không gian trao đổi đã tồn tại:

1. Tải xuống Debian ISO từ trang web chính thức: https://www.debian.org/
2. Trên Windows, sử dụng BalenaEtcher để ghi ISO vào ổ USB.
3. Trên Linux, sử dụng công cụ dòng lệnh như `dd` để tạo USB có khả năng khởi động.

## Cài đặt Debian

1. Cắm ổ USB vào.
2. Khởi động lại và nhấn phím menu khởi động (thường là `F2`, `F12`, `Esc` hoặc `Del`) trong khi khởi động.
3. Chọn thiết bị USB.
4. Chọn trình cài đặt không có đồ họa.
5. Để trống mật khẩu gốc khi được nhắc để trình cài đặt cấp quyền truy cập sudo vào tài khoản người dùng của bạn.
6. Phân vùng thủ công:

   - Hệ thống tệp: ext4 (có ghi nhật ký)
   - Hoán đổi: phân vùng trao đổi hiện có
   - Điểm gắn kết: `/`
   - Nhãn: `linux`
   - Tên máy chủ: tên hệ thống hiển thị là `user@hostname`
   - Tài khoản người dùng: tên đầy đủ của bạn
   - Tên người dùng: tên đăng nhập thiết bị đầu cuối

7. Trình cài đặt Debian cung cấp lựa chọn môi trường máy tính để bàn ở giai đoạn này; chọn **Cinnamon** để có cài đặt được Lumi khuyên dùng.
8. Hoàn tất cài đặt và khởi động lại vào Debian Stable.

## Thiết lập hệ thống

### Chia tỷ lệ hiển thị

Debian Stable hiện xử lý tỷ lệ phân số không nhất quán, đặc biệt trên màn hình 4K. Thay vì giảm độ phân giải, hãy điều chỉnh trực tiếp các thành phần giao diện.

Điều chỉnh đề xuất:

- Tránh chia tỷ lệ hiển thị phân số.
- Menu → Chọn phông chữ → Cài đặt phông chữ → Hệ số tỷ lệ văn bản: `2.5`
- Phông chữ máy tính để bàn: `14`
- Bảng điều khiển → Tùy chỉnh → Chiều cao bảng điều khiển: `60`
- Giao diện bảng điều khiển → Kích thước biểu tượng symbolic vùng bên phải: `48px`
- Chuột và Touchpad → Điều chỉnh kích thước con trỏ
- Desktop (chuột phải) → Tùy chỉnh → Kích thước biểu tượng lớn hơn

Điều chỉnh Firefox:

- Thanh địa chỉ → `about:config`
- Đặt `layout.css.devPixelsPerPx` thành `1`

### Terminal

Cấu hình tùy chọn terminal:

1. Menu → Terminal → Chỉnh sửa → Tùy chọn
2. Văn bản → Kích thước ban đầu: `140 columns`, `40 rows`
3. Văn bản → Phông chữ tùy chỉnh: `Monospace 10`
4. Màu sắc → Đề án tích hợp → Tối năng lượng mặt trời

## Khôi phục dữ liệu

Khôi phục các tệp đã sao lưu vào thư mục Home nếu cần, ví dụ:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Lưu ý: Các thư mục bắt đầu bằng `.` là các thư mục cấu hình ẩn trong Linux.

## Tùy chọn: Thiết lập Git

Chỉ bắt buộc nếu bạn có kế hoạch xây dựng Lumi hoặc khôi phục kho lưu trữ.

### Cài đặt Git

```bash
sudo apt install git
```

Định cấu hình danh tính của bạn:

```bash
git config --global --edit
```

#### Truy cập GitLab

Khôi phục quyền truy cập kho lưu trữ vào GitLab hoặc GitHub:

1. Đổi quyền tệp khóa SSH: `chmod 600 ~/.ssh/id_rsa`
2. Thêm khóa vào phiên SSH agent: `ssh-add ~/.ssh/id_rsa`
3. Kiểm tra kết nối: `ssh -T git@ssh.gitlab.gnome.org` hoặc `ssh -T git@github.com`

Đối với mỗi kho, hãy fetch remote và reset nhánh cục bộ cho khớp:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Chạy `git status` để xác nhận kho lưu trữ sạch sẽ.

Bạn đã có hệ điều hành mới với dữ liệu và kho được khôi phục. Thiết lập này phản ánh môi trường làm việc đã biết dùng cho phát triển Lumi và có thể điều chỉnh theo quy trình riêng nếu cần.

## Biên dịch Lumi sau khi cài đặt hệ điều hành

Tập lệnh biên dịch Lumi nằm tại:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Cài phụ thuộc một lần

sudo bash lumi-install-packages.sh

# Thiết lập và biên dịch lần đầu

bash lumi-build-script.sh --scope setup --dir lumi-dev

# Biên dịch lại sau khi đổi mã

bash lumi-build-script.sh --scope build --dir lumi-dev

# Chỉ biên dịch nhanh

bash lumi-build-script.sh --scope compile --dir lumi-dev

# Khởi chạy Lumi

bash lumi-launch-active.sh lumi-dev
```