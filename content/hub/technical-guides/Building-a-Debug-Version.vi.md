---
title: "Xây dựng phiên bản gỡ lỗi"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
url: "hub/technical-guides/Building-a-Debug-Version"
translation_lock: true
---
Hướng dẫn này mô tả **quy trình gỡ lỗi cục bộ** cho Lumi bằng cách sử dụng tập lệnh trong `build/lumi/scripts`.

Quy trình làm việc được thiết kế để:

- sử dụng artifact build cục bộ (không cần tải symbol),
- xác minh symbol gỡ lỗi thực sự có mặt,
- khởi chạy GDB với chế độ symbol ngoại tuyến theo mặc định.

## Điều kiện tiên quyết

- Linux dựa trên Debian (dự án cơ sở: Debian 13)
- Cây nguồn Lumi đã được nhân bản

## Thiết lập GDB một lần (Tùy chọn nhưng được khuyến nghị)

Cài đặt công cụ GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Thiết lập ghi nhật ký cục bộ tùy chọn:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Lưu ý: Tập lệnh gỡ lỗi cục bộ của Lumi tắt `debuginfod` theo mặc định để giữ phân giải symbol cục bộ và có thể tái tạo.

## Bắt đầu nhanh

Từ thư mục script:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Biên dịch gỡ lỗi và khởi chạy (mặc định)

Sử dụng điều này cho các phiên gỡ lỗi thông thường.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Lệnh này:

1. biên dịch Lumi ở chế độ debug,
2. xác minh symbol gỡ lỗi,
3. khởi chạy Lumi dưới GDB.

### Chỉ biên dịch gỡ lỗi (cho phiên TTY/từ xa sau)

Sử dụng điều này khi bạn muốn xây dựng ngay bây giờ và khởi chạy/gỡ lỗi sau.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Sử dụng TTY trong Linux

TTY (bảng điều khiển văn bản) thường là cách đáng tin cậy nhất để gỡ lỗi treo cứng.

- Chuyển sang TTY với `Ctrl + Alt + F1` thông qua `Ctrl + Alt + F6`
- Đăng nhập từ dấu nhắc văn bản
- Quay lại phiên đồ họa với `Ctrl + Alt + F7` (hoặc `F2` trên một số hệ thống)

Tại sao quan trọng: nếu phiên desktop bị treo, TTY thường vẫn phản hồi, nên bạn có thể gắn GDB, ghi backtrace và thu thập dữ liệu sự cố hữu ích.

## Tùy chọn: Gỡ lỗi từ xa/TTY

Để treo cứng hoặc khóa màn hình, hãy sử dụng `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Sau đó, từ TTY (được khuyến nghị cho các trường hợp đóng băng) hoặc thiết bị đầu cuối khác:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Để khởi chạy GDB cục bộ (đường dẫn không phải TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Ghi chú về hiệu suất

Bản dựng gỡ lỗi chậm hơn theo thiết kế. Khi bạn gỡ lỗi xong, hãy quay lại bản dựng nhanh hơn:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Đặt lại release đầy đủ cho mọi thành phần chính

bash lumi-debug-reset-release.sh lumi-dev

# Biến thể cục bộ nhanh hơn (tùy chọn)

bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```