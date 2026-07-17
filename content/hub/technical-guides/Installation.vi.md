---
title: "Cài đặt"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
url: "hub/technical-guides/Installation"
translation_lock: true
---
Bạn cần Git cho bước sao chép ban đầu bên dưới. Nếu Git chưa được cài đặt, hãy cài đặt nó trước (Debian/Ubuntu: `sudo apt install git`) hoặc làm theo: [Sử dụng Git trên Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (thiết lập lần đầu)

Tạo thư mục cho Lumi và sử dụng Git để sao chép mã nguồn.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone qua SSH (khớp hướng dẫn Git ở trên)

git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Hoặc clone qua HTTPS (không cần thiết lập khóa SSH)

# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev

```

## 2) Cài đặt phụ thuộc (thiết lập lần đầu)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Biên dịch Lumi (thiết lập lần đầu)

Bản dựng thiết lập đầy đủ đầu tiên (lần đầu tiên hoặc sau những thay đổi lớn):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Khởi chạy Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Tùy chọn: Xây dựng lại/Biên dịch

Xây dựng lại bình thường sau khi thay đổi mã:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Đường dẫn chỉ biên dịch nhanh:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Xây dựng một thành phần tích hợp duy nhất (thay thế `babl` bằng `gegl` hoặc `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Tùy chọn: Loại bản dựng

Sử dụng `--type` khi cần:

- `debug` – quy trình gỡ lỗi
- `debugoptimized` – mặc định cân bằng để phát triển
- `release` – thời gian chạy nhanh nhất

Ví dụ:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```