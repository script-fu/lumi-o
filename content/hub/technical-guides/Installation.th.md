---
title: "การติดตั้ง"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

คุณต้องมี Git สำหรับขั้นตอนการโคลนครั้งแรกด้านล่าง หากยังไม่ได้ติดตั้ง Git ให้ติดตั้งก่อน (Debian/Ubuntu: `sudo apt install git`) หรือดูที่: [ใช้ Git บน Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) โคลน Lumi (การตั้งค่าครั้งแรก)

สร้างไดเรกทอรีสำหรับ Lumi และใช้ Git เพื่อโคลนซอร์สโค้ด

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) ติดตั้ง dependencies (การตั้งค่าครั้งแรก)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) สร้าง Lumi (การตั้งค่าครั้งแรก)

การสร้างการตั้งค่าแบบเต็มครั้งแรก (ครั้งแรกหรือหลังการเปลี่ยนแปลงครั้งใหญ่):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) เปิด Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## ตัวเลือก: สร้างใหม่ / คอมไพล์

การสร้างใหม่ตามปกติหลังแก้ไขโค้ด:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

เส้นทางคอมไพล์อย่างเดียวแบบรวดเร็ว:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

สร้างคอมโพเนนต์แบบรวมเดี่ยว (แทนที่ `babl` ด้วย `gegl` หรือ `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## ตัวเลือก: ประเภท build

ใช้ `--type` เมื่อจำเป็น:

- `debug` – สำหรับเวิร์กโฟลว์การดีบัก
- `debugoptimized` – ค่าเริ่มต้นที่สมดุลสำหรับการพัฒนา
- `release` – runtime ที่เร็วที่สุด

ตัวอย่าง:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
