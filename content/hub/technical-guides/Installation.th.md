---
title: "การติดตั้ง"
type: docs
---
คุณต้องมี Git สำหรับขั้นตอนการโคลนเริ่มต้นด้านล่าง หากยังไม่ได้ติดตั้ง Git ให้ติดตั้งก่อน (Debian/Ubuntu: `sudo apt install git`) หรือติดตาม: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (การตั้งค่าครั้งแรก)

สร้างไดเร็กทอรีสำหรับ Lumi และใช้ Git เพื่อโคลนซอร์สโค้ด

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) ติดตั้งการพึ่งพา (การตั้งค่าครั้งแรก)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) สร้าง Lumi (การตั้งค่าครั้งแรก)

การสร้างการตั้งค่าแบบเต็มครั้งแรก (ครั้งแรกหรือหลังการเปลี่ยนแปลงที่สำคัญ):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) เปิดตัว Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## ตัวเลือกเสริม: สร้างใหม่ / คอมไพล์

สร้างใหม่ตามปกติหลังจากการเปลี่ยนแปลงโค้ด:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

เส้นทางการคอมไพล์ด่วนเท่านั้น:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

สร้างส่วนประกอบแบบรวมเดี่ยว (แทนที่ `babl` ด้วย `gegl` หรือ `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## ตัวเลือกเสริม: ประเภทบิลด์

ใช้ `--type` เมื่อจำเป็น:

- `debug` – การแก้ไขจุดบกพร่องเวิร์กโฟลว์
- `debugoptimized` – ค่าเริ่มต้นที่สมดุลสำหรับการพัฒนา
- `release` – รันไทม์ที่เร็วที่สุด

ตัวอย่าง:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```