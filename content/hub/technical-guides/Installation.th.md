---
title: "การติดตั้ง"
type: docs
url: "hub/technical-guides/Installation"
---
คู่มือนี้ใช้สคริปต์บิลด์ Lumi ปัจจุบันใน:

`~/code/lumi-dev/build/lumi/scripts`

## 1) ติดตั้งการพึ่งพา (การตั้งค่าครั้งแรก)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) สร้างลูมิ

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) เปิดตัว Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## ประเภทการสร้าง

ใช้ `--type` เมื่อจำเป็น:

- `debug` – การแก้ไขจุดบกพร่องเวิร์กโฟลว์
- `debugoptimized` – ค่าเริ่มต้นที่สมดุลสำหรับการพัฒนา
- `release` – รันไทม์ที่เร็วที่สุด

ตัวอย่าง:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```