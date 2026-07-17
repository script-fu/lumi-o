---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

AppImage เป็นแพ็กเกจแอปพลิเคชัน Linux ไฟล์เดียว คุณดาวน์โหลดไฟล์เดียว ทำให้เรียกใช้งานได้ แล้วรันโดยไม่ต้องติดตั้งซอฟต์แวร์ทั้งระบบ

เว็บไซต์ AppImage อย่างเป็นทางการ: https://appimage.org/

AppImage มี Lumi เวอร์ชันพกพาที่ทำงานได้โดยไม่ต้องติดตั้งหรือแก้ไขระบบ เหมาะสำหรับศิลปินที่ต้องการใช้ซอฟต์แวร์ทันทีโดยไม่ต้องจัดการ dependencies คอมไพล์ซอร์สโค้ด หรือกำหนดค่าสภาพแวดล้อมการพัฒนา

ในฐานะไฟล์ปฏิบัติการที่พกพาได้ AppImage สามารถเก็บไว้ที่ใดก็ได้ในระบบ ทำให้ทดสอบรีลีสใหม่ เก็บหลายเวอร์ชัน หรือย้ายซอฟต์แวร์ระหว่างเครื่องได้ง่าย

ในกระบวนการพัฒนาของ Lumi AppImage ทำหน้าที่เป็น build ทดสอบแบบพกพาที่ใกล้เคียงกับผลลัพธ์ CI ช่วยให้ทดสอบได้อย่างน่าเชื่อถือในสภาพแวดล้อมที่สอดคล้องกัน ในขณะที่ build จากซอร์สในเครื่องยังมุ่งเน้นงานพัฒนา

หมายเหตุ: CI สร้าง AppImage โดยใช้แหล่ง dependency แบบรวมใน repo ของ Lumi (BABL/GEGL/GTK3) ดังนั้น dependency stack จึงสอดคล้องกับเวิร์กโฟลว์ `lumi-build-script.sh` ในเครื่อง

## AppImage แบบ release กับ development

- **Release AppImage**: ยังไม่พร้อมใช้งาน (Lumi ยังไม่เปิดตัว)
- **Development AppImage (CI artifact)**: สร้างโดยอัตโนมัติจาก commit การพัฒนาปัจจุบันเพื่อการทดสอบ

คู่มือนี้ครอบคลุมเวิร์กโฟลว์ **development AppImage** เป็นหลัก

หน้า artifact ปัจจุบัน:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## พื้นฐานการดาวน์โหลด AppImage จาก CI

CI สร้างไฟล์ zip ของ artifact (เช่น `lumi-appimage*.zip`)

ขั้นตอนด้วยตนเองพื้นฐาน:

1. ดาวน์โหลด zip artifact ของ CI ล่าสุด
2. แตกไฟล์
3. รันไฟล์ `Lumi*.AppImage` ที่รวมมา

สคริปต์ด้านล่างเป็นตัวช่วยเสริมที่ทำขั้นตอนเหล่านี้ให้อัตโนมัติ

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## สคริปต์ตัวช่วยเสริม

- `lumi-appimage-unpack-zip.sh`
  - ค้นหา `lumi-appimage*.zip` ล่าสุดใน `~/Downloads`
  - ติดตั้ง AppImage ไปที่ `~/AppImage/Lumi/Lumi_CI.AppImage`
  - ติดตั้งทรัพยากร desktop ไปที่ `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - เปิด AppImage ในเทอร์มินัล
  - เปิดใช้งาน runtime output (`APPIMAGE_DEBUG=1`)

## หมายเหตุทั่วไป

- หากคุณรัน AppImage ด้วยตนเอง (ไม่มีสคริปต์ตัวช่วย) ให้ทำให้เรียกใช้งานได้ก่อน:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` ตั้งสิทธิ์เรียกใช้งานให้อัตโนมัติแล้ว

- หาก Lumi กำลังทำงานจาก build อื่นอยู่แล้ว ให้ปิดก่อนเปิด AppImage
