---
title: "ดาวน์โหลดและติดตั้ง"
type: docs
url: "hub/quick-start/Download-and-Install"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5f17d7e9009aeeacf256152bef94386ccc5a8eea87cf0feebef073488fb59283
---
หากคุณใช้ Linux อยู่แล้วและต้องการเรียกใช้ Lumi อย่างรวดเร็ว ให้ใช้ **AppImage สำหรับพัฒนา** ล่าสุดจากอาร์ติแฟกต์ GitLab:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. ดาวน์โหลด zip สิ่งประดิษฐ์ AppImage การพัฒนาล่าสุด
2. แตกซิปออก
3. ดับเบิลคลิกไฟล์ `Lumi*.AppImage` เพื่อเรียกใช้

AppImage ควรใช้งานได้แล้ว หากไม่เป็นเช่นนั้น ให้เปิดใช้งาน **อนุญาตให้เรียกใช้ไฟล์ในรูปแบบโปรแกรม** ในการอนุญาตของไฟล์ หรือใช้วิธีเทอร์มินัลด้านล่าง

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```