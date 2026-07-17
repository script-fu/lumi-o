---
title: "กระบวนการแบทช์"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
ตัวอย่างที่ใช้งานได้จริงตั้งแต่ต้นจนจบสำหรับการประมวลผลไฟล์จำนวนมากในคราวเดียว

## ซอร์สโค้ด

- [ดูซอร์สโค้ด](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## เมนูใน Lumi

- **ไฟล์ → กระบวนการแบทช์**

## สิ่งที่แสดง

- `SF-DIRNAME` พารามิเตอร์สำหรับไดเรกทอรีต้นทาง/ปลายทาง
- การตรวจสอบเส้นทาง GUI ด้วยทางเลือกสำรอง (`validate-path-and-dir`)
- การสแกนและการวนซ้ำไดเรกทอรีแบบเรียกซ้ำ
- รายงานความคืบหน้าการดำเนินงานระยะยาว
