---
title: "ห้องสมุดข้อความ"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bfe459b9f717201d646bde29196fd66e6c3b19b3e9dbdb3338e0c853153e1c05
---
เมื่อเวลาผ่านไป สิ่งที่เริ่มต้นจากการเป็นฟังก์ชันเดียวในการส่งข้อความได้พัฒนาไปสู่ชุดของฟังก์ชันที่เกี่ยวข้องกัน These functions now form the foundation of a **Messaging Library**, designed to handle output to different destinations, such as the GUI, Message console, and OS terminal.

### ทำไมต้องมีไลบรารีข้อความ?

เมื่อความต้องการของเราเพิ่มมากขึ้น การจัดการข้อความในเอาต์พุตหลายรายการจำเป็นต้องมีแนวทางแบบแยกส่วนและขยายได้มากขึ้น แทนที่จะใช้ฟังก์ชันเดียวทำทุกอย่าง เราได้แบ่งกระบวนการออกเป็นส่วนประกอบที่นำมาใช้ซ้ำได้ เพื่อให้เกิดความยืดหยุ่นมากขึ้น ขณะนี้ไลบรารีนี้สามารถใช้เป็นเครื่องมือส่งข้อความทั่วไปที่ปลั๊กอินหรือฟังก์ชันอื่นสามารถยืมได้

### What Does the Messaging Library Do?

ปัจจุบันไลบรารีข้อความมีฟังก์ชันดังต่อไปนี้:

- **send-to-gui**: ส่งข้อความไปยังกล่องโต้ตอบ Lumi GUI
- **send-to-error-console**: ส่งข้อความไปยังคอนโซล Lumi Message
- **send-to-terminal**: Sends messages to the terminal window.
- **ส่งข้อความ**: ฟังก์ชันโปรแกรมเลือกจ่ายงานที่จะส่งข้อความไปยังเอาต์พุตที่เหมาะสม
- **validate-message**: ตรวจสอบให้แน่ใจว่าข้อความและเอาต์พุตนั้นถูกต้องก่อนส่ง

### การขยายห้องสมุด

**ไลบรารีข้อความ** สามารถขยายเพื่อรองรับเอาต์พุตเพิ่มเติมได้อย่างง่ายดาย ตัวอย่างเช่น:

- **ส่งไปยังไฟล์**: บันทึกข้อความลงในไฟล์บันทึก
- **ส่งไปยังคนตัดไม้**: ผสานรวมกับระบบบันทึกภายนอก
- **ส่งไปยังการแจ้งเตือน**: แสดงข้อความเป็นการแจ้งเตือนของระบบ

ด้วยการออกแบบโมดูลาร์และฟังก์ชันที่นำกลับมาใช้ใหม่ในรูปแบบเดียวกัน ไลบรารีนี้สามารถเติบโตเป็นเครื่องมือที่ครอบคลุมสำหรับการจัดการงานการรับส่งข้อความทุกประเภท

## ประโยชน์ของไลบรารีข้อความ

- **การนำกลับมาใช้ใหม่ได้**: ฟังก์ชันต่างๆ สามารถนำมาใช้ซ้ำกับปลั๊กอินหรือโปรเจ็กต์ต่างๆ ได้
- **โมดูลาร์**: แต่ละฟังก์ชันจะจัดการงานเฉพาะงานเดียว ทำให้ง่ายต่อการบำรุงรักษาและขยายโค้ด
- **Consistency**: Using the same validation and message-handling functions ensures consistent behavior across the application.

**ไลบรารีการส่งข้อความ** เป็นจุดเริ่มต้นของเฟรมเวิร์กที่กว้างขึ้นซึ่งอาจทำให้วิธีจัดการข้อความในโปรเจ็กต์ของคุณง่ายขึ้น เมื่อไลบรารีเติบโตขึ้น ปลั๊กอินใหม่ๆ ก็สามารถใช้ประโยชน์จากไลบรารีเพื่อส่งข้อความได้ทุกที่ที่ต้องการ

เราสามารถปรับโครงสร้างไฟล์ได้:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

และอย่าลืมปรับ `load` ในปลั๊กอินหลัก:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```