---
title: "กำลังโหลด"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
ทันทีที่ฟังก์ชันตัวช่วยเติบโตขึ้น ให้ย้ายมันไปไว้ในไฟล์ไลบรารีขนาดเล็ก นั่นทำให้ปลั๊กอินโฟกัสอยู่และทำให้ตัวช่วยสามารถนำมาใช้ซ้ำกับปลั๊กอินหลายตัวได้

### สร้างฟังก์ชั่นห้องสมุด

เราสามารถใช้ฟังก์ชันส่งข้อความและสร้างไฟล์ใหม่โดยมีเนื้อหานั้นเป็นเนื้อหาได้ บันทึกไฟล์ลงในโฟลเดอร์ repo ของคุณ ไม่ใช่ส่วนปลั๊กอิน ซึ่งอาจอยู่ใกล้ระดับบนสุด

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: นี่คือไดเร็กทอรีหลักสำหรับจัดเก็บโค้ด Scheme ของคุณ
  - **library/**: นี่คือที่ที่ฟังก์ชันที่ใช้ร่วมกัน เช่น `send-message.scm` ถ่ายทอดสด
  - **ปลั๊กอิน/**: นี่คือที่จัดเก็บปลั๊กอินส่วนบุคคลของคุณ
    - **hello-world/**: โฟลเดอร์สำหรับ plug-in "Hello World!"
      - **hello-world.scm**: ไฟล์สคริปต์ของ plug-in

ตัวอย่างของฟังก์ชันไลบรารี send-message.scm

```scheme
;; ฟังก์ชันจัดการการส่งข้อความไปยังปลายทางต่างๆ
(define (send-message message output)
  (cond
    ;; ส่งไปยัง คอนโซลข้อความ
    ((eq? output 'error-console)
       ;; ตั้งตัวจัดการเป็น คอนโซลข้อความ
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; ส่งไปยังกล่องโต้ตอบ GUI
    ((eq? output 'gui)
       ;; ตั้งตัวจัดการเป็น GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; ส่งไปยังหน้าต่างเทอร์มินัล
    ((eq? output 'terminal)
       ;; เอาต์พุต terminal จัดการด้วย display
       (display message)))

  ;; คืนค่าตัวจัดการข้อความเริ่มต้นไปยัง คอนโซลข้อความ
  (lumi-message-set-handler 2))
```

### โหลดฟังก์ชันไลบรารี

เราสามารถโหลดฟังก์ชันไลบรารีนั้นได้ด้วยคำสั่ง Scheme `load`

กำลังโหลดไฟล์ไลบรารี:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

เฮ้! ตอนนี้เรามีสิ่งที่ง่ายกว่าและสั้นกว่าในการอ่าน ซึ่งอธิบายตัวเองโดยไม่ต้องแสดงความคิดเห็น นี่คือข้อสรุปที่น่าพอใจของการปรับโครงสร้างใหม่