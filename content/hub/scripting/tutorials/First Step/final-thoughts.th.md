---
title: "ความคิดสุดท้าย"
type: docs
weight: 10
---
ขณะนี้คุณมีปลั๊กอินขั้นตอนการทำงานและไลบรารีตัวช่วยขนาดเล็กแล้ว ซีรีส์นี้แนะนำรูปแบบหลักที่คุณจะใช้ในสคริปต์ Lumi ส่วนใหญ่:

- ฟังก์ชั่น: องค์ประกอบพื้นฐานของปลั๊กอินของเรา
- การปรับโครงสร้างใหม่: ปรับปรุงโครงสร้างโค้ดโดยยังคงรักษาฟังก์ชันการทำงานไว้
- Code Libraries: การรวมศูนย์ฟังก์ชันที่ใช้ซ้ำได้เพื่อรักษาโค้ดของเราให้สะอาดและเป็นโมดูล
- เทคนิคการตรวจสอบความถูกต้อง: ตรวจสอบให้แน่ใจว่าอินพุตนั้นถูกต้องก่อนที่จะดำเนินการตรรกะหลักของเรา

คุณยังได้เห็นพื้นฐานของการใช้ Git เพื่อติดตามการเปลี่ยนแปลงและรักษาโครงสร้างโปรเจ็กต์ให้สะอาด เวิร์กโฟลว์นั้นช่วยให้ทำซ้ำได้ง่ายขึ้นโดยไม่สูญเสียเวอร์ชันการทำงาน

นี่คือเวอร์ชันสุดท้ายของโค้ดปลั๊กอินหลักของเรา:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

รหัสห้องสมุด:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## บทสรุป

ด้วยการปรับโครงสร้างตัวช่วยการรับส่งข้อความใหม่ให้เป็นไลบรารีขนาดเล็ก ปลั๊กอินจะยังคงเน้นไปที่จุดประสงค์ และไลบรารีจะมีรายละเอียดการใช้งาน การตรวจสอบความถูกต้องและการกำหนดเส้นทางข้อความที่สอดคล้องกันทำให้สามารถคาดเดาความล้มเหลวได้

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

ขั้นตอนถัดไป:

- ย้ายตัวช่วยที่ใช้ซ้ำได้ไปยังไฟล์ไลบรารีเฉพาะ
- ทำให้ปลั๊กอินมีขนาดเล็กและตั้งชื่อขั้นตอนสำหรับสิ่งที่พวกเขาทำ
- เพิ่มการตรวจสอบความถูกต้องที่ขอบเขต (อินพุต, เส้นทางไฟล์, ตัวเลือกเมนู)

เก็บผลลัพธ์สุดท้ายเป็นสองไฟล์ใน repo ปลั๊กอินของคุณ:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`