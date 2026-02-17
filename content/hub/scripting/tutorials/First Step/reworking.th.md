---
title: "การทำงานซ้ำ"
type: docs
weight: 7
---
ขั้นตอนนี้แก้ไขลักษณะการทำงานที่ละเอียดอ่อนในตัวอย่างการรับส่งข้อความ

เรากำลังส่งสตริง "Hello world!\n" เป็นข้อความ "\n" เป็นอักขระชนิดพิเศษ ซึ่งเป็นอักขระ "escape" มันบอกให้การพิมพ์เอาท์พุตเริ่มต้นการขึ้นบรรทัดใหม่ ใน Scheme มันจะบังคับให้ข้อความที่ส่งไปยังแถบสถานะปรากฏขึ้นเป็นกล่อง GUI

ตัวช่วย `send-to-gui` ส่งข้อความไปยังกล่องโต้ตอบ Lumi

อัปเดตเนื้อหาและปลายทางของข้อความเพื่อให้ตัวอย่างทำงานสอดคล้องกัน

การลบอักขระหลีกและขยายฟังก์ชัน:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

แทนที่ตัวเลขมหัศจรรย์ด้วยค่าคงที่ที่ Lumi ให้ไว้ (เช่น `MESSAGE-BOX` และ `ERROR-CONSOLE`)

จากนั้นแบ่งการตรวจสอบออกเป็นสองฟังก์ชันเพื่อให้สามารถนำมาใช้ซ้ำได้จากไซต์การโทรหลายแห่ง

- (is-valid-string?) หากต้องการตรวจสอบว่าสตริงเป็นสตริงและไม่ใช่สตริงว่าง ภายในฟังก์ชัน send-to*
- (is-valid-output-display?) ในการตรวจสอบปลายทางเอาต์พุตที่กำหนดนั้นถูกต้องในฟังก์ชันส่งข้อความ

ปรับปรุงห้องสมุด:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## บทสรุป

ด้วยการปรับปรุงไลบรารีการรับส่งข้อความของเรา เราได้ทำให้ไลบรารี่มีประสิทธิภาพและเชื่อถือได้มากขึ้น เราแก้ไขปัญหาที่ซ่อนอยู่ด้วยอักขระขึ้นบรรทัดใหม่ แนะนำค่าคงที่เพื่อความชัดเจนที่ดีขึ้น และขยายฟังก์ชันการทำงานโดยเพิ่มการรองรับแถบสถานะและเอาต์พุตของกล่องโต้ตอบ นอกจากนี้ การแยกตรรกะการตรวจสอบออกเป็นฟังก์ชันที่เน้นและเล็กลงทำให้มั่นใจได้ว่าโค้ดของเราง่ายต่อการบำรุงรักษาและขยายในอนาคต

การปรับปรุงใหม่นี้แสดงให้เห็นว่าการเปลี่ยนแปลงเล็กๆ น้อยๆ สามารถปรับปรุงโครงสร้างโดยรวมและฟังก์ชันการทำงานของห้องสมุดของเราได้อย่างไร ซึ่งปูทางไปสู่ความยืดหยุ่นและการนำกลับมาใช้ใหม่ได้มากขึ้นเมื่อโครงการของเราเติบโตขึ้น