---
title: "รีแฟคเตอร์อีกครั้ง"
type: docs
weight: 5
---
เมื่อไลบรารีตัวช่วยเติบโตขึ้น การดูอย่างรวดเร็วจะติดตามได้ยากขึ้น ปรับโครงสร้างใหม่อีกครั้งเพื่อให้แต่ละฟังก์ชันมีขนาดเล็กและมีวัตถุประสงค์เดียว

### ทำลายความซับซ้อน

เพื่อให้ติดตามและบำรุงรักษาฟังก์ชันได้ง่ายขึ้น ให้แบ่งฟังก์ชันออกเป็นฟังก์ชันที่เน้นและมีขนาดเล็กลง เริ่มต้นด้วยการแยกการตรวจสอบความถูกต้องออกจากการกำหนดเส้นทางข้อความ

### สร้างฟังก์ชันการตรวจสอบความถูกต้อง

เราสามารถใช้ส่วนหนึ่งของฟังก์ชันที่ตรวจสอบอาร์กิวเมนต์ `message` และ `output` และย้ายไปยังฟังก์ชันที่แยกจากกัน ด้วยวิธีนี้ ฟังก์ชันหลัก `send-message` ไม่จำเป็นต้องกังวลเกี่ยวกับการตรวจสอบความถูกต้อง ทำให้ง่ายต่อการติดตาม

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### ลดความซับซ้อนในการส่งข้อความ

เมื่อการตรวจสอบถูกย้ายไปยังฟังก์ชันที่แยกจากกัน ฟังก์ชัน `send-message` จึงสามารถมุ่งเน้นไปที่การส่งข้อความเท่านั้น มันจะง่ายกว่ามากเนื่องจากจะจัดการเฉพาะงานเฉพาะในการส่งข้อความไปยังปลายทางที่ถูกต้องเท่านั้น

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### พังทลายเพิ่มเติม: แยกตัวจัดการเอาต์พุตแต่ละตัว

เอาต์พุตข้อความแต่ละประเภท (GUI, Error Console, Terminal) สามารถย้ายไปยังฟังก์ชันของตัวเองได้ ซึ่งช่วยให้ทดสอบ แก้ไข และขยายศักยภาพได้ง่ายขึ้นในอนาคต

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### ใช้การตรวจสอบซ้ำในแต่ละฟังก์ชันส่ง

เนื่องจากการตรวจสอบเป็นส่วนสำคัญในการตรวจสอบให้แน่ใจว่าทั้งข้อความและเอาต์พุตถูกต้อง จึงสมเหตุสมผลที่ฟังก์ชัน `send-*` แต่ละตัวจะดำเนินการตรวจสอบความถูกต้องของตัวเอง สิ่งนี้ทำให้แน่ใจได้ว่าไม่ว่าจะเรียกเอาต์พุตใด เราจะตรวจสอบอินพุตก่อนเสมอ

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

ดูว่าเราได้ลบการตรวจสอบความถูกต้องออกจากฟังก์ชันส่งข้อความแล้ว และเปลี่ยนความรับผิดชอบไปที่ฟังก์ชันเอาต์พุตแต่ละรายการ การเปลี่ยนแปลงนี้ทำให้มั่นใจได้ว่าแต่ละปลายทาง (GUI, Error Console, Terminal) จะจัดการการตรวจสอบของตัวเอง ปรับปรุงฟังก์ชันส่งข้อความ และรักษาตรรกะการตรวจสอบให้ใกล้กับจุดที่จำเป็นมากขึ้น

วิธีการนี้สามารถลดความซับซ้อนของฟังก์ชันส่งข้อความ ทำให้เป็น _dispatcher_ ในขณะที่ทำให้มั่นใจได้ว่าแต่ละฟังก์ชัน send-to-* จะตรวจสอบข้อความอย่างถูกต้องก่อนประมวลผล

ด้วยการย้ายการตรวจสอบไปยังฟังก์ชัน send-to-* แต่ละฟังก์ชัน เราได้ทำให้สามารถนำมาใช้ใหม่เป็นฟังก์ชันแบบสแตนด์อโลนได้ ซึ่งหมายความว่าเราสามารถเรียกใช้ฟังก์ชัน send-to-gui, send-to-error-console หรือ send-to-terminal ใดๆ ได้โดยตรง โดยไม่ต้องอาศัยฟังก์ชัน send-message Dispatcher แต่ละฟังก์ชันเหล่านี้จัดการตรรกะของตัวเองได้อย่างสมบูรณ์แล้ว และสามารถใช้อย่างอิสระในส่วนอื่นๆ ของโค้ดหรือในปลั๊กอินอื่นๆ ทำให้โค้ดของคุณเป็นแบบโมดูลาร์และยืดหยุ่นมากขึ้น

## ประโยชน์ของการปรับโครงสร้างใหม่

- **การแยกข้อกังวลอย่างชัดเจน**: ตอนนี้แต่ละฟังก์ชันจะจัดการความรับผิดชอบเพียงข้อเดียว ทำให้โค้ดเข้าใจได้ง่ายขึ้น
- **ความสามารถในการขยาย**: การเพิ่มประเภทเอาต์พุตใหม่นั้นตรงไปตรงมา คุณเพียงกำหนดฟังก์ชันใหม่ เช่น `send-to-file` หรือ `send-to-logger` จากนั้นเพิ่มตัวพิมพ์ลงในคำสั่ง `cond`
- **การนำกลับมาใช้ใหม่ได้**: แต่ละฟังก์ชันการจัดการเอาต์พุตเหล่านี้สามารถนำมาใช้ซ้ำที่อื่นในโปรเจ็กต์ของคุณ หรือใช้ร่วมกันระหว่างปลั๊กอินหลายตัวได้
- **ความสม่ำเสมอ**: การนำฟังก์ชันการตรวจสอบความถูกต้องกลับมาใช้ใหม่ในแต่ละฟังก์ชัน `send-to-*` จะทำให้คุณมั่นใจได้ว่าเอาต์พุตทั้งหมดได้รับการตรวจสอบอย่างถูกต้อง ซึ่งจะทำให้โค้ดมีประสิทธิภาพมากขึ้น

เวอร์ชันไลบรารีที่ปรับโครงสร้างใหม่:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

นั่นคือทั้งหมดที่เราสามารถทำได้? เลขที่! ยังมีอะไรที่ต้องทำอีกมาก โปรดอ่านต่อ