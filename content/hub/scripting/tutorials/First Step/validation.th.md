---
title: "การตรวจสอบ"
type: docs
weight: 4
---
เมื่อสร้างปลั๊กอินที่มีประสิทธิภาพ สิ่งสำคัญคือต้องแน่ใจว่าฟังก์ชันของเราจัดการกับข้อผิดพลาดได้อย่างงดงามและทำงานตามที่คาดไว้ แม้ว่าในกรณีของการใช้ในทางที่ผิดหรืออินพุตที่ไม่คาดคิดก็ตาม การตรวจสอบความถูกต้องจะช่วยปกป้องความสมบูรณ์ของฟังก์ชันและป้องกันการหยุดทำงานหรือพฤติกรรมที่ไม่ได้ตั้งใจ

มาดูกันว่าเราจะปรับปรุงฟังก์ชัน `send-message` ได้อย่างไรโดยเพิ่มการตรวจสอบความถูกต้องเพื่อให้แน่ใจว่าจะจัดการอินพุตได้อย่างถูกต้อง

### ตรวจสอบอินพุต

ก่อนที่จะส่งข้อความ เราควรตรวจสอบให้แน่ใจว่าอาร์กิวเมนต์ `output` ที่ส่งไปยังฟังก์ชัน `send-message` นั้นถูกต้อง เราสามารถเพิ่มการตรวจสอบเพื่อยืนยันว่าปลายทางเอาต์พุตเป็นหนึ่งในค่าที่คาดหวัง (gui, error-console หรือ terminal)

ตัวอย่าง:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

ในตัวอย่างนี้ เราใช้ `member` เพื่อตรวจสอบว่าอาร์กิวเมนต์ `output` ถูกต้องหรือไม่ ถ้าไม่เช่นนั้น ฟังก์ชันจะแสดงข้อผิดพลาดพร้อมข้อความที่ชัดเจน เพื่อป้องกันไม่ให้ค่าที่ไม่ถูกต้องก่อให้เกิดปัญหา

### จัดการข้อความว่าง

นอกจากนี้ ยังมีประโยชน์ในการตรวจสอบให้แน่ใจว่าอาร์กิวเมนต์ `message` นั้นถูกต้องอีกด้วย ตัวอย่างเช่น ถ้าสตริงว่างหรือ #f (false) ถูกส่งเป็นข้อความ ฟังก์ชันควรจัดการสิ่งนี้อย่างสวยงาม

ตัวอย่างการจัดการข้อความเปล่า:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

วิธีการนี้ช่วยให้มั่นใจได้ว่าฟังก์ชันจะได้รับอินพุตที่ถูกต้องอยู่เสมอ ปรับปรุงความน่าเชื่อถือและป้องกันพฤติกรรมที่ไม่คาดคิด

### ตัวอย่างการตรวจสอบแบบรวม

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

ในเวอร์ชันนี้:
- ฟังก์ชั่นตรวจสอบว่า `message` ว่างเปล่าหรือไม่ถูกต้องก่อน หากข้อความถูกต้อง ข้อความจะไปยังการตรวจสอบว่า `output` เป็นหนึ่งในค่าที่ยอมรับ (`gui`, `error-console` หรือ `terminal`)
- หากผ่านการตรวจสอบทั้งสอง ข้อความจะถูกส่งไปยังเอาต์พุตที่เหมาะสม มิฉะนั้นจะเกิดข้อความแสดงข้อผิดพลาดพร้อมคำอธิบายที่ชัดเจน
- มีการตรวจสอบเพิ่มเติมเพื่อให้แน่ใจว่าข้อความนั้นเป็นสตริงด้วย

ฟังก์ชันการตรวจสอบความถูกต้องแบบรวมนี้ช่วยทำความสะอาดโค้ดและทำให้แน่ใจว่าอินพุตทั้งสองได้รับการตรวจสอบก่อนดำเนินการใดๆ ทำให้ฟังก์ชันมีประสิทธิภาพมากขึ้น โปรดทราบว่าเรากำลังสร้างระบบส่งข้อความแก้ไขจุดบกพร่องด้วย เมื่อ
รหัสล้มเหลว เราได้รับเหตุผล เหตุผลที่เราเขียนเอง

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```