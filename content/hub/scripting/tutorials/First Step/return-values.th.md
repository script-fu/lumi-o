---
title: "ส่งกลับค่า"
type: docs
weight: 8
---
ค่าที่ส่งกลับมีความสำคัญเนื่องจากช่วยให้คุณควบคุมโฟลว์ได้โดยไม่ต้องมีสถานะเพิ่มเติม ใน Scheme นิพจน์ที่ได้รับการประเมินล่าสุดจะกลายเป็นค่าที่ส่งคืน

หน้านี้ใช้ตัวช่วยในการตรวจสอบจากตัวอย่างข้อความเพื่อแสดงให้เห็นว่าค่าที่ส่งคืนอย่างชัดเจนทำให้การเขียนโค้ดง่ายขึ้นได้อย่างไร

### มูลค่าการส่งคืนคืออะไร?

ใน Scheme ค่าที่ส่งคืนของฟังก์ชันจะถูกกำหนดโดยนิพจน์สุดท้ายที่ฟังก์ชันประเมิน ซึ่งหมายความว่าบรรทัดสุดท้ายของโค้ดในฟังก์ชันที่ประเมินค่าจะถูกส่งกลับตามผลลัพธ์ของฟังก์ชัน หากไม่มีการส่งคืนค่าอย่างชัดเจน ฟังก์ชันจะส่งคืน `#f` (false) หรือ `undefined`

เรามาทบทวนฟังก์ชั่นการตรวจสอบอีกครั้ง (is-valid-string?)

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

ในฟังก์ชันนี้ หากข้อความไม่ถูกต้อง ระบบจะแสดงข้อผิดพลาด อย่างไรก็ตาม หากข้อความถูกต้อง จะไม่ระบุค่าที่ส่งคืนอย่างชัดเจน และฟังก์ชันจะส่งคืน `#f` ตามค่าเริ่มต้น

### ทำให้ค่าส่งคืนชัดเจน

เราสามารถปรับปรุงสิ่งนี้ได้โดยทำให้ค่าที่ส่งคืนชัดเจนยิ่งขึ้น ตัวอย่างเช่น เราอาจส่งคืน `#t` (true) หากข้อความถูกต้อง:

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

ในเวอร์ชันนี้ ฟังก์ชันจะส่งกลับ `#t` เมื่อข้อความถูกต้อง โดยให้ผลลัพธ์ที่ชัดเจน ซึ่งช่วยให้สามารถใช้ฟังก์ชันได้อย่างยืดหยุ่นมากขึ้นในบริบทอื่นๆ ที่จำเป็นต้องใช้ผลลัพธ์แบบบูลีน

### การใช้ค่าส่งคืนอย่างมีประสิทธิภาพ

การตัดสินใจว่าฟังก์ชันของเราส่งคืนอะไร เราสามารถทำให้สามารถคาดเดาได้และมีประโยชน์มากขึ้น การส่งคืนค่า เช่น `#t`, `#f` หรือผลลัพธ์เฉพาะเจาะจงช่วยให้เราควบคุมวิธีที่ฟังก์ชันโต้ตอบกับโค้ดที่เหลือได้มากขึ้น ตัวอย่างเช่น คุณสามารถใช้ค่าที่ส่งคืนเพื่อทำการตัดสินใจเพิ่มเติมในฟังก์ชันการโทร หรือส่งผ่านเป็นอาร์กิวเมนต์ไปยังฟังก์ชันอื่นได้

ต่อไปนี้เป็นตัวอย่างง่ายๆ ของการใช้ค่าตอบแทนเพื่อควบคุมการไหลของตรรกะ:

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

ในกรณีนี้ (ส่งข้อความ) อาศัยค่าที่ส่งคืนของ (is-valid-output-display?) เพื่อตัดสินใจว่าจะดำเนินการต่อไปหรือไม่
คำสั่งแบบมีเงื่อนไข `cond` จะถูกข้ามหากการทดสอบครั้งแรกล้มเหลว นอกจากนี้ ให้สังเกตว่ามันอ่านในลักษณะที่เป็นธรรมชาติอย่างไร หากเป็นจอแสดงผลเอาท์พุตที่ถูกต้อง

## ถ้าตรรกะคำสั่งในโครงการ

ก่อนตัวอย่างไลบรารีที่ปรับโครงสร้างใหม่ ต่อไปนี้คือการตรวจสอบตรรกะแบบมีเงื่อนไขโดยย่อ Scheme ใช้ `if` เพื่อเลือกระหว่างสองเส้นทาง

นี่คือรูปแบบง่ายๆ ของคำสั่ง `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

โครงสร้างนี้จะตรวจสอบเงื่อนไข และหากเงื่อนไขเป็นจริง โครงสร้างจะดำเนินการในการดำเนินการแรก หากเงื่อนไขเป็นเท็จ จะดำเนินการที่สอง

ในกรณีที่คุณจำเป็นต้องดำเนินการหลายอย่างเมื่อเงื่อนไขเป็นจริงหรือเท็จ คุณสามารถใช้ `begin` เพื่อจัดกลุ่มเข้าด้วยกัน:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

สิ่งนี้ช่วยให้คุณสามารถจัดการกับสถานการณ์ที่ซับซ้อนมากขึ้น ซึ่งจำเป็นต้องดำเนินการหลายนิพจน์หรือคำสั่ง ทั้งนี้ขึ้นอยู่กับผลลัพธ์ของการทดสอบแบบมีเงื่อนไข

โอเค นี่คือโค้ดไลบรารีที่มีค่าส่งคืนฝังอยู่และใช้เพื่อควบคุมกระบวนการดำเนินการ

### ปรับโครงสร้างใหม่ด้วยค่าส่งคืน

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

ค่าที่ส่งคืนเป็นส่วนพื้นฐานของการทำให้ฟังก์ชันมีความยืดหยุ่นและนำกลับมาใช้ใหม่ได้ ด้วยการตัดสินใจอย่างรอบคอบว่าแต่ละฟังก์ชันควรส่งคืนอะไร เราสามารถมั่นใจได้ว่าฟังก์ชันของเราโต้ตอบกันได้ดี และให้ข้อมูลที่เป็นประโยชน์กับส่วนที่เหลือของโค้ด ไม่ว่าจะเป็นการส่งคืน `#t` หรือ `#f` หรืออะไรที่เฉพาะเจาะจงมากขึ้น ค่าที่ส่งคืนทำให้เรามีวิธีในการควบคุมโฟลว์ของโปรแกรมของเราและจัดการกับผลลัพธ์ต่างๆ