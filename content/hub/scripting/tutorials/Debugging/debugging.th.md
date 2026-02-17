---
title: "การดีบัก"
type: docs
weight: 5
---
ในการเขียนสคริปต์ ไม่มีฟังก์ชันใดที่ผิดพลาดได้ แม้แต่คำสั่งที่น่าเชื่อถือที่สุดก็อาจล้มเหลวได้เมื่อต้องเผชิญกับอินพุตหรือเงื่อนไขที่ไม่คาดคิด เพื่อป้องกันสิ่งนี้ เราสามารถใช้ระบบการดีบักแบบกำหนดเอง และใช้เทคนิคการเขียนโปรแกรมป้องกัน ด้วยการรวมฟังก์ชันมาตรฐานเข้ากับกลไกการจัดการข้อผิดพลาดและการให้ข้อเสนอแนะที่ให้ข้อมูล เราสามารถทำให้สคริปต์ของเรามีประสิทธิภาพมากขึ้นและง่ายต่อการแก้ไขปัญหา

ส่วนสำคัญของกลยุทธ์นี้คือการใช้แฟล็กการดีบักส่วนกลางเพื่อควบคุมเอาต์พุตแบบละเอียด ช่วยให้เราสามารถเปิดใช้งานข้อมูลการดีบักโดยละเอียดเมื่อจำเป็น ในขณะเดียวกันก็รักษาเอาต์พุตให้สะอาดในระหว่างการดำเนินการตามปกติ

## ธงการแก้ปัญหาทั่วโลก

แฟล็กดีบักโกลบอลเป็นวิธีที่เรียบง่ายแต่มีประสิทธิภาพในการควบคุมระดับเอาต์พุตข้อมูลระหว่างการเรียกใช้สคริปต์ เมื่อเปิดใช้งาน จะมีข้อความแก้ไขจุดบกพร่องโดยละเอียดซึ่งมีคุณค่าสำหรับการติดตามปัญหา เมื่อปิดใช้งาน จะทำให้เอาต์พุตมีความกระชับสำหรับการใช้งานจริง

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

ตามค่าเริ่มต้น การดีบักจะถูกปิด หากต้องการเปิดใช้งานเอาต์พุตแบบละเอียดในระหว่างการพัฒนา เพียงตั้งค่าแฟล็กเป็น `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

นอกจากนี้เรายังสามารถเปิดหรือปิดการดีบักชั่วคราวสำหรับส่วนโค้ดเฉพาะโดยใช้ฟังก์ชันตัวช่วย

### การควบคุมการดีบักในเครื่อง

เพื่อการควบคุมที่ละเอียดยิ่งขึ้น เราสามารถเปิดหรือปิดการดีบักภายในส่วนเฉพาะของสคริปต์ได้โดยใช้ฟังก์ชันตัวช่วย

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

สิ่งนี้ทำให้เราสามารถควบคุมการดีบักแบบไดนามิก:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## แก้ปัญหาระบบส่งข้อความ

เพื่อจัดการเอาต์พุตการดีบักใน Scheme อย่างมีประสิทธิภาพ เราใช้วิธีการที่มีโครงสร้างซึ่งเกี่ยวข้องกับฟังก์ชันตัวช่วยหลายตัว ฟังก์ชันเหล่านี้ช่วยให้แน่ใจว่าข้อความดีบักและคำเตือนมีความชัดเจน อ่านได้ และบำรุงรักษาได้

### ภาพรวมของระบบข้อความแก้ไขข้อบกพร่อง

ระบบส่งข้อความแก้ไขข้อบกพร่องของเราประกอบด้วยองค์ประกอบต่อไปนี้:

1. `debug-message` – แสดงข้อความดีบักเมื่อเปิดใช้งานการดีบัก
2. `serialize-item` – แปลงข้อมูล Scheme ประเภทต่างๆ ให้เป็นการแสดงสตริง
3. `concat` – เชื่อมต่อหลายรายการให้เป็นสตริงเดียว
4. `list->string` – จัดรูปแบบรายการให้เป็นสตริงที่อ่านได้
5. `message` – แสดงผลในคอนโซลข้อความของ Lumi
6. `warning-message` – แสดงข้อความเตือนเมื่อเปิดใช้งานคำเตือน

แต่ละฟังก์ชันมีบทบาทในการจัดรูปแบบและการแสดงข้อความที่มีโครงสร้าง

---

### ฟังก์ชั่นข้อความแก้ไขข้อบกพร่อง

ฟังก์ชัน `debug-message` เป็นวิธีการหลักในการแสดงเอาต์พุตการดีบัก ช่วยให้มั่นใจได้ว่าข้อความจะแสดงเฉพาะเมื่อเปิดใช้งานการดีบักเท่านั้น

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- เงื่อนไข `when debug` ช่วยให้มั่นใจได้ว่าข้อความจะปรากฏเฉพาะเมื่อเปิดใช้งานการดีบักเท่านั้น
- ข้อความขึ้นต้นด้วย `"> "` เพื่อความชัดเจน
- ฟังก์ชั่นนี้ใช้ `concat` เพื่อจัดรูปแบบเนื้อหาข้อความ
- สุดท้ายจะเรียก `message` เพื่อส่งออกไปยังคอนโซลข้อความของ Lumi

ตัวอย่างการใช้งาน:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

เมื่อเปิดใช้งานการดีบัก ผลลัพธ์อาจเป็น:

```scheme
> item: background-layer has tree position : 3
```

### การทำให้ข้อมูลเป็นอนุกรมสำหรับข้อความแก้ไขข้อบกพร่อง

ข้อความอาจมีประเภทข้อมูลที่แตกต่างกัน เช่น รายการ เวกเตอร์ และตัวเลข เพื่อให้แน่ใจว่ามีการจัดรูปแบบอย่างถูกต้อง เราใช้ `serialize-item`

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

ตัวอย่างการใช้งาน:

```scheme
(serialize-item '(1 2 3))
```

เอาท์พุท:

```scheme
list:
1
2
3
```

### การต่อข้อมูลสำหรับข้อความ

หากต้องการรวมองค์ประกอบข้อความหลายรายการให้เป็นสตริงเดียว เราใช้ `concat`

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

ตัวอย่างการใช้งาน:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### การจัดรูปแบบรายการเป็นสตริง

ฟังก์ชัน `list->string` จะแปลงรายการเป็นสตริงที่จัดรูปแบบ

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### ข้อความเตือนฟังก์ชัน `warning-message` ทำงานคล้ายกับ `debug-message` แต่จะแสดงคำเตือนแม้ว่าจะปิดใช้งานการแก้ไขจุดบกพร่องก็ตาม

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- ตรวจสอบให้แน่ใจว่าข้อความจะแสดงเฉพาะเมื่อมีการเปิดใช้งานคำเตือน (ตั้งค่าสถานะ `warning` ใน `common.scm` เป็น `#t`)
- โทร `concat` เพื่อจัดรูปแบบเนื้อหาข้อความ
- ใช้ `message` เพื่อส่งออกไปยัง Lumi

## การปรับปรุงฟังก์ชั่นมาตรฐาน

เมื่อมีระบบดีบั๊กแล้ว เราสามารถปรับปรุงไลบรารีฟังก์ชันของเราได้โดยการรวมข้อความโดยละเอียดเข้าด้วยกัน ข้อมูลนี้ให้ข้อมูลเชิงลึกเกี่ยวกับสถานะของรายการ ค่าตัวแปร และการเรียกใช้ฟังก์ชัน

ตัวอย่างทั่วไปคือ `item-is-valid?` ซึ่งล้อม `lumi-item-id-is-valid` เพื่อส่งคืน `#t` หรือ `#f` หาก `#f` ถูกส่งคืน เราสามารถทริกเกอร์ `warning-message` ในรหัสการโทรได้ หากอินพุตไม่ใช่ตัวเลข เราก็สามารถส่งคำเตือนในฟังก์ชันได้

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## การใช้งานจริง

เมื่อพัฒนาปลั๊กอิน Scheme ฟังก์ชันการรวมในลักษณะนี้จะช่วยลดเวลาในการดีบักได้อย่างมาก และรับประกันว่าโค้ดจะมีประสิทธิภาพและบำรุงรักษาได้ ด้วยระบบการดีบักของเรา เราสามารถสร้างสตรีมการดีบักที่มีโครงสร้างในคอนโซลข้อผิดพลาดได้เพียงแค่กดสวิตช์

ในสตรีมการแก้ไขข้อบกพร่องนี้ การเรียกใช้ฟังก์ชันจะมีเครื่องหมายดอกจัน (*) กำกับไว้ ทำให้ง่ายต่อการติดตามการเรียกใช้สคริปต์และระบุความล้มเหลว โดยเฉพาะในปลั๊กอินที่ซับซ้อน การมองเห็นนี้ช่วยให้เราเข้าใจขั้นตอนการปฏิบัติงานและวินิจฉัยพฤติกรรมที่ไม่คาดคิดได้อย่างมีประสิทธิภาพ

wrapper สำหรับฟังก์ชันข้อความของเราเพื่อใช้ `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

ตัวอย่างของ `call` ที่ถูกนำไปใช้ในทางปฏิบัติ:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

ตัวอย่างของสตรีมการดีบักเมื่อปลั๊กอินดำเนินการ:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

บันทึกที่มีโครงสร้างนี้ให้ไทม์ไลน์ที่ชัดเจนของการเรียกใช้ฟังก์ชันและการเปลี่ยนแปลงข้อมูล ทำให้การดีบักและการวิเคราะห์ประสิทธิภาพง่ายขึ้นอย่างมาก

## บทสรุป

ด้วยการนำระบบดีบักที่มีโครงสร้างไปใช้ เราจึงสร้างสคริปต์ที่ปลอดภัยและบำรุงรักษาได้มากขึ้น ซึ่งให้ข้อมูลเชิงลึกแบบเรียลไทม์ในการดำเนินการ

### ประเด็นสำคัญ

- **ควบคุมคำฟุ่มเฟือย** – ใช้แฟล็กการแก้ไขจุดบกพร่องส่วนกลางเพื่อจัดการระดับเอาต์พุต
- **ให้ข้อเสนอแนะที่ชัดเจน** – รวมฟังก์ชันมาตรฐานด้วยข้อความแก้ไขข้อบกพร่องที่ให้ข้อมูล
- **ปรับปรุงความทนทาน** – จัดการอินพุตที่ไม่คาดคิดอย่างสง่างามเพื่อป้องกันข้อผิดพลาด
- **ลดความซับซ้อนในการแก้ไขปัญหา** – ข้อความแก้ไขข้อบกพร่องที่มีโครงสร้างช่วยให้วินิจฉัยและแก้ไขปัญหาได้ง่ายขึ้น

ด้วยแนวทางนี้ สคริปต์ของเราจะ "อธิบายตัวเอง" ได้อย่างมีประสิทธิภาพในขณะที่ประมวลผลข้อมูล ลดความยุ่งยากและปรับปรุงประสิทธิภาพเวิร์กโฟลว์ การดีบักกลายเป็นเครื่องมือเชิงรุกแทนที่จะเป็นงานตอบโต้ ทำให้กระบวนการเขียนสคริปต์ของเราราบรื่นขึ้นและคุ้มค่ามากขึ้น