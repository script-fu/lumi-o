---
title: "ปลั๊กอินตัวกรอง"
type: docs
weight: 2
---
เราใช้ปลั๊กอิน _procedure_ สำหรับบทช่วยสอน [First Step](../../first-step/) ปลั๊กอินประเภทเหล่านั้นทำงานได้โดยไม่ต้องใช้รูปภาพหรือวาดเป็นอินพุตได้ โดยปกติแล้ว เราใช้ปลั๊กอินเพื่อเปลี่ยนรูปภาพและสิ่งที่วาดได้ ปลั๊กอินลักษณะนี้เรียกว่าปลั๊กอิน _filter_

### Drawable คืออะไร?

**วาดได้** ใน Lumi หมายถึงองค์ประกอบรูปภาพที่สามารถวาดลงบนได้ เช่น เลเยอร์หรือช่อง โดยทั่วไปปลั๊กอินตัวกรองจะทำงานบนองค์ประกอบเหล่านี้

### ตัวอย่างปลั๊กอินตัวกรองอย่างง่าย

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

คัดลอกข้อความและบันทึกเป็น `simple-filter-plug-in.scm` ในโฟลเดอร์ชื่อ `simple-filter-plug-in` ภายในโฟลเดอร์ปลั๊กอินของ Lumi โฟลเดอร์ปลั๊กอิน Lumi คือโฟลเดอร์ _any_ ที่อยู่ในรายการ:
 **Lumi > แก้ไข > การตั้งค่า > โฟลเดอร์ > ปลั๊กอิน**

ใน Linux คลิกขวาที่ไฟล์ `simple-filter-plug-in.scm` ไปที่ **Properties > Permissions** และทำเครื่องหมาย **Allow executing file as program** เมื่อไฟล์อยู่ในตำแหน่งที่ถูกต้อง สามารถเรียกใช้งานได้และปราศจากข้อผิดพลาดทางไวยากรณ์ เมื่อรีสตาร์ท Lumi ไฟล์นั้นจะปรากฏในแถบส่วนหัวของเมนูด้านบน ภายในเมนูที่เรียกว่า **ปลั๊กอิน**

### การเรียกใช้ปลั๊กอิน

1. เปิดรูปภาพ (ปลั๊กอินตัวกรองนี้ต้องใช้รูปภาพจึงจะใช้งานได้)
2. เปิด **Windows > Dockable Dialogs > Error Console** เพื่อดูข้อความ
3. เลือก **การสาธิตปลั๊กอินตัวกรองแบบง่าย** จากเมนู **ปลั๊กอิน**
4. หนึ่งในเลเยอร์ที่เลือกจะมีการกลับสีและข้อความจะถูกพิมพ์ไปยังคอนโซลข้อผิดพลาด

### การแก้ไขปลั๊กอิน

คุณสามารถปรับแต่งปลั๊กอินได้โดยแก้ไขไฟล์ `.scm` ตัวอย่างเช่น หากต้องการเปลี่ยนข้อความที่แสดง:

1. เปิดไฟล์และค้นหาบรรทัดที่กำหนด `message`
2. แทนที่ `"hello, world"` ด้วยข้อความที่คุณกำหนดเอง
3. บันทึกไฟล์

ใน Lumi เวอร์ชัน 3 ปลั๊กอินไม่จำเป็นต้องรีเฟรชเพื่อให้การเปลี่ยนแปลงที่บันทึกไว้มีผล เพียงเรียกใช้ปลั๊กอินอีกครั้งเพื่อดูข้อความที่อัปเดต

### การตรวจสอบปลั๊กอิน

#### สายชีบัง

บรรทัดแรกช่วยให้แน่ใจว่าสคริปต์ทำงานเป็นปลั๊กอินใน Lumi 3:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### คำจำกัดความของขั้นตอน

ขั้นตอนยอมรับสองอาร์กิวเมนต์: รูปภาพที่ใช้งานอยู่และสิ่งที่วาดได้ที่เลือก

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### คอร์ลอจิก

คำสั่ง `let` กำหนดตัวแปรและดำเนินการกับสิ่งที่ถอนออกได้

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### การลงทะเบียนปลั๊กอิน

ปลั๊กอินได้รับการลงทะเบียนกับ Lumi เป็นปลั๊กอินตัวกรอง:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### ลงทะเบียนเมนู
บรรทัดนี้ระบุตำแหน่งเมนูสำหรับปลั๊กอิน:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### การแก้ไขปัญหา

หากปลั๊กอินไม่ปรากฏขึ้น ให้ตรวจสอบตำแหน่ง ชื่อ และคุณสมบัติที่ปฏิบัติการได้

ตำแหน่งจะต้องอยู่ในเส้นทางการค้นหาปลั๊กอิน
ชื่อไฟล์จะต้องตรงกับชื่อของโฟลเดอร์ที่มีอยู่
ไฟล์จะต้องถูกตั้งค่าให้ปฏิบัติการได้


**คอนโซลข้อผิดพลาด** เป็นเครื่องมืออันทรงคุณค่าสำหรับการแก้ไขปัญหาปลั๊กอินที่กำหนดเอง หากปลั๊กอินของคุณไม่ทำงานตามที่คาดไว้ ให้ตรวจสอบข้อความแสดงข้อผิดพลาดหรือบันทึกที่นี่ หน้าต่าง **เทอร์มินัล** ยังสามารถให้ข้อมูลการแก้ไขข้อบกพร่องและรายงานปัญหาในการโหลดได้