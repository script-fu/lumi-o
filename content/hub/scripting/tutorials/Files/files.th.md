---
title: "ไฟล์"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
---
การทำงานกับไฟล์และไดเร็กทอรีถือเป็นสิ่งสำคัญสำหรับการพัฒนา Scheme ไม่ว่าคุณจะบันทึกเอาต์พุต โหลดทรัพยากร หรือจัดโครงสร้างโปรเจ็กต์ การทำความเข้าใจการทำงานของไฟล์จะทำให้สคริปต์ของคุณมีประสิทธิภาพและใช้งานง่ายยิ่งขึ้น

หน้านี้ครอบคลุมงานไฟล์และไดเร็กทอรีทั่วไป: การอ่านพาธ การสร้างไดเร็กทอรี และการรวบรวมอินพุตโฟลเดอร์ผ่านพารามิเตอร์ GUI

## โฮมไดเร็กตอรี่ของผู้ใช้

Lumi เป็น Linux เท่านั้น ดังนั้นโฮมไดเร็กตอรี่ของผู้ใช้จึงมาจาก `HOME` ตัวแปรสภาพแวดล้อม

หากต้องการรับโฮมไดเร็กตอรี่ของผู้ใช้เป็นสตริง:

```scheme
(getenv "HOME")
```

ตัวอย่างผลลัพธ์:

```scheme
"/home/username"
```

## DIR-SEPARATOR

นอกจากนี้ยังมีตัวแปรโกลบอล `DIR-SEPARATOR` ซึ่งเป็นตัวแยกพาธเฉพาะแพลตฟอร์ม ใน Lumi (Linux) จะเป็น `/` เสมอ

```scheme
> DIR-SEPARATOR
"/"
```

## การรับตำแหน่งไดเรกทอรี

เราสามารถขอตำแหน่งไดเร็กทอรีจากผู้ใช้ในกล่องโต้ตอบ Scheme สำหรับปลั๊กอิน

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME` จัดเตรียมเบราว์เซอร์ให้กับไดเร็กทอรี

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; …
    ))
```

ที่นี่เราจะตรวจสอบอินพุตไดเรกทอรีทั้งสอง (ต้นทางและปลายทาง) และถอยกลับไปเป็นค่าเริ่มต้นหากเส้นทาง GUI ว่างเปล่า/ไม่ถูกต้อง

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

หากคุณสนใจรายละเอียดการใช้งาน ให้ค้นหาแหล่งที่มาของปลั๊กอินสำหรับ `validate-path-and-dir`

## การทำไดเร็กทอรี

Scheme จัดเตรียมคำสั่ง ```dir-make``` เพื่อสร้างไดเร็กทอรี คำสั่งนี้ใช้เส้นทางที่แยกจากกัน "/" และสร้างไดเร็กทอรีเดียวพร้อมพารามิเตอร์ทางเลือกสำหรับสิทธิ์ เราไม่ได้ให้เส้นทางเฉพาะแพลตฟอร์มแก่มัน

โดยปกติแล้วเราจำเป็นต้องสร้างหลายไดเร็กทอรีสำหรับเส้นทางที่ใช้งานได้จริง เราสามารถใช้ wrapper สำหรับ ```dir-make``` เพื่อช่วยเราที่นี่

```scheme
;; วัตถุประสงค์: ตัวห่อ (dir-make) ที่สร้างพาธที่กำหนดจากแพลตฟอร์ม
;;          พาธที่ให้มา ส่งออกตัวคั่นแบบ Linux เสมอสำหรับ dir-make
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; ไดเรกทอรีหลัก
    ;; สร้างไดเรกทอรีที่เหลือทีละขั้นตอน
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; สร้างเส้นทาง
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

หมายเหตุ: ฟังก์ชันนี้ยังใช้ ```file-exists?``` ในตัวเพื่อข้ามการโทรที่ไม่จำเป็น โดยจะส่งกลับ #t หากไฟล์หรือไดเร็กทอรีที่ระบุมีอยู่ และ #f หากไม่มีอยู่หรือหากผู้ใช้ที่ร้องขอไม่สามารถเข้าถึงได้

## การสร้างเส้นทาง

เรายังต้องพังทลายและสร้างเส้นทางใหม่ใน Scheme ด้วย

หากต้องการแบ่งเส้นทางออกเป็นส่วนๆ ให้ใช้ ```strbreakup```:

### ตัวอย่างเส้นทาง Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> หมายเหตุ: เครื่องหมายทับนำหน้าและต่อท้ายจะกลายเป็นองค์ประกอบสตริงว่างในรายการผลลัพธ์

หากต้องการสร้างเส้นทางใหม่ ให้ใช้ ```string-append```:

### การสร้างเส้นทาง Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```