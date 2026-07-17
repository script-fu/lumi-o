---
title: "รายการเชื่อมโยง (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 05e4621ad061bed6351b31246d6705025936683acec1f2d104a0fd7f038f31f7
---
**รายการการเชื่อมโยง** (หรือ **รายการ**) เป็นโครงสร้างข้อมูลพื้นฐานในโครงการที่ใช้เพื่อแสดงคอลเลกชันของคู่คีย์-ค่า มันถูกนำไปใช้เป็นรายการคู่ โดยที่แต่ละคู่เชื่อมโยงคีย์ (โดยทั่วไปคือสัญลักษณ์) กับค่า รายการมีความเรียบง่าย ยืดหยุ่น และเหมาะสำหรับชุดข้อมูลขนาดเล็กถึงขนาดกลาง

### โครงสร้างรายชื่อสมาคม

อลิสต์คือรายการที่แต่ละองค์ประกอบเป็น **คู่** (สร้างด้วย `cons`) แต่ละคู่ประกอบด้วย:

- **คีย์**: องค์ประกอบแรก (โดยทั่วไปจะเป็นสัญลักษณ์)
- **ค่า**: องค์ประกอบที่ 2 ซึ่งอาจเป็นข้อมูลประเภทใดก็ได้

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **คีย์**: `'name`, `'age`, `'city`
- **มูลค่า**: `"Alice"`, `30`, `"Paris"`
- **โครงสร้าง**: รายการคู่:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### การสร้าง Alist

คุณสามารถสร้างรายการได้โดยการสร้างคู่ด้วยตนเองหรือสร้างโดยทางโปรแกรมโดยใช้ `cons`

#### การใช้คำพูดเดี่ยว (`'`)

เครื่องหมายคำพูดเดี่ยว (`'`) เป็นการย่อสำหรับ **quoting** ซึ่งทำให้ Scheme ไม่สามารถประเมินนิพจน์ได้ ทำให้เหมาะอย่างยิ่งสำหรับการสร้างรายการคงที่โดยที่คีย์และค่าทั้งหมดเป็นแบบฮาร์ดโค้ด

```scheme
;; Alist manuell definieren
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatisch ein neues Paar hinzufügen
(define updated-alist (cons '(country . "France") alist))
```

**ผลลัพธ์**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### การใช้ Backquote (`` ` ``) and Comma (`,`)

ตัวดำเนินการ backquote (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`) สิ่งนี้มีประโยชน์สำหรับการสร้างรายการที่มีการคำนวณคีย์หรือค่าขณะรันไทม์

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**ผลลัพธ์**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### ตัวอย่างการเปรียบเทียบ

รายการคงที่โดยใช้ `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

รายการแบบไดนามิกโดยใช้ `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### การเข้าถึงข้อมูลใน Alist

หากต้องการดึงค่าจากรายการ คุณสามารถใช้ฟังก์ชัน `assoc` ซึ่งจะค้นหาคู่ด้วยคีย์

```scheme
(assoc 'name alist)   ; คืนค่า (name . "Alice")
(assoc 'country alist) ; คืนค่า #f (ไม่พบคีย์)
```

### การแยกคุณค่า

เมื่อคุณดึงข้อมูลคู่โดยใช้ `assoc` ให้ใช้ `cdr` เพื่อแยกค่า:

```scheme
(cdr (assoc 'name alist))   ; คืนค่า "Alice"
```

### สรุปคุณสมบัติที่สำคัญ

- **Single Quote (`'`)**: สร้างรายการคงที่โดยที่องค์ประกอบทั้งหมดเป็นข้อมูลตามตัวอักษร
- **ราคาย้อนกลับ (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`)
- **Dot Notation (`.`)**: ใช้เพื่อสร้างคู่ เชื่อมโยงคีย์กับค่าในรายการ