---
title: "การผสมสีสเปกตรัม"
type: docs
---
ระบบจานสีของ Lumi ใช้แบบจำลองสีสเปกตรัมเพื่อจำลองการผสมของเม็ดสีจริง เป้าหมายคือการทำให้ประสบการณ์ในการสร้างและเลือกสีจากจานสีดิจิทัลมีพฤติกรรมเหมือนกับการผสมสีจริง เมื่อลงสีบนผืนผ้าใบแล้ว ก็จะเป็น RGB มาตรฐาน

## การผสมสเปกตรัมหมายถึงอะไร

การผสม RGB แบบดั้งเดิมเป็นการเพิ่มเติม: การผสมค่า RGB สองค่าจะเฉลี่ยค่าเหล่านั้นไปที่จุดกึ่งกลาง การผสมเม็ดสีเป็นแบบลบ: เม็ดสีแต่ละสีดูดซับความยาวคลื่นที่แน่นอน และผลรวมของเม็ดสีจะเข้มขึ้นและมักจะเปลี่ยนสี

Lumi จำลองสิ่งนี้โดยใช้การแสดงสเปกตรัมการสะท้อน 10 แบนด์สำหรับสีจานสี แทนที่จะเป็น RGB

ซึ่งให้ผลลัพธ์เหมือนสี: การผสมสีน้ำเงินและสีเหลืองทำให้เกิดสีเขียว ไม่ใช่สีเทา การผสมสีอิ่มตัวสองสีจะทำให้สีเปลี่ยนไปเป็นสีที่เป็นกลางเหมือนกับที่เม็ดสีทางกายภาพทำ

การคำนวณสเปกตรัมจะทำงานในระหว่างการสร้างพาเล็ต — เมื่อสร้างรายการพาเล็ตรองและตติยภูมิ และเมื่อตัวผสมพาเล็ตผสมสีหลักสองสี สีที่ได้จะถูกแปลงเป็น RGB เชิงเส้นสำหรับการแสดงผลและการลงสี

## โปรไฟล์เม็ดสี

รายการจานสีสามารถอิงตามข้อมูลเม็ดสีจริงโดยใช้ **รหัสดัชนีสี (CI)** เม็ดสี CI แต่ละตระกูลมีลักษณะเฉพาะของสเปกตรัมที่มีอิทธิพลต่อการผสมสี

| บทบาทของเม็ดสี | พฤติกรรมการผสม | ตัวอย่าง |
| :--- | :--- | :--- |
| **ประถมศึกษา** | โครมาสูง รองสะอาด | PY3 (เหลืองเลมอน), PR122 (ม่วงแดง) |
| **ร่างกาย** | โทนสีทึบทึบ เปลี่ยนเป็นสีมะกอกผสมสีเขียว | PY35 (แคดเมียมเหลือง), PR108 (แคดเมียมแดง) |
| **สารทำให้เป็นกลาง** | ลดความอิ่มตัวและปิดเสียงอย่างรวดเร็ว | PBk11 (ดาวอังคารดำ), PBr7 (เซียนน่า) |
| **โครมาแองเคอร์** | ความเข้มสีสูง ครองส่วนผสม | PB29 (ฟ้าอัลตรามารีน), PG7 (เขียวพลู) |

การเพิ่มแม่สีด้วยรหัส CI ลงในจานสีจะทำให้เครื่องมือผสมมีอคติสเปกตรัมสำหรับสีเหล่านั้นอย่างแม่นยำ ดังนั้นมิกซ์ทุติยภูมิและตติยภูมิที่สร้างขึ้นจึงสะท้อนถึงพฤติกรรมการผสมในโลกแห่งความเป็นจริง

## เม็ดสีลูมิ

Master Palette มาพร้อมกับเม็ดสีดังต่อไปนี้ ตัวอย่างแสดงลักษณะของแมสสโตนตามแบบฉบับของเม็ดสีแต่ละสี (มีความเข้มข้นเต็มที่ ไม่มีการเจือปน)

### ส้มและเหลือง| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไพโรลออเรนจ์ | PO73 | แดง (สการ์เล็ต) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ส้มแคดเมียม | PO20 | เหลือง (ตัว) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | แคดเมียมเหลือง | PY35 | เหลือง (ตัว) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | แคดเมียม เหลืองซีด | PY35:ซีด | สีเหลือง (แคดเมียมซีด) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เหลืองมะนาว | PY3 | เหลือง (มะนาว) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | นิกเกิล อาโซ เหลือง | PY150 | เหลือง (กลาง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | กรีนโกลด์ | PY129 | เหลือง-เขียว (ทอง) |

### สีเอิร์ธโทน

| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เผาเซียนน่า | PBr7:เบิร์นท์ | ดิน (น้ำตาลแดง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไหม้อัมเบอร์ | PBr7:อัมเบอร์ | โลก (เป็นกลาง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เซียนน่าดิบ | PBr7:ดิบ | ดิน (น้ำตาลเหลือง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | สีเหลืองสด | PY42 | ดิน (สีเหลือง) |

### สีเขียว

| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | พทาโล กรีน (YS) | PG36 | เขียว (พทาโลเหลืองเฉด) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | วิริเดียน | PG18 | เขียว (วิริเดียน) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | แตร์ แวร์เต้ | PG23 | เขียว(เอิร์ธคูล) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | วินเซอร์ กรีน (BS) | PG7 | สีเขียว (พทโลบลูเชด) |### บลูส์และสีฟ้า

| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | โคบอลต์เทอร์ควอยซ์ | PG50 | สีฟ้า (แร่) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | สีฟ้าเซรูเลียน | PB35 | สีฟ้า (แร่) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | พทาโล เทอร์ควอยส์ | PB16 | ฟ้า(พทโล) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | โคบอลต์บลู | PB28 | สีฟ้า (ม่วง-ลีน) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | วินเซอร์บลู | PB15 | ฟ้า(พทโล) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | อุลตร้ามารีน | PB29 | สีฟ้า (ม่วง-ลีน) |

### สีม่วง สีม่วงแดง และสีแดง

| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | สีม่วงสดใส | PV23 | สีม่วง (ไดออกซาซีน) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | กุหลาบถาวร | PV19:โรส | สีม่วงแดง (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ควินาคริโดน สีม่วงแดง | PV19:สีม่วงแดง | สีม่วงแดง (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | อลิซาริน คริมสันถาวร | PV19:สีแดงเข้ม | สีม่วงแดง (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เพอรีลีน ไวโอเล็ต | พีวี29 | สีม่วงแดง (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เพอรีลีน มารูน | PR179 | แดง (แดงเข้ม) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไพโรล เรด | PR254 | แดง (สการ์เล็ต) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไพโรลไฟแดง | PR255 | สีแดง (ไฟไพโรล) |

### คนผิวดำและคนผิวขาว| สวอตช์ | ชื่อ | รหัสซีไอ | ครอบครัว |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ดาวอังคารดำ (อุ่น) | PBk11 | ดำ (ดาวอังคาร) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เพอรีลีน กรีน | PBk31 | สีดำ (สีเขียวเพอริลีน) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไอวอรี่แบล็ค (คูล) | PBk9 | ดำ (งาช้าง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | โคมไฟ สีดำ (เป็นกลาง) | PBk7 | ดำ(โคม) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | สีขาวไทเทเนียม (โทนอุ่น) | PW6:อุ่น | สีขาว (ไทเทเนียมอุ่น) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ไทเทเนียมสีขาว (เป็นกลาง) | PW6 | สีขาว (ไทเทเนียมเป็นกลาง) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | ซิงค์ไวท์ (คูล) | PW4 | สีขาว (ซิงค์คูล) |

### ควบคุมสีเทา

สีเทาควบคุมคือสารทำให้เป็นกลางที่เป็นมาตรฐานซึ่งใช้ในการลดความอิ่มตัวของสีผสมที่คาดเดาได้

| สวอตช์ | ชื่อ | รหัสซีไอ |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เทาอบอุ่น | N_อบอุ่น |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | สีเทากลาง | N_เป็นกลาง |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;พื้นหลัง:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | เทาคูล | N_COOL |

## แผนที่จานสี

แผนที่จานสีจะแสดงภาพจานสีที่ใช้งานอยู่เป็นวงล้อสี: 36 ส่วนเฉดสี (ขั้นละ 10°) × 15 เซลล์ความสว่าง เมื่อเพิ่มแม่สีแล้ว ระบบจะสร้างส่วนผสมรองและตติยภูมิ และวางไว้ในตำแหน่งแผนที่ที่เหมาะสม

การคลิกเซลล์จะเลือกสีเป็นพื้นหน้า Shift-click จะกำหนดให้เป็นจุดสิ้นสุดหลักใน Palette Mixer

## มิกเซอร์จานสี

Palette Mixer ได้รับสีใหม่จากรายการหลักสองรายการโดยใช้ไปป์ไลน์แบบคงที่สามขั้นตอน:

1. **ผสมผสาน**: Spectral WGM ระหว่างพาเรนต์ A (CCW) และพาเรนต์ B (CW)
2. **โครมา**: ผสมผสานกับสเปกตรัมที่เป็นกลางของพาเล็ต เพื่อลดความอิ่มตัวของสี
3. **โทนสี**: เกลี่ยไปทางผสมสีขาวหรือผสมสีดำ ปรับความสว่าง

โทนสีจะถูกนำไปใช้เป็นครั้งสุดท้าย ดังนั้นการปรับความสว่างจะไม่ถูกเจือจางด้วยการเปลี่ยนแปลงของสี การควบคุมการล็อคค่าและแถบรัดจะจำกัดผลลัพธ์ให้อยู่ในระดับความสว่างหรือแถบค่าเฉพาะ

สีผสมสามารถบันทึกลงในจานสีเป็นรายการ **กำหนดเอง** โดยจัดเก็บสูตรทั้งหมด (UID หลัก ปัจจัยการผสมผสาน โทนสี ค่าสี) เพื่อการกู้คืนในภายหลัง

## พิกเซลแคนวาสเป็น RGBระบบสเปกตรัมทำงานทั้งหมดภายในการสร้างจานสีและการเลือกสี เมื่อใช้ฝีแปรง สีพื้นหน้าซึ่งแปลงเป็น RGB เชิงเส้นแล้วคือสีที่ถูกทาสี Canvas จัดเก็บข้อมูลพิกเซล RGB มาตรฐาน

การผสมสเปกตรัมช่วยปรับปรุงประสบการณ์ในการสร้างจานสีและการเลือกสีในลักษณะที่สอดคล้องกับพฤติกรรมของเม็ดสีทางกายภาพ โดยไม่ต้องเปลี่ยนวิธีจัดเก็บหรือจัดวางข้อมูลภาพ