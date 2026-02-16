---
title: "การสร้างเวอร์ชันดีบัก"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
คู่มือนี้จะอธิบาย **เวิร์กโฟลว์การแก้ไขจุดบกพร่องในตัวเครื่อง** สำหรับ Lumi โดยใช้สคริปต์ใน `build/lumi/scripts`

ขั้นตอนการทำงานได้รับการออกแบบเพื่อ:

- ใช้สิ่งประดิษฐ์สร้างในเครื่อง (ไม่จำเป็นต้องดาวน์โหลดสัญลักษณ์)
- ตรวจสอบว่ามีสัญลักษณ์การดีบักอยู่จริง
- เปิด GDB ด้วยโหมดสัญลักษณ์ออฟไลน์ตามค่าเริ่มต้น

## ข้อกำหนดเบื้องต้น

- Linux ที่ใช้ Debian (พื้นฐานโครงการ: Debian 13)
- แผนผังต้นทาง Lumi ได้รับการโคลนแล้ว

## การตั้งค่า GDB ครั้งเดียว (ไม่บังคับ แต่แนะนำ)

ติดตั้งเครื่องมือ GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

การตั้งค่าการบันทึกในเครื่องเพิ่มเติม:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

หมายเหตุ: สคริปต์ดีบักในเครื่องของ Lumi จะปิดใช้งาน `debuginfod` ตามค่าเริ่มต้น เพื่อให้ความละเอียดของสัญลักษณ์อยู่ในเครื่องและสามารถทำซ้ำได้

## เริ่มต้นอย่างรวดเร็ว

จากไดเร็กทอรีสคริปต์:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug Build + Launch (ค่าเริ่มต้น)

ใช้สิ่งนี้สำหรับเซสชันการดีบักปกติ

```bash
bash lumi-debug-local.sh lumi-dev build
```

คำสั่งนี้:

1. สร้าง Lumi ในโหมดแก้ไขข้อบกพร่อง
2. ตรวจสอบสัญลักษณ์การดีบัก
3. เปิดตัว Lumi ภายใต้ GDB

### Debug Build เท่านั้น (สำหรับ TTY/เซสชันระยะไกลในภายหลัง)

ใช้สิ่งนี้เมื่อคุณต้องการสร้างตอนนี้และเปิดใช้/แก้ไขข้อบกพร่องในภายหลัง

```bash
bash lumi-build-debug.sh lumi-dev build
```

## การใช้ TTY ใน Linux

TTY (คอนโซลข้อความ) มักเป็นวิธีที่น่าเชื่อถือที่สุดในการดีบักการค้างแบบฮาร์ด

- เปลี่ยนเป็น TTY ด้วย `Ctrl + Alt + F1` ถึง `Ctrl + Alt + F6`
- เข้าสู่ระบบจากข้อความแจ้ง
- กลับสู่เซสชันกราฟิกด้วย `Ctrl + Alt + F7` (หรือ `F2` ในบางระบบ)

เหตุใดจึงสำคัญ: หากเซสชันเดสก์ท็อปหยุดทำงาน TTY มักจะยังคงตอบสนอง ดังนั้นคุณจึงสามารถแนบ GDB บันทึกย้อนหลัง และกู้คืนข้อมูลข้อขัดข้องที่เป็นประโยชน์ได้

## ตัวเลือกเสริม: การดีบักระยะไกล/TTY

สำหรับการค้างหรือการล็อคการแสดงผล ให้ใช้ `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

จากนั้นจาก TTY (แนะนำสำหรับสถานการณ์การหยุดทำงาน) หรือเทอร์มินัลอื่น:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

สำหรับการเปิดตัว GDB โลคัล (พาธที่ไม่ใช่ TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## หมายเหตุประสิทธิภาพ

การสร้างการแก้ไขข้อบกพร่องจะช้าลงตามการออกแบบ เมื่อคุณแก้ไขจุดบกพร่องเสร็จแล้ว ให้เปลี่ยนกลับเป็นบิลด์ที่เร็วขึ้น:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```