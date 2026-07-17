---
title: "สร้างเวอร์ชันดีบัก"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

คู่มือนี้อธิบาย **เวิร์กโฟลว์การดีบักในเครื่อง** ของ Lumi โดยใช้สคริปต์ใน `build/lumi/scripts`

เวิร์กโฟลว์นี้ออกแบบมาเพื่อ:

- ใช้ build artifacts ในเครื่อง (ไม่ต้องดาวน์โหลดสัญลักษณ์),
- ตรวจสอบว่ามีสัญลักษณ์ดีบักอยู่จริง,
- เปิด GDB ในโหมดสัญลักษณ์ offline ตามค่าเริ่มต้น

## ข้อกำหนดเบื้องต้น

- Linux ที่ใช้ Debian (baseline ของโปรเจกต์: Debian 13)
- โคลนต้นไม้ซอร์สของ Lumi แล้ว

## การตั้งค่า GDB ครั้งเดียว (ไม่บังคับ แต่แนะนำ)

ติดตั้งเครื่องมือ GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

การตั้งค่า log ในเครื่อง (ไม่บังคับ):

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

หมายเหตุ: สคริปต์ดีบักในเครื่องของ Lumi ปิด `debuginfod` ตามค่าเริ่มต้น เพื่อให้การ resolve สัญลักษณ์อยู่ในเครื่องและทำซ้ำได้

## เริ่มต้นอย่างรวดเร็ว

จากไดเรกทอรีสคริปต์:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Build แบบดีบัก + เปิด (ค่าเริ่มต้น)

ใช้สำหรับเซสชันดีบักปกติ

```bash
bash lumi-debug-local.sh lumi-dev build
```

คำสั่งนี้จะ:

1. สร้าง Lumi ในโหมดดีบัก,
2. ตรวจสอบสัญลักษณ์ดีบัก,
3. เปิด Lumi ภายใต้ GDB

### Build ดีบักอย่างเดียว (สำหรับ TTY/เซสชัน remote ภายหลัง)

ใช้เมื่อต้องการ build ตอนนี้และเปิด/ดีบักภายหลัง

```bash
bash lumi-build-debug.sh lumi-dev build
```

## การใช้ TTY บน Linux

TTY (คอนโซลข้อความ) มักเป็นวิธีที่เชื่อถือได้ที่สุดในการดีบักการค้างแบบ hard freeze

- สลับไป TTY ด้วย `Ctrl + Alt + F1` ถึง `Ctrl + Alt + F6`
- เข้าสู่ระบบจาก prompt ข้อความ
- กลับไปเซสชันกราฟิกด้วย `Ctrl + Alt + F7` (หรือ `F2` ในบางระบบ)

เหตุผลที่สำคัญ: หากเซสชันเดสก์ท็อปค้าง TTY มักยังตอบสนอง ทำให้คุณ attach GDB จับ backtrace และเก็บข้อมูล crash ที่มีประโยชน์ได้

## ตัวเลือก: ดีบัก remote/TTY

สำหรับการค้างแบบ hard freeze หรือ display lockup ให้ใช้ `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

จากนั้นจาก TTY (แนะนำเมื่อค้าง) หรือเทอร์มินัลอื่น:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

สำหรับการเปิด GDB ในเครื่อง (ไม่ใช้ TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## หมายเหตุด้านประสิทธิภาพ

build แบบดีบักช้ากว่าโดยเจตนา เมื่อดีบักเสร็จแล้ว ให้กลับไปใช้ build ที่เร็วขึ้น:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
