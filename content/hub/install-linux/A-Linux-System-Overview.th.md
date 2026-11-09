---
title: "ภาพรวมระบบ Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux เป็นระบบปฏิบัติการที่ทรงพลังและอเนกประสงค์ พร้อมชุมชนนักพัฒนาขนาดใหญ่ โดยพื้นฐานแล้ว ระบบ Linux ประกอบด้วยองค์ประกอบสำคัญหลายส่วนที่ทำงานร่วมกันเพื่อมอบประสบการณ์ผู้ใช้ที่ราบรื่น ภาพรวมนี้อธิบายส่วนสำคัญของระบบ Linux ได้แก่ kernel ดิสทริบิวชัน ตัวจัดการแพ็กเกจ display manager สภาพแวดล้อมเดสก์ท็อป และ display server (X11 หรือ Wayland)

Lumi พัฒนาและทดสอบบน Debian ด้วย Cinnamon (X11) นั่นยังคงเป็นสภาพแวดล้อมของการพัฒนา ทดสอบ GNOME บน Wayland ควบคู่กันแล้ว ทั้งรูปลักษณ์โดยรวมและการเรนเดอร์ OpenGL

**ค่าเริ่มต้นทั่วไปของดิสทริบิวชัน Linux ในปัจจุบัน**

| **ดิสทริบิวชัน** | **ตัวจัดการแพ็กเกจ** | **Display Manager** | **สภาพแวดล้อมเดสก์ท็อป** | **Display Server** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | ตามที่ผู้ใช้เลือก     | ตามที่ผู้ใช้เลือก        | ตามที่ผู้ใช้เลือก   |

### คำศัพท์สำคัญ

#### Kernel

แกนของระบบปฏิบัติการที่เชื่อมต่อกับฮาร์ดแวร์โดยตรง — โดยทั่วไปคือ Linux

#### ดิสทริบิวชัน

ดิสทริบิวชัน Linux รวม kernel เข้ากับเครื่องมือ ไลบรารี และซอฟต์แวร์ใน user space ตัวอย่างเช่น Debian, Arch Linux และ Fedora

#### ตัวจัดการแพ็กเกจ

เครื่องมือสำหรับติดตั้ง อัปเดต และลบแอปพลิเคชันจาก repository ตัวอย่างเช่น APT สำหรับดิสทริบิวชันแบบ Debian, DNF สำหรับ Fedora และ Pacman สำหรับ Arch Linux

#### ตัวจัดการการแสดงผล

จัดการหน้าจอเข้าสู่ระบบแบบกราฟิกและการเริ่มเซสชัน ตัวอย่างเช่น GDM (GNOME Display Manager), LightDM และ SDDM (Simple Desktop Display Manager)

#### สภาพแวดล้อมเดสก์ท็อป

ให้ graphical user interface (GUI) และจัดการรูปลักษณ์โดยรวมกับประสบการณ์ผู้ใช้ ตัวอย่างเช่น GNOME, Cinnamon และ KDE Plasma

#### เซิร์ฟเวอร์การแสดงผล

จัดการเอาต์พุตภาพและเหตุการณ์อินพุต ตัวอย่างเช่น X11 (X Window System) และ Wayland. X11 เป็น display server แบบดั้งเดิม ส่วน Wayland เป็นทางเลือกที่ใหม่กว่าและปลอดภัยกว่า
