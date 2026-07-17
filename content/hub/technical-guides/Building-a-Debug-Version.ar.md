---
title: "بناء نسخة تصحيح"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
url: "hub/technical-guides/Building-a-Debug-Version"
translation_lock: true
---

يصف هذا الدليل **سير عمل التصحيح المحلي** لـ Lumi باستخدام السكربتات في `build/lumi/scripts`.

صُمّم سير العمل لـ:

- استخدام مخرجات البناء المحلية (دون الحاجة إلى تنزيل الرموز)،
- التحقق من وجود رموز التصحيح فعليًا،
- تشغيل GDB بوضع الرموز دون اتصال افتراضيًا.

## المتطلبات الأساسية

- Linux مبني على Debian (خط أساس المشروع: Debian 13)
- شجرة مصدر Lumi مستنسخة مسبقًا

## إعداد GDB لمرة واحدة (اختياري لكن موصى به)

ثبّت أدوات GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

إعداد تسجيل محلي اختياري:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

ملاحظة: تعطّل سكربتات التصحيح المحلية في Lumi `debuginfod` افتراضيًا للإبقاء على حل الرموز محليًا وقابلًا للتكرار.

## بداية سريعة

من دليل السكربتات:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### بناء تصحيح + تشغيل (افتراضي)

استخدم هذا لجلسات التصحيح العادية.

```bash
bash lumi-debug-local.sh lumi-dev build
```

ينفّذ هذا الأمر:

1. بناء Lumi في وضع التصحيح،
2. التحقق من رموز التصحيح،
3. تشغيل Lumi تحت GDB.

### بناء تصحيح فقط (لجلسة TTY/بعيدة لاحقة)

استخدم هذا عندما تريد البناء الآن والتشغيل/التصحيح لاحقًا.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## استخدام TTY في Linux

غالبًا ما تكون وحدات TTY (الطرفيات النصية) الطريقة الأكثر موثوقية لتصحيح التجمّد الشديد.

- انتقل إلى TTY بـ `Ctrl + Alt + F1` حتى `Ctrl + Alt + F6`
- سجّل الدخول من موجه النص
- عد إلى الجلسة الرسومية بـ `Ctrl + Alt + F7` (أو `F2` في بعض الأنظمة)

لماذا يهم ذلك: إذا توقفت جلسة سطح المكتب، غالبًا ما يستمر TTY بالاستجابة، فيمكنك إرفاق GDB والتقاط backtrace واستعادة بيانات مفيدة عن العطل.

## اختياري: تصحيح بعيد/عبر TTY

للتجمّد الشديد أو تعلّق العرض، استخدم `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

ثم من TTY (موصى به لسيناريوهات التجمّد) أو من طرفية أخرى:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

لتشغيل GDB محليًا (مسار غير TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## ملاحظة الأداء

بناءات التصحيح أبطأ بالتصميم. عند انتهاء التصحيح، عد إلى بناء أسرع:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# إعادة ضبط كاملة لجميع المكوّنات الرئيسية إلى الإصدار
bash lumi-debug-reset-release.sh lumi-dev

# متغيّر محلي أسرع اختياري
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
