---
title: "التثبيت"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
url: "hub/technical-guides/Installation"
translation_lock: true
---

تحتاج Git لخطوة الاستنساخ الأولية أدناه. إذا لم يكن Git مثبتًا بعد، ثبّته أولًا (Debian/Ubuntu: `sudo apt install git`) أو اتبع: [استخدام Git على Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) استنساخ Lumi (إعداد أولي)

أنشئ الدليل لـ Lumi واستخدم Git لاستنساخ المصدر.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# استنساخ عبر SSH (يتوافق مع دليل Git أعلاه)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# أو استنساخ عبر HTTPS (دون إعداد مفتاح SSH)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) تثبيت التبعيات (إعداد أولي)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) بناء Lumi (إعداد أولي)

أول بناء إعداد كامل (أول مرة أو بعد تغييرات كبيرة):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) تشغيل Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## اختياري: إعادة البناء / التجميع

إعادة بناء عادية بعد تغييرات الكود:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

مسار تجميع سريع فقط:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

بناء مكوّن متكامل واحد (استبدل `babl` بـ `gegl` أو `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## اختياري: أنواع البناء

استخدم `--type` عند الحاجة:

- `debug` – سير عمل التصحيح
- `debugoptimized` – الإعداد الافتراضي المتوازن للتطوير
- `release` – أسرع وقت تشغيل

مثال:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
