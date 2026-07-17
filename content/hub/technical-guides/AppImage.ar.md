---
title: "AppImage"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
url: "hub/technical-guides/AppImage"
translation_lock: true
---

AppImage حزمة تطبيق Linux في ملف واحد. تنزّل ملفًا واحدًا، تجعله قابلاً للتنفيذ، وتشغّله دون تثبيت البرنامج على مستوى النظام.

الموقع الرسمي لـ AppImage: https://appimage.org/

يوفر AppImage نسخة محمولة من Lumi تعمل دون تثبيت أو تعديل للنظام. مثالي للفنانين الذين يريدون استخدام البرنامج فورًا دون إدارة التبعيات أو تجميع المصدر أو إعداد بيئة تطوير.

باعتباره ملفًا تنفيذيًا مستقلًا، يمكن تخزين AppImage في أي مكان على النظام. يسهّل ذلك اختبار إصدارات جديدة أو الاحتفاظ بعدة إصدارات أو نقل البرنامج بين الأجهزة.

في عملية تطوير Lumi، يعمل AppImage كبناء اختبار محمول يطابق مخرجات التكامل المستمر عن كثب. يتيح ذلك اختبارًا موثوقًا في بيئة متسقة مع الإبقاء على بناء المصدر المحلي مركّزًا على التطوير.

ملاحظة: يبني CI ملف AppImage باستخدام مصادر التبعيات المدمجة في Lumi (BABL/GEGL/GTK3)، لذا تكون حزمة التبعيات متوافقة مع سير عمل `lumi-build-script.sh` المحلي.

## AppImage للإصدار مقابل التطوير

- **AppImage للإصدار**: غير متاح بعد (لم يُصدَر Lumi بعد).
- **AppImage للتطوير (مخرج CI)**: يُنشأ تلقائيًا من التزامات التطوير الجارية للاختبار.

يغطي هذا الدليل أساسًا سير عمل **AppImage للتطوير**.

صفحة المخرجات الحالية:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## أساسيات تنزيل AppImage من CI

ينتج CI ملفات zip للمخرجات (مثل `lumi-appimage*.zip`).

التدفق اليدوي الأساسي:

1. نزّل أحدث ملف zip لمخرج CI.
2. فكّ ضغطه.
3. شغّل ملف `Lumi*.AppImage` المضمّن.

السكربتات أدناه أدوات مساعدة اختيارية تؤتمت هذه الخطوات.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# فك أحدث zip من CI تم تنزيله إلى ~/Downloads
bash lumi-appimage-unpack-zip.sh

# تشغيل AppImage مع إخراج الطرفية
bash lumi-appimage-launch.sh
```

## سكربتات مساعدة اختيارية

- `lumi-appimage-unpack-zip.sh`
  - يعثر على أحدث `lumi-appimage*.zip` في `~/Downloads`
  - يثبّت AppImage في `~/AppImage/Lumi/Lumi_CI.AppImage`
  - يثبّت موارد سطح المكتب في `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - يشغّل AppImage في الطرفية
  - يفعّل إخراج وقت التشغيل (`APPIMAGE_DEBUG=1`)

## ملاحظات شائعة

- إذا شغّلت AppImage يدويًا (دون السكربتات المساعدة)، اجعله قابلاً للتنفيذ أولًا:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` يطبّق أذونات التنفيذ تلقائيًا.

- إذا كان Lumi يعمل بالفعل من بناء آخر، أغلقه قبل تشغيل AppImage.
