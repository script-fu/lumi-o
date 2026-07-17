---
title: "plug-in التصفية"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
translation_lock: true
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
استخدمنا plug-in _procedure_ في برنامج [First Step](../../first-step/) التعليمي. تعمل plug-ins من هذا النوع دون الحاجة إلى صورة أو drawable كمدخل. عادةً نستخدم plug-in لتغيير صورة وdrawables الخاصة بها. تُسمى plug-ins من هذا النوع plug-ins _filter_.

### ما هو drawable؟

**drawable** في Lumi يشير إلى عنصر صورة يمكن الرسم عليه، مثل طبقة أو قناة. تعمل plug-ins التصفية عادةً على هذه العناصر.

### مثال بسيط على plug-in تصفية

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; استخدام let لتعريف متغير الرسالة والكود الأساسي
  (let ((message "hello, world"))
    ;; عرض الرسالة في وحدة تحكم الأخطاء في Lumi
    (lumi-message message)
    ;; عكس ألوان أول drawable محدّد
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; تسجيل plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; اسم الإجراء الرئيسي
  "Simple Filter Plug-in Demo"             ;; الاسم كما يظهر في قائمة Lumi
  "Tests a basic Scheme filter plug-in"    ;; وصف التلميح
  "Author Name"                            ;; اسم المؤلف
  "License"                                ;; الترخيص
  "Date written"                           ;; تاريخ الكتابة
  "*"                                      ;; يشير إلى أن plug-in يتطلب صورة
  SF-ONE-OR-MORE-DRAWABLE)                 ;; يتطلب drawable واحدًا أو أكثر محدّدًا

;; تحديد موقع القائمة لـ plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

انسخ النص واحفظه باسم `simple-filter-plug-in.scm` في مجلد `simple-filter-plug-in` داخل أحد مجلدات plug-ins في Lumi. مجلد plug-ins في Lumi هو _أي_ مجلد مدرج ضمن:
 **Lumi > تحرير > التفضيلات > المجلدات > Plug-ins**

في Linux، انقر بزر الماوس الأيمن على `simple-filter-plug-in.scm`، انتقل إلى **الخصائص > الأذونات**، وفعّل **السماح بتنفيذ الملف كبرنامج**. بمجرد أن يكون الملف في المكان الصحيح وقابلًا للتنفيذ وخاليًا من أخطاء بناء الجملة، عند إعادة تشغيل Lumi سيظهر في شريط القائمة العلوي ضمن قائمة **Plug-in**.

### تشغيل plug-in

1. افتح صورة (يتطلب plug-in التصفية هذا صورة ليعمل).
2. افتح **Tools > Debug > وحدة تحكم الرسائل** لرؤية الرسالة.
3. اختر **Simple Filter Plug-in Demo** من قائمة **Plug-in**.
4. تُعكَس ألوان إحدى الطبقات المحددة وتُطبَع رسالة في وحدة تحكم الأخطاء.

### تحرير plug-in

يمكنك تخصيص plug-in بتحرير ملف `.scm`. على سبيل المثال، لتغيير الرسالة المعروضة:

1. افتح الملف وحدّد السطر الذي يعرّف `message`.
2. استبدل `"hello, world"` بنصك.
3. احفظ الملف.

في Lumi الإصدار 3، لا تحتاج plug-ins لإعادة التحميل حتى تُطبَّق التغييرات المحفوظة. أعد تشغيل plug-in فقط لرؤية الرسالة المحدّثة.

### فحص plug-in

#### سطر shebang

يضمن السطر الأول أن السكربت يعمل كـ plug-in في Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### تعريف الإجراء

يقبل الإجراء وسيطتين: الصورة النشطة والdrawables المحددة.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### المنطق الأساسي

عبارة `let` تعرّف متغيرًا وتنفّذ عمليات على drawable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; عرض رسالة في وحدة تحكم الأخطاء في Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; عكس ألوان أول drawable محدّد
```

### تسجيل plug-in

يُسجَّل plug-in في Lumi كـ plug-in filter:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; تسجيل الإجراء الرئيسي
  "Simple Filter Plug-in Demo"             ;; الاسم كما يظهر في قائمة Lumi
  "Tests a basic Scheme filter plug-in"    ;; وصف التلميح
  "Author Name"                            ;; اسم المؤلف
  "License"                                ;; نوع الترخيص
  "Date written"                           ;; تاريخ الكتابة
  "*"                                      ;; يشير إلى أن plug-in يتطلب صورة
  SF-ONE-OR-MORE-DRAWABLE)                 ;; يتطلب drawable واحدًا أو أكثر محدّدًا
```

#### تسجيل القائمة

يحدّد هذا السطر موقع القائمة لـ plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### استكشاف الأخطاء وإصلاحها

إذا لم يظهر plug-in، تحقق من موقعه واسمه وخاصية التنفيذ.

يجب أن يكون الموقع في مسار بحث plug-in.
يجب أن يتطابق اسم الملف مع اسم المجلد الذي يحتويه.
يجب تعيين الملف كقابل للتنفيذ.


**وحدة تحكم الرسائل** أداة قيمة لاستكشاف أخطاء plug-ins المخصصة وإصلاحها. إذا لم يعمل plug-in كما هو متوقع، تحقق هنا من رسائل الخطأ أو السجلات. نافذة **Terminal** قد توفر أيضًا معلومات تصحيح وتبلغ عن مشكلات التحميل.
