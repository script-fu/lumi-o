---
title: "مرحبًا بالعالم!"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: c250d07dff926c7b51434efc644786f35b5189e03449dcdf4ec5916c1c151886
translation_lock: true
url: "hub/scripting/tutorials/First Step/hello-world"
---
يستعرض هذا البرنامج التعليمي البنية الدنيا لـ plug-in Scheme. بعض الأسطر «نموذجية»: فهي مطلوبة حتى يتمكن Lumi من تحميل الملف، حتى لو لم تفهمها بالكامل بعد.

```bash
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

على مستوى عام، ستقوم بما يلي:

1. تعريف وظيفة
2. تسجيلها حتى تظهر في Procedure Database
3. (اختياري) إضافة إدخال قائمة
4. تثبيت الملف في مجلد plug-ins

### تعريف وظيفة

الوظيفة، المعروفة أيضًا باسم _procedure_، هي جزء من التعليمات البرمجية له اسم وغرض؛ فهي تأخذ مدخلات وتنتج مخرجات.

**المدخل** > **_الوظيفة_** > **المخرج**

### تسجيل الوظيفة

التسجيل يعني إدراج اسم الوظيفة في قائمة حتى يعرف Lumi بها.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### ربط القائمة

يخبر Lumi أين يجد وظيفتك في نظام القوائم.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

يعرض هذا القائمة «Funky» في شريط القائمة الرئيسي. غيّر المسار لوضع plug-in في مكان آخر. المسار `<Image>/Funky` يعني أن plug-in سيظهر ضمن فئة القائمة **Image**. يمكنك استبدال `<Image>` بـ `<Tools>` أو `<Filters>` وغيرها، حسب المكان الذي تريد ظهور plug-in فيه.

### التعليقات

في Scheme، تُكتَب التعليقات عادةً بإضافة `;;` قبل سطر نصي مفيد. يعتمد استخدامك للتعليقات على خبرتك كمبرمج: إذا برمجت من حين لآخر، فستفيدك تعليقات أكثر. إذا برمجت طوال الوقت، فقد يكون الكود أسهل قراءة من التعليق نفسه. كذلك، في البرمجة الوظيفية، يصبح الكود وصفيًا بما يكفي لقراءته كسكربت.

### بناء الجملة

للتعليمات البرمجية قواعد قليلة حول ترتيب العناصر في السطر، حتى يسهل قراءته. على سبيل المثال، قد تحتوي الجملة على مسافة بعد الفاصلة أو النقطة؛ وهذا يحسّن القراءة.

قد يرتّب الكود العناصر بطريقة مشابهة، وقد تبدو غريبة في البداية:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## مثال على الكود

إليك المثال الكامل. معظم إجراءات Lumi تبدأ بـ `lumi-`. على سبيل المثال، `lumi-message` يطبع سلسلة إلى معالج الرسائل المُعدّ.

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; ضبط معالج الرسائل لإخراج الرسالة إلى مربع حوار GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; ضبط معالج الرسائل لإخراج الرسالة إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; إرسال الرسالة إلى الطرفية، نافذة نظام التشغيل التي شغّلت Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### تثبيت plug-in

1. انتقل إلى **Lumi -> تحرير -> التفضيلات -> المجلدات -> Plug-ins**.
2. أضف مجلد plug-ins في [Git](/hub/scripting/tools/git) إلى القائمة.
3. أنشئ مجلدًا للـ plug-in واحفظ مثال الكود أعلاه باسم `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. انقر بزر الماوس الأيمن على الملف `hello-world.scm`.
5. انتقل إلى **الخصائص -> الأذونات -> السماح بتنفيذ الملف كبرنامج**.
6. أعد تشغيل Lumi.

### تجربة plug-in

يجب أن يظهر plug-in الآن ضمن قائمة «Funky» في النافذة الرئيسية لـ Lumi. انقر عليه، وستظهر رسالة «Hello world!». جرّب تعديل الكود، مثل تغيير نص الرسالة، واحفظ الملف. عند تشغيل plug-in مرة أخرى، ستُطبَّق التغييرات دون إعادة تشغيل Lumi.

جرّب تغيير مسار القائمة. على سبيل المثال، `"<Image>/File"` يضعه داخل قائمة File، و`"<Image>/File/Funky"` ينشئ قسمًا جديدًا في قائمة File. هذه طريقة ممتازة لتخصيص مكان ظهور plug-in وتنظيم أدواتك.
