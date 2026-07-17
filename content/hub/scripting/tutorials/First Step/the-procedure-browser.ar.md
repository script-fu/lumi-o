---
title: "Procedure Browser"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: f2ea095c0407f9641d28803e937a992e044584f6bcbed960239d0c0df4b430d2
url: "hub/scripting/tutorials/first-step/the-procedure-browser"
translation_lock: true
---
يتيح لك **Lumi Procedure Browser** البحث في الإجراءات المتاحة (المضمّنة وتلك التي توفرها plug-ins) وفحص معلماتها وقيم إرجاعها.

### أين تجد Lumi Procedure Browser

يمكنك الوصول إلى Procedure Browser في Lumi من قائمة **Help**:

- **Help** -> **Procedure Browser**

### ماذا يفعل Procedure Browser

يسرد Procedure Browser جميع إجراءات Lumi الداخلية، بالإضافة إلى تلك التي أضافتها plug-ins، بما في ذلك الذي ثبّتته للتو. يقدّم كل إدخال إجراء معلومات مفيدة، منها:

- اسم الإجراء.
- وصف لما يفعله.
- المعلمات التي يقبلها (قيم المدخلات).
- قيم الإرجاع (المخرجات).

ابحث بكلمة مفتاحية أو باسم الإجراء عندما تحتاج للتحقق من توقيع الاستدعاء أو تأكيد الاسم الدقيق.

#### (lumi-message) في Procedure Browser

ابحث عن `lumi-message` لرؤية معلماته وقيم إرجاعه.

### العثور على plug-in الخاص بك

بعد تثبيت plug-in «Hello World!»، ستجده مدرجًا في Procedure Browser. ابحث عن اسم الوظيفة التي سجّلتها مع Lumi، في هذه الحالة `"scheme-hello-world"`. يعرض الإدخال المعلمات وأي قيم إرجاع مرتبطة بالـ plug-in، مع وصف مختصر. ستجد أيضًا بعض أسطر النص التي أدخلتها كمعلمات أثناء التسجيل ضمن قسم **Additional Information**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; اسم الإجراء
  "Hello world!"                                        ;; اسم عنصر القائمة
  "A Scheme procedure plug-in"                       ;; تلميح وأداة وصف
  "Your Name"                                           ;; المؤلف
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; الترخيص
  "2024")                                               ;; تاريخ حقوق النشر
```

يسهل هذا التحقق من تسجيل plug-in بشكل صحيح ويمنحك طريقة سريعة لمراجعة تفاعله مع إجراءات Lumi الأخرى. Procedure Browser أداة قوية لتصحيح الأخطاء وتوسيع plug-ins باستكشاف جميع الإجراءات المتاحة في Lumi.
