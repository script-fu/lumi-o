---
title: "الملفات"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
url: "hub/scripting/tutorials/files"
translation_lock: true
---
العمل مع الملفات والأدلة ضروري لتطوير Scheme. سواء كنت تحفظ المخرجات أو تحمّل الموارد أو تنظّم بنية مشروعك، فإن فهم عمليات الملفات يجعل سكربتاتك أكثر متانة وسهولة في الاستخدام.

تغطي هذه الصفحة مهام الملفات والأدلة الشائعة: قراءة المسارات وإنشاء الأدلة وجمع مدخلات المجلدات عبر معلمات واجهة GUI.

## الدليل الرئيسي للمستخدم

Lumi يعمل على Linux فقط، لذا يأتي الدليل الرئيسي للمستخدم من متغير البيئة `HOME`.

للحصول على الدليل الرئيسي كسلسلة:

```scheme
(getenv "HOME")
```

مثال على المخرج:

```scheme
"/home/username"
```

## DIR-SEPARATOR

يوجد أيضًا المتغير العام `DIR-SEPARATOR`، وهو فاصل المسار الخاص بالمنصة. في Lumi (Linux)، يكون دائمًا `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## الحصول على موقع دليل

يمكننا طلب موقع دليل من المستخدم في حوار Scheme لـ plug-in.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

يوفر `SF-DIRNAME` متصفحًا لاختيار دليل.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ؛؛ ...
    ))
```

هنا نتحقق من مدخلي الدليل (المصدر والوجهة) ونعود إلى القيم الافتراضية إذا كانت مسارات GUI فارغة أو غير صالحة.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

إذا اهتممت بتفاصيل التنفيذ، ابحث في مصدر plug-in عن `validate-path-and-dir`.

## إنشاء دليل

يوفر Scheme الأمر `dir-make` لإنشاء دليل. يأخذ مسارًا مفصولًا بـ `/` وينشئ دليلًا واحدًا، مع معلمة اختيارية للامتيازات. لا نمرّر مسارات خاصة بمنصة.

عادة نحتاج لإنشاء أدلة متعددة لمسار عملي. يمكننا استخدام غلاف لـ `dir-make` للمساعدة:

```scheme
;; الغرض: غلاف لـ (dir-make) ينشئ مسارًا معيّنًا من مسار
;;          مُزوَّد من المنصة. يستخدم دائمًا فواصل Linux لـ dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; الدليل الجذري
    ;; إنشاء بقية الأدلة خطوة بخطوة
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; بناء المسار
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

ملاحظة: تستخدم هذه الوظيفة أيضًا `file-exists?` المدمجة لتخطي الاستدعاءات غير الضرورية. تُعيد `#t` إذا كان الملف أو الدليل موجودًا، و`#f` إذا لم يكن موجودًا أو لم يكن المستخدم قادرًا على الوصول إليه.

## بناء المسار

نحتاج أيضًا لتفكيك المسارات وإعادة بنائها في Scheme.

لتقسيم مسار إلى أجزاء، استخدم `strbreakup`:

### أمثلة مسارات Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> ملاحظة: الشرطات المائلة البادئة واللاحقة تصبح عناصر سلسلة فارغة في القائمة الناتجة.

لإعادة بناء مسار، استخدم `string-append`:

### بناء مسار Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
