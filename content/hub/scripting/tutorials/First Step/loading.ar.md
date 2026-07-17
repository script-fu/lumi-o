---
title: "التحميل"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
بمجرد أن تنمو وظيفة مساعدة، انقلها إلى ملف مكتبة صغير. يبقي ذلك plug-in مركزًا ويجعل المساعد قابلًا لإعادة الاستخدام عبر plug-ins متعددة.

### إنشاء وظيفة مكتبة

يمكننا أخذ وظيفة `send-message` وإنشاء ملف جديد يحتويها. احفظ الملف في مجلد مستودع Git، وليس في مجلد plug-ins، ربما قرب المستوى الأعلى:

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: الدليل الرئيسي لتخزين كود Scheme.
  - **library/**: حيث توجد الوظائف المشتركة مثل `send-message.scm`.
  - **plug-ins/**: حيث تُخزَّن plug-ins الفردية.
    - **hello-world/**: مجلد plug-in «Hello World!».
      - **hello-world.scm**: ملف السكربت للـ plug-in.

مثال على وظيفة المكتبة send-message.scm

```scheme
;; وظيفة لمعالجة إخراج الرسائل إلى وجهات مختلفة
(define (send-message message output)
  (cond
    ;; الإرسال إلى وحدة تحكم الأخطاء
    ((eq? output 'error-console)
       ;; ضبط المعالج على وحدة تحكم الأخطاء
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; الإرسال إلى مربع حوار GUI
    ((eq? output 'gui)
       ;; ضبط المعالج على مربع حوار GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; الإرسال إلى نافذة الطرفية
    ((eq? output 'terminal)
       ;; إخراج الطرفية يُعالَج بـ display
       (display message)))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))
```

### تحميل وظيفة المكتبة

يمكننا تحميل وظيفة المكتبة بأمر Scheme `load`:

تحميل ملف مكتبة:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

رائع! أصبح لدينا شيء أبسط وأقصر للقراءة، يصف نفسه تقريبًا دون تعليقات. هذه نتيجة مرضية لإعادة الهيكلة.
