---
title: "مكتبة الرسائل"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
translation_lock: true
url: "hub/scripting/tutorials/First Step/messaging-library"
---
مع مرور الوقت، تطوّر ما بدأ كوظيفة واحدة لإرسال الرسائل إلى مجموعة من الوظائف المترابطة. تشكل هذه الوظائف الآن أساس **مكتبة الرسائل**، المصممة للتعامل مع الإخراج إلى وجهات مختلفة: واجهة GUI ووحدة تحكم الأخطاء وطرفية نظام التشغيل.

### لماذا مكتبة رسائل؟

مع نمو احتياجاتنا، يتطلب التعامل مع الرسائل عبر مخرجات متعددة نهجًا أكثر نمطية وقابلية للتوسع. بدل وظيفة واحدة تفعل كل شيء، قسّمنا العملية إلى مكونات قابلة لإعادة الاستخدام. يمكن الآن استخدام هذه المكتبة كأداة مراسلة عامة يستعير منها plug-ins أو وظائف أخرى.

### ماذا تفعل مكتبة الرسائل؟

تتضمن مكتبة الرسائل حاليًا الوظائف التالية:

- **send-to-gui**: يرسل رسائل إلى مربع حوار GUI في Lumi.
- **send-to-error-console**: يرسل رسائل إلى وحدة تحكم الأخطاء في Lumi.
- **send-to-terminal**: يرسل رسائل إلى نافذة الطرفية.
- **send-message**: وظيفة مرسل توجّه الرسائل إلى المخرج المناسب.
- **التحقق من الرسالة**: التأكد من صحة الرسالة والمخرج قبل الإرسال.

### توسيع المكتبة

يمكن توسيع **مكتبة الرسائل** بسهولة لدعم مخرجات إضافية. على سبيل المثال:

- **send-to-file**: حفظ الرسائل في ملف سجل.
- **send-to-logger**: التكامل مع نظام تسجيل خارجي.
- **send-to-notifications**: عرض الرسائل كإشعارات نظام.

باتباع نفس نمط التصميم النمطي والوظائف القابلة لإعادة الاستخدام، يمكن أن تنمو المكتبة لتصبح أداة شاملة لجميع مهام المراسلة.

## فوائد مكتبة الرسائل

- **قابلية إعادة الاستخدام**: الوظائف قابلة لإعادة الاستخدام عبر plug-ins أو مشاريع مختلفة.
- **النمطية**: كل وظيفة تتولى مهمة واحدة محددة، فيسهل صيانة الكود وتوسيعه.
- **الاتساق**: استخدام نفس وظائف التحقق ومعالجة الرسائل يضمن سلوكًا متسقًا عبر التطبيق.

**مكتبة الرسائل** بداية لإطار أوسع يبسّط إدارة الرسائل في مشروعك. مع نمو المكتبة، يمكن للـ plug-ins الجديدة الاستفادة منها بسهولة لإرسال الرسائل حيثما شئت.

يمكننا ضبط بنية الملف:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

وتذكر ضبط `load` في plug-in الرئيسي:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

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
