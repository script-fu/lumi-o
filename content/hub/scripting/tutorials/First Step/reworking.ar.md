---
title: "إعادة الصياغة"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: af1b2f3233ef50936b24aa195d3a7da50529a4fff3109b087be2f861e15496d1
translation_lock: true
url: "hub/scripting/tutorials/First Step/reworking"
---
تعالج هذه الخطوة سلوكًا دقيقًا في مثال المراسلة.

كنا نمرّر السلسلة `"Hello world!\n"` كرسالة. `\n` حرف خاص، «حرف هروب»، يخبر المخرج ببدء سطر جديد. في Scheme، قد تُجبر الرسالة المرسلة إلى شريط الحالة أيضًا على الظهور كمربع GUI.

المساعد `send-to-gui` يرسل رسائل إلى مربع حوار Lumi.

حدّث محتوى الرسالة ووجهاتها ليعمل المثال بشكل متسق.

إزالة حرف الهروب وتوسيع الوظائف:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
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

استبدل الأرقام السحرية بالثوابت التي يوفرها Lumi (مثل `MESSAGE-BOX` و`ERROR-CONSOLE`).

ثم قسّم التحقق إلى وظيفتين لإعادة استخدامهما من مواقع استدعاء متعددة:

- `(is-valid-string?)` للتحقق من أن المدخل سلسلة غير فارغة، داخل كل `send-to-*`.
- `(is-valid-output-display?)` للتحقق من صحة وجهة الإخراج، في `send-message`.

إعادة صياغة المكتبة:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; إلحاق سطر جديد لإجبار الرسالة على الظهور كمربع
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; الغرض: توجيه رسالة إلى وجهة الإخراج المناسبة
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; الغرض: التحقق من أن الرسالة سلسلة غير فارغة
(define (is-valid-string? message)
  ;; التحقق من أن الرسالة سلسلة غير فارغة
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; الغرض: التحقق من أن الرسالة تُرسَل إلى مخرج صالح
(define (is-valid-output-display? output)
  ;; التحقق من أن الإخراج إحدى وجهات العرض المتوقعة
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## الخلاصة

بإعادة صياغة مكتبة الرسائل، جعلناها أكثر متانة وموثوقية. أصلحنا مشكلة حرف السطر الجديد الخفية، وقدّمنا ثوابت للوضوح، ووسّعنا الوظائف بدعم شريط الحالة ومربع الحوار. كذلك، فصل منطق التحقق إلى وظائف أصغر ومركزة يسهل صيانة الكود وتوسيعه مستقبلًا.

توضّح إعادة الصياغة هذه كيف يمكن للتغييرات الصغيرة أن تعزز الهيكل العام للمكتبة ووظائفها، مما يمهّد لمزيد من المرونة وقابلية إعادة الاستخدام مع نمو المشروع.
