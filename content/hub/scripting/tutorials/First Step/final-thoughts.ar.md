---
title: "خاتمة"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
translation_lock: true
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
لديك الآن plug-in procedure ومكتبة مساعدة صغيرة. قدّمت هذه السلسلة الأنماط الأساسية التي ستستخدمها في معظم سكربتات Lumi:

- **الوظائف**: اللبنات الأساسية لـ plug-ins.
- **إعادة الهيكلة**: تحسين بنية الكود مع الحفاظ على السلوك.
- **مكتبات الكود**: مركزية الوظائف القابلة لإعادة الاستخدام للحفاظ على الكود نظيفًا ونمطيًا.
- **تقنيات التحقق**: ضمان صحة المدخلات قبل تنفيذ المنطق الأساسي.

رأيت أيضًا أساسيات استخدام Git لتتبع التغييرات والحفاظ على بنية مشروع نظيفة. يسهّل هذا سير العمل التكراري دون فقدان إصدارات عاملة.

إليك الإصدار النهائي من كود plug-in الرئيسي:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

كود المكتبة:

```scheme
;; الغرض: إرسال رسالة إلى شريط الحالة، يُعيد #t عند النجاح
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; الغرض: إرسال رسالة إلى مربع الحوار، يُعيد #t عند النجاح
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; الغرض: إرسال رسالة إلى وحدة تحكم الأخطاء، يُعيد #t عند النجاح
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; الغرض: إرسال رسالة إلى الطرفية، يُعيد #t عند النجاح
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; الغرض: توجيه رسالة إلى المخرج المناسب، يُعيد #t عند النجاح
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; الغرض: التحقق من أن الرسالة سلسلة غير فارغة، يُعيد #t إذا كانت صالحة
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; الغرض: التحقق من أن الإخراج وجهة صالحة، يُعيد #t إذا كان صالحًا
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## الخلاصة

بإعادة هيكلة مساعدي المراسلة في مكتبة صغيرة، يبقى plug-in مركزًا على النية وتحتوي المكتبة على تفاصيل التنفيذ. التحقق والتوجيه المتسق للرسائل يجعلان الفشل قابلًا للتنبؤ.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

الخطوات التالية:

- انقل المساعدين القابلين لإعادة الاستخدام إلى ملف مكتبة مخصص.
- أبقِ plug-ins صغيرة وحدّد الإجراءات حسب ما تفعله.
- أضف التحقق عند الحدود (المدخلات ومسارات الملفات وخيارات القائمة).

احتفظ بالنتيجة النهائية كملفين في مستودع plug-ins:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`
