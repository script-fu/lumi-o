---
title: "التحقق"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
translation_lock: true
url: "hub/scripting/tutorials/First Step/validation"
---
عند بناء plug-ins قوية، من المهم أن تتعامل وظائفنا مع الأخطاء بأمان وتعمل كما هو متوقع، حتى عند سوء الاستخدام أو المدخلات غير المتوقعة. يساعد التحقق على حماية سلامة الوظيفة ومنع الأعطال أو السلوك غير المقصود.

لنرى كيف نحسّن وظيفة `send-message` بإضافة فحوصات تحقق تضمن معالجة المدخلات بشكل صحيح.

### التحقق من المدخلات

قبل إرسال رسالة، يجب التأكد من صحة الوسيطة `output` المُمرَّرة إلى `send-message`. يمكننا إضافة فحص يؤكد أن وجهة الإخراج إحدى القيم المتوقعة (`gui` أو `error-console` أو `terminal`).

مثال:

```scheme
(define (send-message message output)
  ;; التحقق من وسيطة الإخراج
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; الإرسال إلى وحدة تحكم الأخطاء
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; الإرسال إلى مربع حوار GUI
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; الإرسال إلى نافذة الطرفية
      ((eq? output 'terminal)
         (display message))))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))
```

في هذا المثال، نستخدم `member` للتحقق من صحة الوسيطة `output`. وإلا، تُطلِق الوظيفة خطأ برسالة واضحة، فتمنع القيم غير الصالحة من التسبب بمشاكل.

### التعامل مع الرسائل الفارغة

من المفيد أيضًا التأكد من صحة الوسيطة `message`. على سبيل المثال، إذا مُرِّرت سلسلة فارغة أو `#f` (خطأ) كرسالة، يجب أن تتعامل الوظيفة مع ذلك بأمان.

مثال على التعامل مع رسالة فارغة:

```scheme
(define (send-message message output)
  ;; التحقق مما إذا كانت الرسالة فارغة
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

يضمن هذا أن الوظيفة تتلقى دائمًا مدخلات صالحة، فيتحسّن الاعتمادية ويُمنع السلوك غير المتوقع.

### مثال تحقق مدمج

```scheme
;; وظيفة لمعالجة إخراج الرسائل إلى وجهات مختلفة
(define (send-message message output)

  ;; التحقق من وسيطتي الرسالة والإخراج
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; الإرسال إلى وحدة تحكم الأخطاء
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; الإرسال إلى مربع حوار GUI
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; الإرسال إلى نافذة الطرفية
        ((eq? output 'terminal)
           (display message)))))

  ;; استعادة معالج الرسائل الافتراضي إلى وحدة تحكم الأخطاء
  (lumi-message-set-handler 2))
```

في هذا الإصدار:
- تتحقق الوظيفة أولًا مما إذا كان `message` فارغًا أو غير صالح. إذا كانت الرسالة صالحة، تنتقل للتحقق مما إذا كان `output` إحدى القيم المقبولة (`gui` أو `error-console` أو `terminal`).
- إذا نجح الفحسان، تُرسَل الرسالة إلى المخرج المناسب. وإلا، يُطلَق خطأ مع شرح واضح.
- يُجرى فحص إضافي للتأكد من أن الرسالة سلسلة أيضًا.

تحافظ وظيفة التحقق المدمجة هذه على نظافة الكود وتضمن التحقق من كلا المدخلين قبل أي إجراء، فتصبح الوظيفة أكثر متانة. لاحظ أيضًا أننا نبني نظام رسائل للتصحيح: عندما
يفشل الكود، نحصل على سبب كتبناه بأنفسنا.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```
