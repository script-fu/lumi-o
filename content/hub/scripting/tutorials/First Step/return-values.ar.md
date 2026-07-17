---
title: "قيم الإرجاع"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
translation_lock: true
url: "hub/scripting/tutorials/First Step/return-values"
---
قيم الإرجاع مهمة لأنها تتيح التحكم في التدفق دون حالة إضافية. في Scheme، آخر تعبير يُقيَّم يصبح القيمة المرجعة.

تستخدم هذه الصفحة مساعدي التحقق من مثال المراسلة لإظهار كيف تجعل قيم الإرجاع الصريحة بناء الكود أسهل.

### ما هي قيمة الإرجاع؟

في Scheme، تُحدَّد قيمة إرجاع الوظيفة بالتعبير الأخير الذي تُقيِّمه. أي شيء يُقيَّم في السطر الأخير من كود الوظيفة يُعاد كناتج. إذا لم تُرجَع قيمة صراحة، تُعيد الوظيفة `#f` (خطأ) أو `undefined`.

لنراجع وظيفة التحقق (هل هي سلسلة صالحة؟):

```scheme
;; الغرض: التحقق من أن الرسالة سلسلة غير فارغة
(define (is-valid-string? message)
  ;; التحقق من أن الرسالة سلسلة غير فارغة
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

في هذه الوظيفة، إذا كانت الرسالة غير صالحة، يُطلَق خطأ. أما إذا كانت صالحة، فلا تُعطى قيمة إرجاع صريحة، فتُعيد `#f` افتراضيًا.

### جعل قيم الإرجاع واضحة

يمكننا تحسين ذلك بجعل قيمة الإرجاع أوضح. على سبيل المثال، نُعيد `#t` (صح) إذا كانت الرسالة صالحة:

```scheme
;; الغرض: التحقق من أن الرسالة تُرسَل إلى مخرج صالح
(define (is-valid-output-display? output)
  ;; التحقق من أن الإخراج إحدى وجهات العرض المتوقعة
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

في هذا الإصدار، تُعيد `#t` عندما يكون الإخراج صالحًا، فيوفّر نتيجة واضحة. يسهل استخدام الوظيفة في سياقات أخرى تحتاج نتيجة منطقية.

### استخدام قيم الإرجاع بفعالية

بتحديد ما تُرجعه وظائفنا، تصبح أكثر قابلية للتنبؤ وفائدة. إرجاع `#t` أو `#f` أو نتيجة محددة يمنحنا تحكمًا أكبر في تفاعل الوظيفة مع بقية الكود. يمكن استخدام القيمة المرجعة لاتخاذ قرارات في الوظيفة المستدعية أو تمريرها كوسيطة لوظيفة أخرى.

مثال بسيط لاستخدام قيمة الإرجاع للتحكم في تدفق المنطق:

```scheme
;; الغرض: توجيه رسالة إلى وجهة الإخراج المناسبة
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

هنا، يعتمد `send-message` على القيمة المرجعة من `is-valid-output-display?` لتحديد ما إذا كان سيستمر.
يُتخطَّى `cond` إذا فشل الفحص الأول. لاحظ أيضًا كيف يُقرأ تقريبًا: «إذا كان عرض الإخراج صالحًا؟»

## منطق if في Scheme

قبل مثال المكتبة المُعاد هيكلتها، مراجعة سريعة للمنطق الشرطي. يستخدم Scheme `if` للاختيار بين مسارين.

نموذج بسيط لعبارة `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

تتحقق هذه البنية من الشرط؛ إن كان صحيحًا، تنفّذ الإجراء الأول. وإن كان خاطئًا، تنفّذ الثاني.

عند الحاجة لتنفيذ إجراءات متعددة عندما يكون الشرط صحيحًا أو خاطئًا، استخدم `begin` لتجميعها:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

يتيح ذلك التعامل مع مواقف أكثر تعقيدًا، حيث يلزم تنفيذ تعبيرات أو عبارات متعددة حسب نتيجة الاختبار.

إليك كود المكتبة مع قيم الإرجاع المضمّنة والمستخدمة للتحكم في التنفيذ.

### إعادة الهيكلة باستخدام قيم الإرجاع

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

قيم الإرجاع جزء أساسي من جعل الوظائف مرنة وقابلة لإعادة الاستخدام. بتحديد ما يجب أن تُرجعه كل وظيفة بعناية، نضمن تفاعلها جيدًا مع بعضها وتوفير معلومات مفيدة لبقية الكود. سواء أُعيد `#t` أو `#f` أو شيء أكثر تحديدًا، فإن قيم الإرجاع تمنحنا طريقة للتحكم في تدفق البرنامج والتعامل مع النتائج المختلفة.
