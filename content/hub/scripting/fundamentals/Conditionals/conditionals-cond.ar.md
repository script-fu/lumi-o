---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
في Scheme، يُستخدم الشرط `cond` لاختيار إحدى عدة كتل محتملة من التعليمات البرمجية للتنفيذ، بناءً على اختبارات متعددة. يشبه `if` متعدد الفروع، حيث يُفحص كل فرع بالترتيب حتى يُعثر على تطابق.

### بناء الجملة

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- يُقيَّم كل اختبار حسب ترتيب كتابته.
- عندما يُقيَّم اختبار إلى true (`#t`)، تُنفَّذ **consequent** المقابلة له، ويتوقف تعبير `cond` عن تقييم المزيد من الاختبارات.
- جملة `else` اختيارية وتعمل كبديل إذا لم يُقيَّم أي اختبار إلى true.

### كيف يعمل

1. **اختبار كل شرط**:
   - يُقيّم `cond` الاختبارات بالترتيب الذي وُضعت به.

2. **تنفيذ consequent المطابقة**:
   - عند العثور على أول اختبار يُقيَّم إلى true (`#t`)، تُنفَّذ **consequent** الخاصة به.
   - إذا لم يُقيَّم أي اختبار إلى true وكانت هناك جملة `else`، تُنفَّذ **fallback-consequent**.

### أمثلة

#### مثال 1: consequents من تعبير واحد

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- الاختبار الأول `(< 3 2)` يُقيَّم إلى false (`#f`).
- الاختبار الثاني `(= 3 3)` يُقيَّم إلى true (`#t`)، لذا تُرجع `"This will run"`.
- لم تُنفَّذ جملة `else` لأنه وُجد تطابق بالفعل.

النتيجة: **"This will run"**

#### مثال 2: إجراءات متعددة باستخدام `begin`

عندما تتضمن consequent إجراءات متعددة، استخدم `begin` لتجميعها:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- الاختبار الأول `(< 5 3)` يُقيَّم إلى false (`#f`).
- الاختبار الثاني `(> 5 3)` يُقيَّم إلى true (`#t`):
  - يطبع `"Condition met"`.
  - ثم يحسب `(* 5 5)` ويُرجع `25`.

النتيجة: **تطبع « Condition met » وتُرجع 25.**

#### مثال 3: استخدام كتلة `let` في consequent

عندما تحتاج إلى متغيرات محلية، استخدم كتلة `let`:

```scheme
(cond
  ;; الحالة 1: إذا كان 0 أقل من -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; الحالة 2: إذا كان 0 أكبر من -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; الحالة الافتراضية: إذا لم يتحقق أي من الشروط أعلاه
  (else
    (let ((z 0))
      z)))
```

- الاختبار الأول `(< 0 -1)` خاطئ.
- الاختبار الثاني `(> 0 -1)` صحيح، لذا:
  - تُنفَّذ كتلة `let`، ويرتبط `y` بـ `20`.
  - يطبع `"Positive condition met"`.
  - ثم يحسب `(+ y y)` ويُرجع `40`.

النتيجة: **تطبع « Positive condition met » وتُرجع 40.**

#### مثال 4: الاحتياط باستخدام `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- لم يُقيَّم أي من الاختبارين الأولين إلى true.
- تُنفَّذ جملة `else` وتُرجع `"Fallback value"`.

النتيجة: **"Fallback value"**

### ملخص

- استخدم `cond` للتعامل مع شروط متعددة بطريقة واضحة وموجزة.
- يمكن أن تكون consequents تعبيرات فردية أو إجراءات مجمّعة باستخدام `begin`.
- استخدم `let` في consequents للإعلان عن متغيرات محلية للعمليات الحسابية.
- ضمّن دائمًا جملة `else` كاحتياط للتعامل مع الحالات غير المتوقعة.

هذه المرونة تجعل `cond` أداة قوية وسهلة القراءة للتعامل مع منطق التفرّع المعقد.
