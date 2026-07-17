---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
في أبسط صوره، يقيّم الشرط `if` في Scheme اختبارًا، وبناءً على النتيجة ينفّذ إحدى كتلتي التعليمات البرمجية المحتملتين. أبسط شكل يبدو كالتالي:

```scheme
(if test-is-true
  do-this)
```

- إذا قيّم `test` إلى true (`#t`)، تُنفَّذ **كتلة consequent**. قد تُرجع الكتلة قيمة أو تنفّذ إجراءات أخرى، مثل تعيين متغير أو طباعة مخرجات.

### مثال

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- هنا، `test` هو `(< 0 1)` (التحقق مما إذا كان 0 أقل من 1).
- بما أن نتيجة الاختبار صحيحة (`#t`)، تُنفَّذ كتلة التعليمات البرمجية `(lumi-message "True!")`، التي تطبع `"True!"`.

### إضافة فرع else: `if-else`

عند استخدام `if` مع كتلة بديلة (حالة `else`)، تبدو البنية كالتالي:

```scheme
(if test
  do-this
  else-do-this)
```

- إذا قيّم `test` إلى true (`#t`)، تُنفَّذ كتلة **consequent**.
- إذا قيّم `test` إلى false (`#f`)، تُنفَّذ كتلة **alternative**.

```scheme
(if test
  consequent
  alternative)
```

### كيف يعمل

1. **تعبير الاختبار**:
   - يُقيَّم تعبير `test` أولًا.

2. **النتيجة بناءً على الاختبار**:
   - إذا قيّم `test` إلى true (`#t`)، تُنفَّذ **كتلة consequent**.
   - إذا قيّم `test` إلى false (`#f`)، تُنفَّذ **كتلة alternative**.

يمكن لكل من كتلتي `consequent` و`alternative` تنفيذ أي عملية Scheme صالحة، بما في ذلك إرجاع القيم أو تعديل المتغيرات أو تشغيل إجراءات.

### أمثلة

#### مثال 1: إرجاع قيمة

```scheme
(if (< 0 1)
  1
  0)
```

- هنا، `test` هو `(< 0 1)` (التحقق مما إذا كان 0 أقل من 1).
- بما أن نتيجة الاختبار صحيحة (`#t`)، تُنفَّذ الكتلة **consequent** (`1`) وتُرجع قيمتها.

النتيجة: **1**

#### مثال 2: تقييم كتلة `begin`

عندما تحتاج إلى تنفيذ إجراءات متعددة عندما يكون الشرط صحيحًا أو خاطئًا، يمكنك استخدام `begin` أو `let` لتجميعها.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- هنا، `test` هو `(= 0 1)` (التحقق مما إذا كان 0 يساوي 1).
- بما أن نتيجة الاختبار خاطئة (`#f`)، تُنفَّذ الكتلة **alternative**:
  - أولًا، تطبع `"False condition met, calculating..."`.
  - ثم تحسب `(* 3 4)` وتُرجع `12`.

النتيجة: **تطبع « False condition met, calculating... » وتُرجع 12.**

#### مثال 3: تقييم تعبير `let`

يتيح `let` الإعلان عن متغيرات محلية داخل كتلة التعليمات البرمجية.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- هنا، `test` هو `(= 1 1)` (التحقق مما إذا كان 1 يساوي 1).
- بما أن نتيجة الاختبار صحيحة (`#t`)، تُنفَّذ كتلة **consequent**:
  - أولًا، تطبع `"True condition met, calculating..."`.
  - ثم تحسب `(* -1 10)` وتُرجع `-10`.

النتيجة: **تطبع « True condition met, calculating... » وتُرجع -10.**

### ملخص

- الشرط `if` أداة قوية في Scheme لتقييم الاختبارات وتنفيذ كتل التعليمات البرمجية المقابلة.
- يمكنه التعامل مع التعبيرات البسيطة وكتل التعليمات البرمجية المعقدة التي تُرجع قيمًا أو تعدّل المتغيرات أو تنفّذ تأثيرات جانبية.
- تذكّر: إذا لم تكن هناك كتلة `else` صريحة، يقيّم `if` وينفّذ **consequent** فقط إذا كان الاختبار صحيحًا؛ وإلا يقيّم وينفّذ **alternative**.
