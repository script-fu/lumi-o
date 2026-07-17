---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
في Scheme، رغم أن `if` أنيق ومتعدد الاستخدامات، قد يصبح مربكًا عند استخدامه بدون `else` صريح. يحدث هذا خصوصًا عندما يكون المقصود تنفيذ فرع واحد فقط عندما يكون الشرط صحيحًا، دون إجراء بديل لحالة `false`. في مثل هذه السيناريوهات، توفر بنية `when` بديلًا أوضح وأكثر إيجازًا.

يبدو الشكل الأساسي لـ `when` كالتالي:

```scheme
(when test-is-true
  do-this
  do-that)
```

- إذا قيّم `test` إلى true (`#t`)، تُنفَّذ جميع التعبيرات في نص بنية `when` بالتسلسل.
- إذا قيّم `test` إلى false (`#f`)، لا يحدث شيء ولا تُرجع أي قيم.

### مثال

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### مقارنة `if` و`when`

لفهم الفرق بين `if` و`when` بشكل أفضل، انظر المثال التالي حيث يُستخدمان معًا:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### الشرح:

1. **شرط `if`**:
   - يتحقق الاختبار `(= 0 1)` مما إذا كان 0 يساوي 1.
   - بما أن هذا خاطئ (`#f`)، يُنفَّذ فرع `else` من `if`.

2. **بنية `when` في فرع `else`**:
   - يتحقق اختبار `when` `(< 0 1)` مما إذا كان 0 أقل من 1.
   - بما أن هذا صحيح (`#t`)، تُنفَّذ جميع التعبيرات داخل نص `when` بالتسلسل:
     - أولًا، تطبع `"The 'when' condition is true!"`.
     - ثم تطبع `"Executing multiple actions within 'when'."`.

#### لماذا تستخدم `when` هنا؟

- استخدام `when` بدل `if` آخر يبسّط المنطق عندما لا حاجة لفرع `else` صريح.
- يوضح `when` أن الفرع الصحيح فقط هو المعني، ما يقلّل الارتباك المحتمل.

### ملخص

- استخدم `if` عندما تحتاج فرعًا صحيحًا وفرعًا خاطئًا.
- استخدم `when` عندما يوجد فرع واحد فقط للحالة الصحيحة، خصوصًا عند تنفيذ إجراءات متعددة.
- الجمع بين `if` و`when` يساعد على بناء شروط أكثر تعقيدًا بوضوح وإيجاز.
