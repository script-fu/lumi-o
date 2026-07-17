---
title: "قوائم الارتباط (Alists)"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/alists"
---
**قائمة الارتباط** (أو **alist**) بنية بيانات أساسية في Scheme لتمثيل مجموعات من أزواج المفتاح-القيمة. تُنفَّذ كقائمة من الأزواج، حيث يربط كل زوج مفتاحًا (عادةً رمزًا) بقيمة. هي بسيطة ومرنة ومناسبة لمجموعات بيانات صغيرة ومتوسطة.

### هيكل alist

الـ alist قائمة يكون فيها كل عنصر **زوجًا** (يُنشأ بـ `cons`). يتكون كل زوج من:

- **المفتاح**: العنصر الأول (رمز عادةً).
- **القيمة**: العنصر الثاني، ويمكن أن يكون من أي نوع بيانات.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **المفاتيح**: `'name`، `'age`، `'city`
- **القيم**: `"Alice"`، `30`، `"Paris"`
- **الهيكل**: `((name . "Alice") (age . 30) (city . "Paris"))`

### إنشاء alist

يمكن إنشاء alist يدويًا أو برمجيًا بـ `cons`.

#### استخدام الاقتباس المفرد (`'`)

علامة الاقتباس المفردة (`'`) اختصار لـ **quote**، ما يمنع Scheme من تقييم التعبير. مثالي لإنشاء alists ثابتة حيث تكون المفاتيح والقيم مكتوبة بشكل ثابت.

```scheme
;; تعريف alist يدويًا
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; إضافة زوج جديد برمجيًا
(define updated-alist (cons '(country . "France") alist))
```

**النتيجة**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### استخدام الاقتباس الخلفي (`` ` ``) والفاصلة (`,`)

الاقتباس الخلفي (`` ` ``) يشبه الاقتباس المفرد لكنه يسمح بإدراج تعبيرات مُقيَّمة ديناميكيًا باستخدام الفاصلة (`,`). مفيد لإنشاء alists حيث تُحسب المفاتيح أو القيم وقت التشغيل.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**النتيجة**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### مثال للمقارنة

alist ثابت باستخدام `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

alist ديناميكي باستخدام `` ` `` و`,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### الوصول إلى البيانات في alist

لاسترداد قيمة من alist، استخدم `assoc` التي تبحث عن زوج بمفتاحه.

```scheme
(assoc 'name alist)    ; يرجع (name . "Alice")
(assoc 'country alist) ; يرجع #f (المفتاح غير موجود)
```

### استخراج القيمة

بعد استرداد زوج بـ `assoc`، استخدم `cdr` لاستخراج القيمة:

```scheme
(cdr (assoc 'name alist))   ; يرجع "Alice"
```

### ملخص الميزات الرئيسية

- **اقتباس مفرد (`'`)**: إنشاء alist ثابت حيث تكون جميع العناصر بيانات حرفية.
- **اقتباس خلفي (`` ` ``)**: يسمح بإنشاء alists ديناميكيًا بمزج عناصر ثابتة مع تعبيرات مُقيَّمة (باستخدام `,`).
- **الترميز النقطي (`.`)**: يُستخدم لإنشاء أزواج تربط مفتاحًا بقيمة في alist.
