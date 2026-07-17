---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
У Scheme `if` елегантний і універсальний, але без явного `else` може бути незрозумілим — особливо коли потрібно виконати код лише за істинної умови, без альтернативи для хибного випадку. Тоді конструкція `when` дає чіткішу і стислішу альтернативу.

Базова форма `when` виглядає так:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Якщо `test` істинний (`#t`), усі вирази в тілі `when` виконуються послідовно.
- Якщо `test` хибний (`#f`), нічого не відбувається і значення не повертаються.

### Приклад

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Порівняння `if` і `when`

Щоб краще зрозуміти різницю, розгляньте приклад, де обидві конструкції використовуються разом:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Пояснення:

1. **Умова `if`:**
   - Тест `(= 0 1)` перевіряє, чи 0 дорівнює 1.
   - Оскільки це хибно (`#f`), виконується гілка `else` оператора `if`.

2. **Конструкція `when` у гілці `else`:**
   - Тест `when` `(< 0 1)` перевіряє, чи 0 менше 1.
   - Оскільки це істина (`#t`), усі вирази в тілі `when` виконуються послідовно:
     - спочатку виводиться `"The 'when' condition is true!"`;
     - потім `"Executing multiple actions within 'when'."`.

#### Навіщо тут `when`?

- Замість другого `if` `when` спрощує логіку, коли явна гілка `else` не потрібна.
- `when` показує, що важлива лише істинна гілка, що зменшує плутанину.

### Підсумок

- Використовуйте `if`, коли потрібні обидві гілки — істинна і хибна.
- Використовуйте `when`, коли є лише одна гілка для істинного випадку, особливо якщо потрібно виконати кілька дій.
- Поєднання `if` і `when` допомагає структурувати складніші умовні конструкції чітко і стисло.
