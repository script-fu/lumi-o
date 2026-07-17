---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
In Scheme, il condizionale `cond` seleziona uno di diversi blocchi di codice da eseguire in base a più test. È come un `if` multiramo, in cui ogni ramo viene verificato in ordine fino a trovare una corrispondenza.

### Sintassi

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Ogni test viene valutato nell'ordine in cui è scritto.
- Quando un test è vero (`#t`), viene eseguito il **consequent** corrispondente e `cond` smette di valutare altri test.
- La clausola `else` è opzionale e funge da ripiego se nessun test è vero.

### Come funziona

1. **Verificare ogni condizione:**
   - `cond` valuta i test nell'ordine elencato.

2. **Eseguire il consequent corrispondente:**
   - Quando il primo test vero (`#t`) viene trovato, viene eseguito il suo **consequent**.
   - Se nessun test è vero e c'è `else`, viene eseguito il **fallback-consequent**.

### Esempi

#### Esempio 1: consequent con singola espressione

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Il primo test `(< 3 2)` è falso (`#f`).
- Il secondo test `(= 3 3)` è vero (`#t`), quindi viene restituito `"This will run"`.
- La clausola `else` non viene eseguita perché è già stata trovata una corrispondenza.

Risultato: **"This will run"**

#### Esempio 2: più azioni con `begin`

Quando il consequent richiede più azioni, raggrupparle con `begin`:

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

- Il primo test `(< 5 3)` è falso (`#f`).
- Il secondo test `(> 5 3)` è vero (`#t`):
  - Stampa `"Condition met"`.
  - Poi calcola `(* 5 5)` e restituisce `25`.

Risultato: **Stampa "Condition met" e restituisce 25.**

#### Esempio 3: blocco `let` nel consequent

Per variabili locali, usare `let`:

```scheme
(cond
  ;; Caso 1: se 0 è minore di -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Caso 2: se 0 è maggiore di -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Caso predefinito
  (else
    (let ((z 0))
      z)))
```

- Il primo test `(< 0 -1)` è falso.
- Il secondo test `(> 0 -1)` è vero:
  - Viene eseguito un blocco `let` che lega `y` a `20`.
  - Stampa `"Positive condition met"`.
  - Poi calcola `(+ y y)` e restituisce `40`.

Risultato: **Stampa "Positive condition met" e restituisce 40.**

#### Esempio 4: ripiego con `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Nessuno dei primi due test è vero.
- Viene eseguita la clausola `else`, che restituisce `"Fallback value"`.

Risultato: **"Fallback value"**

### Riepilogo

- Usare `cond` per gestire più condizioni in modo chiaro e conciso.
- I consequent possono essere singole espressioni o azioni raggruppate con `begin`.
- Usare `let` nei consequent per variabili locali nei calcoli.
- Includere sempre `else` come ripiego per casi imprevisti.

Questa flessibilità rende `cond` uno strumento potente e leggibile per logiche di branching complesse.
