---
title: "cond"
type: docs
weight: 5
---
In Scheme, il condizionale `cond` viene utilizzato per selezionare uno dei diversi possibili blocchi di codice da eseguire, in base a più test. È come un multi-ramo `if`, in cui ogni ramo viene controllato in ordine finché non viene trovata una corrispondenza.

### Sintassi

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Ciascun test viene valutato nell'ordine in cui è scritto.
- Quando un test restituisce true (`#t`), viene eseguito il corrispondente **conseguente** e l'espressione `cond` interrompe la valutazione di ulteriori test.
- La clausola `else` è facoltativa e funge da fallback se nessuno dei test risulta vero.

### Come funziona

1. **Testare ogni condizione**:
   - `cond` valuta i test nell'ordine in cui sono elencati.

2. **Eseguire la corrispondenza conseguente**:
   - Quando viene trovato il primo test che restituisce true (`#t`), viene eseguito il suo **conseguente**.
   - Se nessun test restituisce true ed è presente una clausola `else`, viene eseguito il **fallback-consequent**.

### Esempi

#### Esempio 1: Conseguenti di una singola espressione

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Il primo test `(< 3 2)` restituisce falso (`#f`).
- Il secondo test `(= 3 3)` restituisce true (`#t`), quindi viene restituito `"This will run"`.
- La clausola `else` non viene eseguita perché è già stata trovata una corrispondenza.

Risultato: **"Verrà eseguito"**

#### Esempio 2: azioni multiple utilizzando `begin`

Quando un conseguente implica più azioni, utilizzare `begin` per raggrupparle:

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

- Il primo test `(< 5 3)` restituisce falso (`#f`).
- Il secondo test `(> 5 3)` restituisce vero (`#t`):
  - Stampa `"Condition met"`.
  - Quindi calcola `(* 5 5)` e restituisce `25`.

Risultato: **Stampa "Condizione soddisfatta" e restituisce 25.**

#### Esempio 3: utilizzo di un blocco `let` in un blocco successivo

Quando è necessario introdurre variabili locali, utilizzare un blocco `let`:

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- Il primo test `(< 0 -1)` è falso.
- Il secondo test `(> 0 -1)` è vero, quindi:
  - Viene eseguito un blocco `let`, che collega `y` a `20`.
  - Stampa `"Positive condition met"`.
  - Quindi calcola `(+ y y)` e restituisce `40`.

Risultato: **Stampa "Condizione positiva soddisfatta" e restituisce 40.**

#### Esempio 4: Fallback con `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Nessuno dei primi due test risulta vero.
- La clausola `else` viene eseguita e restituisce `"Fallback value"`.

Risultato: **"Valore di riserva"**

### Riepilogo

- Utilizzare `cond` per gestire più condizioni in modo chiaro e conciso.
- I conseguenti possono essere espressioni singole o azioni raggruppate utilizzando `begin`.
- Utilizzare `let` nei conseguenti per dichiarare variabili locali per i calcoli.
- Includere sempre una clausola `else` come fallback per gestire casi imprevisti.

Questa flessibilità rende `cond` uno strumento potente e leggibile per gestire la logica di ramificazione complessa.