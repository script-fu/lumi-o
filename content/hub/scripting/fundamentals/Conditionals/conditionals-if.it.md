---
title: "Se"
type: docs
weight: 4
---
Nella sua forma più semplice, il condizionale `if` in Scheme valuta un test e, in base al risultato, esegue uno dei due possibili blocchi di codice. La forma più semplice è simile a questa:

```scheme
(if test-is-true
  do-this)
```

- Se `test` restituisce true (`#t`), viene eseguito il **blocco di codice nel conseguente**. Il blocco può restituire un valore o eseguire altre azioni, come l'assegnazione di una variabile o la stampa dell'output.

### Esempio

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- In questo caso, `test` è `(< 0 1)` (controllando se 0 è inferiore a 1).
- Poiché il test restituisce true (`#t`), viene eseguito il blocco di codice `(lumi-message "True!")`, che stampa `"True!"`.

### Aggiunta di un'altra condizione: `if-else`

Quando si utilizza un condizionale `if` con un blocco di codice alternativo (il caso `else`), la struttura è simile alla seguente:

```scheme
(if test
  do-this
  else-do-this)
```

- Se `test` restituisce true (`#t`), viene eseguito il blocco di codice **conseguente**.
- Se `test` restituisce false (`#f`), viene eseguito il blocco di codice **alternativo**.

```scheme
(if test
  consequent
  alternative)
```

### Come funziona

1. **Espressione di prova**:
   - L'espressione `test` viene valutata per prima.

2. **Risultato basato sul test**:
   - Se `test` restituisce true (`#t`), viene eseguito il **blocco di codice conseguente**.
   - Se `test` restituisce false (`#f`), viene eseguito il **blocco di codice alternativo**.

Entrambi i blocchi di codice `consequent` e `alternative` possono eseguire qualsiasi operazione Scheme valida, inclusa la restituzione di valori, la modifica di variabili o l'esecuzione di procedure.

### Esempi

#### Esempio 1: restituzione di un valore

```scheme
(if (< 0 1)
  1
  0)
```

- Qui, `test` è `(< 0 1)` (controllando se 0 è inferiore a 1).
- Poiché il test restituisce true (`#t`), viene eseguito il blocco **conseguente** (`1`) e viene restituito il relativo valore.

Risultato: **1**

#### Esempio 2: valutazione di un blocco iniziale

Nei casi in cui è necessario eseguire più azioni quando la condizione è vera o falsa, è possibile utilizzare `begin` o `let` per raggrupparle.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- In questo esempio, `test` è `(= 0 1)` (controllando se 0 è uguale a 1).
- Poiché il test restituisce falso (`#f`), viene eseguito il blocco **alternativo**:
  - Innanzitutto, stampa `"False condition met, calculating..."`.
  - Quindi calcola `(* 3 4)` e restituisce `12`.

Risultato: **Stampa "Falsa condizione soddisfatta, calcolo in corso..." e restituisce 12.**

#### Esempio 3: valutazione di un'istruzione let

L'utilizzo di `let` ci consente di dichiarare variabili di ambito locale all'interno del blocco di codice.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- In questo esempio, `test` è `(= 1 1)` (controllando se 1 è uguale a 1).
- Poiché il test restituisce true (`#t`), viene eseguito il blocco **conseguente**:
  - Innanzitutto, stampa `"True condition met, calculating..."`.
  - Quindi calcola `(* -1 10)` e restituisce `-10`.

Risultato: **Stampa "Vera condizione soddisfatta, calcolo in corso..." e restituisce -10.**

### Riepilogo- Il condizionale `if` è un potente strumento in Scheme per valutare i test ed eseguire i blocchi di codice corrispondenti.
- Può gestire sia espressioni semplici che blocchi di codice complessi che restituiscono valori, modificano variabili o eseguono effetti collaterali.
- Ricorda: se non è presente un blocco `else` esplicito, `if` valuta ed esegue il **conseguente** solo se il test è vero. Altrimenti valuta ed esegue l'**alternativa**.