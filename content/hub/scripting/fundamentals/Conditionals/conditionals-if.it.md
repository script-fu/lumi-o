---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
Nella sua forma più semplice, il condizionale `if` in Scheme valuta un test e, in base al risultato, esegue uno di due possibili blocchi di codice:

```scheme
(if test-is-true
  do-this)
```

- Se il `test` è vero (`#t`), viene eseguito il **blocco consequent**. Il blocco può restituire un valore o eseguire altre azioni, come assegnare una variabile o stampare output.

### Esempio

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- In questo caso il `test` è `(< 0 1)` (verifica se 0 è minore di 1).
- Poiché il test è vero (`#t`), viene eseguito `(lumi-message "True!")`, che stampa `"True!"`.

### Aggiungere else: `if-else`

Quando `if` ha un blocco alternativo (caso `else`), la struttura è:

```scheme
(if test
  do-this
  else-do-this)
```

- Se il `test` è vero (`#t`), viene eseguito il **consequent**.
- Se il `test` è falso (`#f`), viene eseguito l'**alternative**.

```scheme
(if test
  consequent
  alternative)
```

### Come funziona

1. **Espressione di test:**
   - Prima viene valutato il `test`.

2. **Risultato in base al test:**
   - Se il `test` è vero (`#t`), viene eseguito il **consequent**.
   - Se il `test` è falso (`#f`), viene eseguito l'**alternative**.

Entrambi i blocchi possono eseguire qualsiasi operazione Scheme valida, incluso restituire valori, modificare variabili o eseguire procedure.

### Esempi

#### Esempio 1: restituire un valore

```scheme
(if (< 0 1)
  1
  0)
```

- Il `test` è `(< 0 1)`.
- Poiché il test è vero (`#t`), viene eseguito il **consequent** (`1`) e ne viene restituito il valore.

Risultato: **1**

#### Esempio 2: valutare un blocco `begin`

Quando servono più azioni, raggrupparle con `begin` o `let`.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Il `test` è `(= 0 1)`.
- Poiché il test è falso (`#f`), viene eseguito l'**alternative**:
  - Prima stampa `"False condition met, calculating..."`.
  - Poi calcola `(* 3 4)` e restituisce `12`.

Risultato: **Stampa "False condition met, calculating..." e restituisce 12.**

#### Esempio 3: valutare un'espressione `let`

Con `let` è possibile dichiarare variabili locali nel blocco.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Il `test` è `(= 1 1)`.
- Poiché il test è vero (`#t`), viene eseguito il **consequent**:
  - Prima stampa `"True condition met, calculating..."`.
  - Poi calcola `(* -1 10)` e restituisce `-10`.

Risultato: **Stampa "True condition met, calculating..." e restituisce -10.**

### Riepilogo

- Il condizionale `if` è uno strumento potente in Scheme per valutare test ed eseguire blocchi corrispondenti.
- Gestisce sia espressioni semplici sia blocchi complessi che restituiscono valori, modificano variabili o producono effetti collaterali.
- Senza un `else` esplicito, `if` esegue il **consequent** solo se il test è vero; altrimenti l'**alternative**.
