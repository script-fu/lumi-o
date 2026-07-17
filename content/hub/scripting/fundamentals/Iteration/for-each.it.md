---
title: "for-each"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
url: "hub/scripting/fundamentals/Iteration/for-each"
---
La funzione `for-each` in Scheme applica una procedura a ogni elemento di una lista (o di più liste). A differenza di `map`, che restituisce una nuova lista con i risultati, `for-each` serve per i **suoi effetti collaterali**, come la stampa o l'aggiornamento di variabili.

La forma più semplice di `for-each` è:

```scheme
(for-each procedure list)
```

- **Procedura:** funzione applicata a ogni elemento della lista.
- **Lista:** lista i cui elementi verranno elaborati.

---

### Esempio: stampare una lista

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Qui la funzione `print-item` viene applicata a ogni elemento della lista `(1 2 3 4)`.
- Ogni numero viene stampato in sequenza.

**Output**: `1 2 3 4`

---

### Come funziona

1. **Itera su ogni elemento:**
   - La procedura fornita viene eseguita per ogni elemento della lista, in ordine.

2. **Esegue effetti collaterali:**
   - Effetti collaterali comuni includono stampa, logging o modifica di variabili esterne. A differenza di `map`, `for-each` non restituisce una nuova lista.

---

#### Esempio: più liste

Se vengono fornite più liste, `for-each` elabora gli elementi corrispondenti di ciascuna lista.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- La funzione `sum-and-print` somma gli elementi corrispondenti delle due liste e stampa i risultati.

**Output**: `5 7 9`

---

### Riepilogo

- La funzione `for-each` è utile quando l'obiettivo sono effetti collaterali su ogni elemento di una lista.
- A differenza di `map`, `for-each` non produce una nuova lista: si concentra solo sugli effetti collaterali della procedura.
- Può gestire più liste contemporaneamente, applicando la procedura agli elementi corrispondenti.

Con `for-each` è possibile elaborare liste in modo efficace quando l'obiettivo è eseguire azioni piuttosto che trasformare dati.
