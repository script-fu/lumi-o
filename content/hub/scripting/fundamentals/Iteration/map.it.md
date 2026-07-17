---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
url: "hub/scripting/fundamentals/Iteration/map"
---
La funzione `map` in Scheme applica una procedura a ogni elemento di una lista (o di più liste) e **restituisce una nuova lista** con i risultati. Ideale per la trasformazione dei dati.

La forma più semplice di `map` è:

```scheme
(map procedure list)
```

- **Procedura:** funzione applicata a ogni elemento della lista.
- **Lista:** lista i cui elementi verranno elaborati.

---

### Esempio: raddoppiare ogni elemento

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Qui la funzione `double` viene applicata a ogni elemento della lista `(1 2 3 4)`.
- Il risultato è una nuova lista con valori raddoppiati.

**Output**: `(2 4 6 8)`

---

### Come funziona

1. **Crea una nuova lista:**
   - `map` raccoglie i risultati della procedura in una nuova lista.

2. **Trasforma i dati:**
   - A differenza di `for-each`, `map` si concentra sulla trasformazione dei dati piuttosto che sugli effetti collaterali.

---

#### Esempio: più liste

Se vengono fornite più liste, `map` elabora gli elementi corrispondenti di ciascuna lista.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- La funzione `sum` somma gli elementi corrispondenti delle due liste e restituisce i risultati come nuova lista.

**Output**: `(5 7 9)`

---

### Riepilogo

- La funzione `map` è uno strumento potente per trasformare liste elemento per elemento.
- A differenza di `for-each`, `map` **produce una nuova lista**.
- Supporta più liste e consente operazioni elemento per elemento.

Con `map` è possibile creare versioni trasformate dei dati senza modificare le liste originali.
