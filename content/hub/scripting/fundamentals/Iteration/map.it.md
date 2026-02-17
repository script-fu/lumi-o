---
title: "mappa"
type: docs
weight: 3
---
La funzione `map` in Scheme viene utilizzata per applicare una procedura a ciascun elemento di una lista (o più liste) e **restituire una nuova lista** contenente i risultati. Ciò lo rende ideale per la trasformazione dei dati.

La forma più semplice di `map` è simile alla seguente:

```scheme
(map procedure list)
```

- **Procedura**: Una funzione da applicare a ciascun elemento della lista.
- **Lista**: la lista i cui elementi verranno trasformati.

---

### Esempio: raddoppia ogni elemento

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- In questo caso, la funzione `double` viene applicata a ciascun elemento dell'elenco `(1 2 3 4)`.
- Il risultato è una nuova lista con ogni elemento raddoppiato.

**Uscita**: `(2 4 6 8)`

---

### Come funziona

1. **Crea un nuovo elenco**:
   - `map` applica la procedura fornita a ciascun elemento dell'elenco e raccoglie i risultati in un nuovo elenco.

2. **Trasforma i dati**:
   - Viene utilizzato principalmente per le trasformazioni dei dati piuttosto che per l'esecuzione di effetti collaterali.

---

#### Esempio: utilizzo con più elenchi

Se vengono forniti più elenchi, `map` elabora gli elementi corrispondenti da ciascun elenco.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- La funzione `sum` aggiunge gli elementi corrispondenti dai due elenchi e restituisce i risultati come un nuovo elenco.

**Uscita**: `(5 7 9)`

---

### Riepilogo

- La funzione `map` è un potente strumento per trasformare le liste applicando una procedura a ciascun elemento.
- A differenza di `for-each`, `map` **produce un nuovo elenco** contenente i risultati dell'applicazione della procedura.
- Supporta più elenchi, consentendo operazioni a livello di elementi su di essi.

Utilizzando `map`, puoi creare in modo efficiente versioni trasformate dei tuoi dati mantenendo invariati gli elenchi originali.