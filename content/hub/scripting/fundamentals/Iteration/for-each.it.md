---
title: "per-ciascuno"
type: docs
weight: 5
---
La funzione `for-each` in Scheme viene utilizzata per applicare una procedura a ciascun elemento di un elenco (o più elenchi). A differenza di `map`, che restituisce un nuovo elenco con i risultati, `for-each` viene utilizzato per i suoi **effetti collaterali**, come la stampa o l'aggiornamento delle variabili.

La forma più semplice di `for-each` è simile alla seguente:

```scheme
(for-each procedure list)
```

- **Procedura**: Una funzione da applicare a ciascun elemento della lista.
- **Lista**: la lista i cui elementi verranno elaborati.

---

### Esempio: stampare un elenco

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- In questo caso, la funzione `print-item` viene applicata a ciascun elemento dell'elenco `(1 2 3 4)`.
- Ciò fa sì che ciascun numero venga stampato in sequenza.

**Uscita**: `1 2 3 4`

---

### Come funziona

1. **Itera su ciascun elemento**:
   - La procedura fornita viene eseguita per ogni elemento dell'elenco, in ordine.

2. **Produce effetti collaterali**:
   - Gli effetti collaterali più comuni includono la stampa, la registrazione o la modifica di variabili esterne. A differenza di `map`, `for-each` non restituisce un nuovo elenco.

---

#### Esempio: utilizzo con più elenchi

Se vengono forniti più elenchi, `for-each` elabora gli elementi corrispondenti da ciascun elenco.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- La funzione `sum-and-print` somma gli elementi corrispondenti dai due elenchi e stampa i risultati.

**Uscita**: `5 7 9`

---

### Riepilogo

- La funzione `for-each` è utile per eseguire effetti collaterali su ciascun elemento di un elenco.
- A differenza di `map`, `for-each` non produce un nuovo elenco: si concentra esclusivamente sugli effetti collaterali della procedura.
- Può gestire più liste contemporaneamente, applicando la procedura agli elementi corrispondenti.

Utilizzando `for-each`, puoi elaborare efficacemente gli elenchi quando l'obiettivo è eseguire azioni anziché trasformare i dati.