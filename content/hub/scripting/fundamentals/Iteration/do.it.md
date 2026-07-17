---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
---
La funzione `do` in Scheme è un meccanismo di loop che consente l'iterazione con inizializzazione, aggiornamento e condizioni di terminazione. È particolarmente utile quando è necessario eseguire una sequenza di operazioni un numero specifico di volte o finché non si verifica una condizione.

La forma generale di `do` è:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabile:** variabile(i) del loop.
- **Valore iniziale:** valore di partenza di ogni variabile del loop.
- **Espressione di aggiornamento:** espressione che aggiorna la variabile del loop al termine di ogni iterazione.
- **Condizione di terminazione:** condizione che arresta il loop.
- **Espressione risultato:** valore restituito quando il loop termina.
- **Corpo:** codice eseguito a ogni iterazione.

---

### Esempio: somma dei numeri da 1 a 5

```scheme
(do ((i 1 (+ i 1))      ; Inizializzare i a 1, incrementare di 1
     (sum 0 (+ sum i))) ; Inizializzare sum a 0, aggiungere i a sum
    ((> i 5) sum)       ; Terminare quando i > 5, restituire sum
  (lumi-message (number->string sum))) ; Stampa la somma a ogni passo
```

- La variabile del loop `i` parte da 1 e aumenta di 1 a ogni iterazione.
- La variabile `sum` accumula la somma di `i`.
- Il loop termina quando `i > 5`, restituendo il valore finale di `sum`.

**Output**: `15`

---

### Come funziona

1. **Inizializzazione:**
   - Ogni variabile del loop riceve il valore iniziale.

2. **Controllo di terminazione:**
   - All'inizio di ogni iterazione si verifica la condizione di terminazione. Se è vera, il loop si arresta e viene valutata l'espressione risultato.

3. **Iterazione:**
   - Se la condizione di terminazione è falsa, viene eseguito il corpo e le variabili del loop vengono aggiornate con le rispettive espressioni di aggiornamento.

---

### Riepilogo

- Il costrutto `do` offre un modo flessibile di implementare loop con più variabili e condizioni di terminazione complesse.
- È utile per compiti che richiedono aggiornamenti di stato tra le iterazioni.
- La condizione di terminazione determina quando il loop finisce e può restituire un risultato finale.

Con `do` è possibile implementare algoritmi iterativi in Scheme con controllo preciso su inizializzazione, aggiornamenti e terminazione. Combina un **meccanismo di binding con scope** (come `let`) e una **struttura di controllo iterativa**, gestendo loop e stato temporaneo in modo pulito e conciso.
