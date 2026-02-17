---
title: "Fare"
type: docs
weight: 5
---
La funzione `do` in Scheme è un meccanismo di loop che consente l'iterazione con condizioni di inizializzazione, aggiornamento e terminazione. È particolarmente utile quando è necessario eseguire una sequenza di operazioni un numero specifico di volte o finché non viene soddisfatta una condizione.

La forma generale di `do` è:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabile**: le variabili del ciclo.
- **Valore iniziale**: il valore iniziale di ciascuna variabile del ciclo.
- **Update-expression**: l'espressione per aggiornare le variabili del ciclo alla fine di ogni iterazione.
- **Condizione di terminazione**: la condizione per interrompere il ciclo.
- **Espressione-risultato**: il valore da restituire al termine del ciclo.
- **Body**: il codice da eseguire in ogni iterazione.

---

### Esempio: Somma i numeri da 1 a 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- La variabile del loop `i` inizia da 1 e aumenta di 1 in ogni iterazione.
- La variabile `sum` accumula la somma di `i`.
- Il ciclo termina quando `i > 5`, restituendo il valore finale di `sum`.

**Uscita**: `15`

---

### Come funziona

1. **Inizializzazione**:
   - A ciascuna variabile del ciclo viene assegnato il suo valore iniziale.

2. **Controllo della risoluzione**:
   - All'inizio di ogni iterazione viene verificata la condizione di terminazione. Se vero, il ciclo si interrompe e viene valutata l'espressione del risultato.

3. **Iterazione**:
   - Se la condizione di terminazione è falsa, il corpo viene eseguito e le variabili del ciclo vengono aggiornate utilizzando le rispettive espressioni di aggiornamento.

---

### Riepilogo

- Il costrutto `do` fornisce un modo flessibile per implementare cicli con più variabili e condizioni di terminazione complesse.
- È utile per le attività che richiedono aggiornamenti dello stato tra le iterazioni.
- La condizione di terminazione determina quando termina il ciclo e può restituire un risultato finale.

Utilizzando `do`, è possibile implementare algoritmi iterativi in ​​Scheme con un controllo preciso su inizializzazione, aggiornamenti e terminazione. Ciò rende `do` una combinazione di un **meccanismo di associazione con ambito** (come `let`) e una **struttura di controllo iterativa**, che gli consente di gestire il looping e lo stato temporaneo in modo pulito e conciso.