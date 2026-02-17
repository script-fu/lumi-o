---
title: "Ricorsione semplice"
type: docs
weight: 5
---
La ricorsione è un concetto potente in Scheme, in cui una funzione richiama se stessa per risolvere sottoproblemi più piccoli del problema originale. Un modello di **ricorsione semplice** prevede un caso base per interrompere la ricorsione e un caso ricorsivo per ridurre il problema.

La struttura generale di una funzione ricorsiva è simile alla seguente:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condizione base**: Interrompe la ricorsione.
- **Risultato base**: il valore restituito quando viene soddisfatta la condizione di base.
- **Chiamata ricorsiva**: una chiamata alla funzione stessa con argomenti modificati che avvicinano il calcolo al caso base.

---

### Esempio: somma di numeri (da 1 a n)

Una semplice funzione ricorsiva per calcolare la somma dei numeri da 1 a n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Come funziona: scomporre e rimontare

La ricorsione funziona suddividendo il problema originale in parti più piccole. Ogni chiamata alla funzione gestisce un pezzo e passa il resto. Una volta raggiunto il caso più semplice, i risultati vengono riassemblati al termine del calcolo.

#### Traccia passo passo di somma-a-n 3

1. **Chiamata iniziale**: *somma-n 3*
   → *(+ 3 (somma a n 2))*

2. **Seconda chiamata**: *somma-n 2*
   → *(+ 2 (somma a n 1))*

3. **Terza chiamata**: *somma-n 1*
   → *(+ 1 (somma a n 0))*

4. **Caso base**: *somma su n 0*
   → *0*

---

#### Riassemblare il risultato finale

Una volta risolto il caso più semplice, ogni livello del calcolo viene completato:

1. *somma-a-n 0* dà *0*
2. *somma-n-1* diventa *(+ 1 0) = 1*
3. *somma-a-n 2* diventa *(+ 2 1) = 3*
4. *somma-a-n 3* diventa *(+ 3 3) = 6*

---

### Esempio: stampa di ogni elemento di un elenco

Ecco una semplice funzione ricorsiva per stampare ogni elemento in un elenco:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Caso base:** Se l'elenco è vuoto (*null? lst*), interrompe la ricorsione.
- **Caso ricorsivo:** Stampa il primo elemento (*car lst*), quindi chiama la funzione sul resto della lista (*cdr lst*).

#### Esempio di utilizzo

```scheme
(print-elements (list 1 2 3))
```

Uscita:

- *"1"*
- *"2"*
- *"3"*

Risultato: *"fatto"*

---

#### Come funziona

1. La funzione recupera il primo elemento dell'elenco utilizzando *car* e lo elabora.
2. Quindi richiama se stesso con il resto della lista (*cdr*).
3. Questo processo si ripete finché l'elenco non è vuoto (*null? lst*).

---

### Riepilogo

- La ricorsione semplice è composta da:
  1. **Caso base**: Interrompe la ricorsione.
  2. **Caso ricorsivo**: riduce il problema verso il caso base.
- Ogni chiamata ricorsiva fa avanzare il calcolo verso il completamento.
- Una volta raggiunto il caso base, i risultati vengono combinati al completamento della ricorsione.

La ricorsione rispecchia la struttura del problema e fornisce un flusso chiaro e logico. Garantire sempre un caso base per evitare la ricorsione infinita.