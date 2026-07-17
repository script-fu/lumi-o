---
title: "Recursione semplice"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
In Scheme, la ricorsione significa che una funzione chiama se stessa per risolvere sottoproblemi più piccoli del problema originale. Un modello di **ricorsione semplice** prevede un caso base che arresta la ricorsione e un caso ricorsivo che riduce il problema.

La struttura generale di una funzione ricorsiva è:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condizione base:** arresta la ricorsione.
- **Risultato base:** valore restituito quando la condizione base è soddisfatta.
- **Chiamata ricorsiva:** chiamata alla funzione stessa con argomenti modificati che avvicinano il calcolo al caso base.

---

### Esempio: somma dei numeri (da 1 a n)

Una semplice funzione ricorsiva per calcolare la somma dei numeri da 1 a n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Caso base: fermarsi quando n è 0
    0                          ; Risultato base: la somma è 0
    (+ n (sum-to-n (- n 1))))) ; Chiamata ricorsiva: sommare n al risultato del sottoproblema minore
```

---

#### Scomporre e ricomporre

La ricorsione scompone il problema originale in parti più piccole. Ogni chiamata gestisce un pezzo e passa avanti il resto. Quando si raggiunge il caso più semplice, i risultati vengono ricomposti man mano che il calcolo si completa.

#### Traccia passo passo di sum-to-n 3

1. **Chiamata iniziale:** *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Seconda chiamata:** *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Terza chiamata:** *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Caso base:** *sum-to-n 0*
   → *0*

---

#### Ricomporre il risultato finale

Quando il caso più semplice è risolto, ogni livello del calcolo si completa:

1. *sum-to-n 0* restituisce *0*
2. *sum-to-n 1* diventa *(+ 1 0) = 1*
3. *sum-to-n 2* diventa *(+ 2 1) = 3*
4. *sum-to-n 3* diventa *(+ 3 3) = 6*

---

### Esempio: stampare ogni elemento di una lista

Ecco una semplice funzione ricorsiva che stampa ogni elemento di una lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Stampa il primo elemento
      (print-elements (cdr lst)))))             ; Elabora il resto della lista
```

- **Caso base:** se la lista è vuota (*null? lst*), la ricorsione si arresta.
- **Caso ricorsivo:** stampa il primo elemento (*car lst*), poi chiama la funzione sul resto della lista (*cdr lst*).

#### Esempio d'uso

```scheme
(print-elements (list 1 2 3))
```

Output:

- *"1"*
- *"2"*
- *"3"*

Risultato: *"done"*

---

#### Come funziona

1. La funzione recupera il primo elemento della lista con *car* e lo elabora.
2. Poi chiama se stessa con il resto della lista (*cdr*).
3. Il processo si ripete finché la lista non è vuota (*null? lst*).

---

### Riepilogo

- La ricorsione semplice consiste in:
  1. **Caso base:** arresta la ricorsione.
  2. **Caso ricorsivo:** riduce il problema verso il caso base.
- Ogni chiamata ricorsiva avvicina il calcolo al completamento.
- Quando si raggiunge il caso base, i risultati vengono combinati al termine della ricorsione.

La ricorsione rispecchia la struttura del problema e offre un flusso chiaro e logico. Assicurarsi sempre di avere un caso base per evitare ricorsione infinita.
