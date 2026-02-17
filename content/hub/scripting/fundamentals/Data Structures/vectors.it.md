---
title: "Vettori"
type: docs
weight: 5
---
In Scheme, un vettore è un'altra struttura dati fondamentale utilizzata per raggruppare valori. A differenza degli elenchi, i vettori sono raccolte di elementi indicizzati e di dimensione fissa, che forniscono accesso casuale e aggiornamenti più rapidi. Ogni elemento in un vettore può essere di qualsiasi tipo, incluso un altro vettore. I vettori sono rappresentati utilizzando # seguito da parentesi. `#(1 2 3)`

Anche se i vettori e gli elenchi possono sembrare simili, hanno scopi diversi nella programmazione di Scheme:

- Le liste sono più comunemente usate per operazioni ricorsive e strutture dinamiche, poiché la loro implementazione a nodo collegato consente una manipolazione efficiente del loro inizio e del loro attraversamento attraverso la scomposizione ricorsiva.

- I vettori, d'altro canto, sono ottimizzati per scenari in cui è richiesto l'accesso casuale a elementi o aggiornamenti a indici specifici, rendendoli più adatti a casi d'uso come tabelle di ricerca, configurazioni a dimensione fissa o operazioni indicizzate critiche per le prestazioni.

In sostanza, le liste sono la scelta naturale per algoritmi ricorsivi e dati di dimensione dinamica, mentre i vettori brillano quando i modelli di accesso a dimensione fissa o indicizzati sono fondamentali.

### Vettori semplici

```scheme
(vector 1 2 3)
```

- Crea un vettore di tre elementi: `1`, `2` e `3`.

Risultato: **`#(1 2 3)`**

#### Accesso agli elementi vettoriali

È possibile accedere agli elementi in un vettore utilizzando la procedura `vector-ref`, che recupera l'elemento in un indice specificato (a partire da `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iterazione: elaborazione di ciascun elemento in un vettore

È possibile scorrere un vettore utilizzando un ciclo o la ricorsione. Lo schema fornisce `vector-length` per determinare la dimensione di un vettore. Ecco un semplice ciclo per stampare ogni elemento in un vettore:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Caso base:** Se l'indice `i` raggiunge la lunghezza del vettore, interrompe il ciclo.
- **Caso ricorsivo:** stampa l'elemento all'indice `i`, quindi incrementa `i`.

#### Esempio di utilizzo

```scheme
(print-elements (vector 1 2 3))
```

Risultato:

- `"1"`
- `"2"`
- `"3"`

Risultato: "fatto"

### Vettori misti

I vettori possono includere elementi di diverso tipo, tra cui stringhe, booleani, numeri, altri vettori o persino il risultato di espressioni:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Questo crea un vettore con:
  - Un numero (`42`)
  - Una stringa (`"hello"`)
  - Un valore booleano (`#t`)
  - Un altro vettore (`#(1 2)`)
  - Il risultato di un'espressione (`(+ 3 4)`, che restituisce `7`)

Risultato: **`#(42 "hello" #t #(1 2) 7)`**

### Costruire vettori

I vettori vengono creati utilizzando `vector` oppure utilizzando `make-vector` per creare un vettore di dimensione fissa con un valore iniziale.

```scheme
(make-vector 5 0)
```

Crea un vettore di dimensione `5` con tutti gli elementi inizializzati su `0`.

Risultato: `#(0 0 0 0 0)`

### Aggiornamento dei vettori

La procedura `vector-set!` aggiorna un elemento in un vettore in un indice specificato.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Risultato: `#(1 42 3)`

### Controllo dei vettori

La procedura `vector?` controlla se un dato valore è un vettore.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Risultato:

- `(vector? (vector 1 2 3))` restituisce `#t` (vero)
- `(vector? 42)` restituisce `#f` (falso)

### Vettori e comportamento di passaggio per riferimentoIn Scheme, i vettori sono mutabili e passati per riferimento. Ciò significa che quando passi un vettore a una funzione, la funzione può modificare direttamente il vettore originale. Qualsiasi modifica apportata al vettore all'interno della funzione si rifletterà anche all'esterno della funzione. Questo comportamento è utile per condividere e aggiornare in modo efficiente i dati tra più funzioni, ma richiede anche cautela per evitare effetti collaterali indesiderati.

#### Esempio: modifica di un vettore in una funzione

Ecco un esempio che dimostra come i vettori vengono passati per riferimento e modificati:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Risultato: `#(10 99 30)`

#### Spiegazione passo passo

1. **Crea un vettore:** `my-vector` viene inizializzato con i valori `10`, `20` e `30`.
2. **Passa a una funzione:** `my-vector` viene passato a `modify-vector` insieme all'indice e al nuovo valore da aggiornare.
3. **Modifica nella funzione:** La procedura `vector-set!` aggiorna il valore nell'indice specificato direttamente nel vettore originale.
4. **Riflette modifiche:** poiché i vettori vengono passati per riferimento, le modifiche apportate all'interno della funzione si riflettono nel vettore originale.

#### Implicazioni del passaggio per riferimento

- **Prestazioni:** Il passaggio di vettori per riferimento è efficiente perché evita di copiare strutture di grandi dimensioni.
- **Effetti collaterali:** prestare attenzione quando si condividono vettori tra funzioni per evitare modifiche involontarie ai dati condivisi.

### Operazioni sui vettori

Scheme fornisce diverse procedure integrate per lavorare con i vettori, tra cui:

- `vector-length`: restituisce il numero di elementi in un vettore.
- `vector->list`: converte un vettore in un elenco.
- `list->vector`: converte un elenco in un vettore.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Risultato:

- `(vector-length (vector 1 2 3))` restituisce `3`
- `(vector->list (vector 1 2 3))` restituisce `(1 2 3)`
- `(list->vector (list 1 2 3))` restituisce `#(1 2 3)`

### Vettori nidificati

I vettori in Scheme possono contenere altri vettori come elementi, creando una struttura nidificata.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Crea un vettore di tre elementi, ognuno dei quali è a sua volta un vettore.

Risultato: **`#(#(1 2) #(3 4) #(5))`**

#### Accesso ai dati annidati

Per accedere agli elementi all'interno di un vettore nidificato, utilizzare `vector-ref` più volte per navigare nella struttura.

#### Esempio: accesso agli elementi

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Riepilogo

- I **Vettori** nello Schema sono strutture dati indicizzate a dimensione fissa.
- Utilizzare `vector` per creare un vettore, `vector-ref` per accedere agli elementi e `vector-set!` per aggiornare gli elementi.
- Le procedure integrate come `vector-length`, `vector->list` e `list->vector` consentono operazioni flessibili.
- I vettori nidificati consentono strutture di dati complesse e gerarchiche.