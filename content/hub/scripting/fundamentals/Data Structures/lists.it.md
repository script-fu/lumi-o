---
title: "Elenchi"
type: docs
weight: 4
---
In Scheme, una **lista** è una struttura dati fondamentale utilizzata per raggruppare valori. Gli elenchi sono raccolte ordinate di elementi in cui ciascun elemento può essere di qualsiasi tipo, incluso un altro elenco. Gli elenchi sono ampiamente utilizzati in Scheme sia per l'archiviazione dei dati che per la struttura del programma.

### Esempio 1: Elenco semplice

```scheme
(list 1 2 3)
```

- Crea un elenco di tre elementi: `1`, `2` e `3`.

Risultato: **`(1 2 3)`**

---

#### Accesso agli elementi dell'elenco

È possibile accedere agli elementi di un elenco utilizzando le procedure `car` e `cdr`:

- `car` recupera il primo elemento di un elenco.
- `cdr` recupera il resto dell'elenco (tutto tranne il primo elemento).

#### Esempi

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Risultato:

- `(car my-list)` restituisce `1`
- `(cdr my-list)` restituisce `(2 3)`

---

#### Ricorsione semplice: scorrere una lista

Chiamando ricorsivamente `car` sul `cdr` di un elenco, è possibile elaborare ciascun elemento uno per uno finché l'elenco non viene attraversato. Ciò costituisce la base di molti algoritmi di elaborazione delle liste.

#### Esempio: stampa di ciascun elemento di un elenco

Ecco una semplice funzione ricorsiva per stampare ogni elemento in un elenco:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Caso base:** Se l'elenco è vuoto (`null? lst`), interrompe la ricorsione.
- **Caso ricorsivo:** Stampa il primo elemento (`car lst`), quindi chiama la funzione sul resto dell'elenco (`cdr lst`).

#### Esempio di utilizzo

```scheme
(print-elements (list 1 2 3))
```

Uscita:

- `"1"`
- `"2"`
- `"3"`

Risultato: "fatto"

---

#### Come funziona

1. La funzione recupera il primo elemento dell'elenco utilizzando `car` e lo elabora.
2. Quindi si richiama con il resto dell'elenco (`cdr`).
3. Questo processo si ripete finché l'elenco è vuoto (`null? lst`).

---

### Esempio 2: tipi misti

Gli elenchi possono includere elementi di diverso tipo, tra cui stringhe, valori booleani, numeri, altri elenchi o persino il risultato di espressioni:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Questo crea un elenco con:
  - Un numero (`42`)
  - Una stringa (`"hello"`)
  - Un valore booleano (`#t`)
  - Un'altra lista (`(1 2)`)
  - Il risultato di un'espressione (`(+ 3 4)`, che restituisce `7`)

Risultato: **`(42 "hello" #t (1 2) 7)`**

---

Questi esempi dimostrano la versatilità degli elenchi in Scheme, rendendoli un potente strumento per organizzare e manipolare i dati.

### Costruzione di elenchi

La procedura `cons` viene utilizzata per costruire una nuova lista combinando un elemento con una lista esistente.

```scheme
(cons new-element existing-list)
```

#### Esempio

```scheme
(cons 0 (list 1 2 3))
```

- Aggiunge `0` all'inizio dell'elenco `(1 2 3)`.

Risultato: **`(0 1 2 3)`**

---

### Controllo degli elenchi

La procedura `list?` controlla se un dato valore è una lista.

```scheme
(list? value)
```

#### Esempio: elenco?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Risultato:

- `(list? (list 1 2 3))` restituisce `#t` (vero)
- `(list? 42)` restituisce `#f` (falso)

---

### Operazioni sulle liste

Scheme fornisce diverse procedure integrate per lavorare con gli elenchi, tra cui:

- `length`: restituisce il numero di elementi in un elenco.
- `append`: combina due o più elenchi in uno solo.
- `reverse`: restituisce un nuovo elenco con gli elementi in ordine inverso.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Risultato:

- `(length (list 1 2 3))` restituisce `3`
- `(append (list 1 2) (list 3 4))` restituisce `(1 2 3 4)`
- `(reverse (list 1 2 3))` restituisce `(3 2 1)`#### Utilizzo di `list-ref`

La procedura `list-ref` recupera l'elemento in un indice specificato di un elenco (indice in base zero).

```scheme
(list-ref lst index)
```

- **`lst`**: l'elenco da cui recuperare l'elemento.
- **`index`**: un indice in base zero che indica quale elemento restituire.

##### Esempio: rif-elenco

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Risultato: `30`

---

### Elenchi nidificati

Gli elenchi in Scheme possono contenere altri elenchi come elementi, creando una struttura nidificata.

#### Esempio: creazione di un elenco nidificato

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Crea un elenco di tre elementi, ognuno dei quali è esso stesso un elenco.

Risultato: **`((1 2) (3 4) (5))`**

---

#### Accesso ai dati annidati

Per accedere agli elementi all'interno di un elenco nidificato, è possibile utilizzare le combinazioni di `car` e `cdr` per navigare attraverso la struttura.

#### Esempio: accesso agli elementi

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Spiegazione

1. **`car nested-list`**:
   - Recupera il primo elemento di `nested-list`, che è `(1 2)`.

2. **`car (car nested-list)`**:
   - Recupera il primo elemento di `(1 2)`, che è `1`.

3. **`cdr (car nested-list)`**:
   - Recupera il resto di `(1 2)`, ovvero `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Recupera il primo elemento di `(2)`, che è `2`.

---

#### Esempio: accesso agli elementi da altre sottoliste

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Questo approccio consente di navigare e accedere sistematicamente a elementi specifici in un elenco nidificato, offrendo una potente flessibilità per lavorare con dati gerarchici.

### Riepilogo

- Le **Elenchi** in Scheme sono strutture dati versatili ed essenziali.
- Utilizzare `list` per creare un elenco, `car` e `cdr` per accedere agli elementi e `cons` per costruire elenchi.
- Procedure integrate come `length`, `append`, `reverse` e `list-ref` rendono le operazioni sugli elenchi semplici ed efficienti.
- Gli elenchi possono essere nidificati, consentendo strutture di dati complesse per casi d'uso avanzati.