---
title: "Funzioni variadiche"
type: docs
weight: 2
---
Le **Funzioni variadiche** in Scheme sono funzioni che accettano un numero variabile di argomenti. Queste funzioni sono altamente versatili e consentono di creare codice flessibile e riutilizzabile. Nella programmazione funzionale, le funzioni variadiche semplificano le operazioni che richiedono l'elaborazione di un numero arbitrario di input, come la somma di un elenco di numeri o la concatenazione di stringhe.

Le funzioni variadiche sono particolarmente utili quando:

- Il numero di argomenti non può essere determinato in anticipo.
- È necessario applicare la stessa operazione a un elenco dinamico di input.
- Scrittura di utilità per l'aggregazione o la trasformazione dei dati.

### Sintassi delle funzioni variadiche

Le funzioni variadiche vengono definite utilizzando il simbolo `.` prima del nome dell'ultimo parametro. Quest'ultimo parametro raccoglie tutti gli argomenti rimanenti in un elenco.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Qualsiasi argomento fisso richiesto accettato dalla funzione.
- **`variadic-parameter`:** Un parametro speciale preceduto da `.` che raccoglie argomenti aggiuntivi sotto forma di elenco.
- **`body-expression`:** La logica eseguita quando viene chiamata la funzione.

### Esempi di funzioni variadiche

#### Funzione variadica di base

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Spiegazione**:
  - `numbers` raccoglie tutti gli argomenti in un elenco.
  - `apply` applica la funzione `+` a tutti gli elementi dell'elenco.

**Utilizzo**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Funzione variadica con parametri fissi

È possibile combinare parametri fissi con un parametro variadico per creare funzioni più flessibili.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Spiegazione**:
  - `prefix` è un argomento fisso.
  - `names` raccoglie gli argomenti rimanenti in un elenco.
  - Ogni nome è preceduto dalla stringa specificata utilizzando `map` e `lambda`.

**Utilizzo**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Combinazione di logica fissa e variadica

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Spiegazione**:
  - `collection-name` è un parametro fisso.
  - `items` raccoglie argomenti aggiuntivi in ​​un elenco.
  - La funzione concatena il nome della raccolta e gli elementi in un'unica stringa.

**Utilizzo**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Casi d'uso avanzati

#### Elaborazione di input arbitrari

Le funzioni variadiche eccellono nella gestione di dati arbitrari. Ecco un esempio per sommare solo numeri positivi:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtra i numeri non positivi prima della somma.

**Utilizzo**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Funzioni variadiche con logica ricorsiva

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Spiegazione**:
  - `first` gestisce il primo argomento.
  - `rest` raccoglie gli argomenti rimanenti in un elenco.
  - Calcola ricorsivamente il valore massimo.

**Utilizzo**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Vantaggi delle funzioni variadiche

- **Flessibilità:** gestiscono un'ampia gamma di casi di input.
- **Concisione:** riduce la necessità di molteplici funzioni sovraccaricate.
- **Operazioni dinamiche:** abilita l'elaborazione dei dati di runtime senza conoscere in anticipo il conteggio degli argomenti.

### Quando utilizzare le funzioni variadiche

Utilizzare le funzioni variadici quando:

- La funzione deve elaborare un numero sconosciuto di argomenti.
- Una singola operazione si applica a tutti gli input (ad esempio somma, concatenazione o mappatura).
- Semplificazione della logica di ordine superiore con argomenti dinamici.

Evitare le funzioni variadici quando:

- La convalida dell'input o il controllo del tipo sono complessi.
- Gli argomenti fissi sono sufficienti per la logica richiesta.
- La leggibilità è compromessa a causa di operazioni eccessivamente complesse.

### ConclusioneLe funzioni variadiche in Scheme forniscono un meccanismo robusto per la gestione degli input dinamici. Comprendendone la sintassi e l'utilizzo, puoi creare script flessibili e potenti che si adattano a vari scenari. Combinate con funzioni di ordine superiore, le funzioni variadiche rendono il codice più conciso ed espressivo.