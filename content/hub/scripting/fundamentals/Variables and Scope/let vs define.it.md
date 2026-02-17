---
title: "Denominato let o definizione locale"
type: docs
weight: 5
---
Entrambi **denominati `let`** e **local `define`** sono potenti strumenti in Scheme per strutturare il codice, ma hanno scopi diversi. Capire quando utilizzarli aiuta a creare script puliti, modulari ed efficienti.

### Panoramica

- **Denominato `let`**: un costrutto che combina associazione di variabili e ricorsione in un ambito localizzato, tipicamente utilizzato per calcoli iterativi o ricorsivi.
- **Locale `define`**: un modo per definire funzioni o variabili di supporto nell'ambito di una funzione di inclusione, rendendole riutilizzabili in diverse parti di quella funzione.

---

### Chiamato `let`

#### Caratteristiche:
1. Combina associazioni di variabili e ricorsione in un unico costrutto.
2. Limitato al corpo del blocco `let`.
3. Ideale per la **ricorsione localizzata** o processi iterativi specifici per una singola attività.

#### Sintassi
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Esempio: somma degli elementi di una lista
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Risultato**: `10`

- **Come funziona**: la funzione `loop` è definita all'interno di `let`, consentendo chiamate ricorsive con associazioni aggiornate.

---

### Locale `define`

#### Caratteristiche:
1. Consente la creazione di funzioni o variabili di supporto riutilizzabili all'interno della funzione di inclusione.
2. Destinato alla funzione di contenimento ma visibile in tutto il suo corpo.
3. Ideale per modularizzare il codice con più passaggi o logica riutilizzabile.

#### Sintassi
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Esempio: elaborazione di più valori
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Risultato**: `41` (Calcola \(2^2 + 3^3 + 4^2\))

- **Come funziona**: le funzioni di supporto `square` e `cube` sono riutilizzabili all'interno della funzione `process-values`, abilitando la logica modulare.

---

### Differenze chiave

| **Aspetto** | **Nominato `let`** | **Locale `define`** |
|-----------------|------------------------------------------------||------------------------------------------|
| **Scopo** | Combina ricorsione e iterazione in modo localizzato. | Definisce funzioni o variabili di supporto riutilizzabili. |
| **Ambito** | Limitato al corpo del blocco `let`.           | Visibile in tutta la funzione di contenimento.      |
| **Riutilizzabilità** | Non riutilizzabile al di fuori del blocco `let`.             | Riutilizzabile più volte all'interno della funzione.    |
| **Miglior caso d'uso** | Ricorsione o iterazione localizzata legata a una singola attività. | Codice modulare con più passaggi riutilizzabili. |
| **Sintassi** | Combina associazione e ricorsione in un unico costrutto.  | Definisce esplicitamente funzioni o variabili.      |

---

### Quando utilizzare il nome `let`

1. **Logica monouso**: quando la ricorsione o l'iterazione è specifica per un singolo calcolo.
2. **Incapsulamento**: per evitare di aggiungere nomi di funzioni extra allo spazio dei nomi della funzione che la racchiude.
3. **Iterazione**: quando si gestiscono variabili intermedie in un costrutto di loop.

**Esempio: calcolo fattoriale**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Risultato**: `120`

---

### Quando utilizzare Local `define`

1. **Helper riutilizzabili**: quando la logica deve essere riutilizzata in più parti della funzione.
2. **Design modulare**: per suddividere calcoli complessi in sottoattività più piccole e denominate.
3. **Passaggi multipli**: quando sono necessarie più funzioni di supporto per diverse parti del calcolo.**Esempio: elaborazione degli input**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Risultato**: `(13 36)` (Calcola \(2^2 + 3^2\) e \(2^2 \cdot 3^2\))

---

### Combinazione di dichiarazione e input in Named `let`

Una delle caratteristiche più potenti di un nome `let` è la sua capacità di combinare la **dichiarazione di variabili locali** e i **parametri di input** per la ricorsione in un unico costrutto. Ciò rende il nome `let` conciso ed espressivo per attività iterative o ricorsive.

#### Dichiarazione di variabile locale
In un denominato `let`, i collegamenti tra parentesi agiscono come **variabili locali** che vengono inizializzate con valori specifici. Queste variabili hanno come ambito il corpo di `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` e `y`** sono variabili locali definite e inizializzate come parte di `let`.

---

#### Parametri di input per la ricorsione
Le stesse variabili fungono anche da **parametri di input** per le chiamate ricorsive al nome `let`. Quando il nome `let` richiama se stesso, aggiorna queste variabili con nuovi valori.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Prima iterazione**: `x = 1`, `y = 2`
- **Seconda iterazione**: `x = 2`, `y = 4`
- **Terza iterazione**: `x = 3`, `y = 8` e così via...

---

#### Equivalente utilizzando locale `define`

Un nome `let` include l'inizializzazione della variabile come parte della sua sintassi. Ciò elimina la necessità di un passaggio separato per impostare i valori iniziali. I due esempi seguenti sono equivalenti:

##### Utilizzo del nome `let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Utilizzo locale `define`
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Entrambi eseguono lo stesso calcolo, ma il nome `let` combina la dichiarazione della variabile e l'impostazione della ricorsione in un unico costrutto conciso.

---

#### Vantaggi della combinazione di dichiarazione e input

1. **Concisione**: denominato `let` riduce il boilerplate unendo l'inizializzazione delle variabili e la ricorsione in un unico costrutto.
2. **Chiarezza**: Chiarisce che la ricorsione è locale a `let` e legata a un'attività specifica.
3. **Incapsulamento**: la logica ricorsiva rimane autonoma e non inquina lo spazio dei nomi della funzione di inclusione.

Questa natura a duplice scopo di un nome `let`, sia come dichiarazione di variabile che come meccanismo di input ricorsivo, è ciò che lo rende una funzionalità potente e unica nella programmazione di Scheme.

### Riepilogo

- Utilizzare **denominato `let`** per la **ricorsione localizzata** o **iterazione**, soprattutto quando la logica è strettamente collegata a una singola attività.
- Utilizzare **local `define`** per **modularizzare il codice** con funzioni o variabili di supporto riutilizzabili.

Comprendendo le loro differenze, puoi scrivere programmi Scheme più concisi, organizzati e gestibili.