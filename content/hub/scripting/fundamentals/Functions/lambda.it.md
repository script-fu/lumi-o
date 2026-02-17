---
title: "Funzioni Lambda"
type: docs
weight: 1
---
Le **funzioni lambda** in Scheme sono funzioni anonime, ovvero sono funzioni senza nome. Queste funzioni sono definite in linea e vengono generalmente utilizzate per operazioni brevi e una tantum. Il costrutto `lambda` è un potente strumento di programmazione funzionale, che consente di creare al volo una logica concisa e flessibile.

Le funzioni Lambda sono particolarmente utili quando:

- Hai bisogno di una piccola funzione per uno scopo specifico e temporaneo.
- Passaggio di funzioni come argomenti a funzioni di ordine superiore come `map`, `filter` o `fold`.
- Restituzione di funzioni da altre funzioni.

### Sintassi delle funzioni Lambda

Le funzioni Lambda possono essere definite autonomamente...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...o invocato immediatamente:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** I parametri accettati dalla funzione.
- **`body-expression`:** La logica eseguita quando viene chiamata la funzione.
- **Invocazione immediata:** La seconda forma mostra un lambda che viene immediatamente chiamato con argomenti.

### Esempi di funzioni Lambda

#### Utilizzo di Lambda per calcoli semplici

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Qui:

- Viene creata una funzione lambda per aggiungere due numeri (`x` e `y`).
- La funzione viene immediatamente richiamata con gli argomenti `3` e `5`.

#### Funzioni Lambda in linea

L'esempio seguente dimostra come utilizzare `for-each` sia con una funzione denominata che con una funzione lambda:

**Utilizzo di una funzione con nome:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Spiegazione**:
  - `print-item` è una funzione con nome che converte un numero in una stringa (`number->string`) e lo stampa utilizzando `lumi-message`.
  - `for-each` applica `print-item` a ciascun elemento nell'elenco `(1 2 3 4)`.

**Uscita**: 1 2 3 4

**Utilizzo di una funzione Lambda:**

La stessa logica può essere scritta in linea con una funzione lambda, evitando la necessità di una funzione con nome separata:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Spiegazione**:
  - `(lambda (x) (lumi-message (number->string x)))` definisce una funzione anonima.
  - Questa funzione viene applicata a ciascun elemento dell'elenco `(1 2 3 4)` da `for-each`.

**Uscita**: 1 2 3 4

#### Funzioni Lambda come argomenti

Le funzioni Lambda vengono spesso passate direttamente a funzioni di ordine superiore come `map` o `filter`.

#### Quadratura di una lista di numeri

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- La funzione `lambda` quadra ogni elemento dell'elenco.
- La funzione `map` applica `lambda` a ciascun elemento.

#### Funzioni Lambda come valori restituiti

Puoi restituire una funzione lambda da un'altra funzione per creare un comportamento dinamico.

#### Generazione di una funzione sommatrice

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` genera una nuova funzione lambda che aggiunge un numero specifico (`n`).
- Il lambda restituito viene memorizzato in `add5`, che aggiunge `5` al suo input.

#### Utilizzo di Lambda con `let`

I lambda vengono spesso utilizzati con `let` per creare funzioni temporanee con ambito locale.

#### Lambda locale per aggiunta

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- `let` associa una funzione lambda al nome `add`.
- La lambda viene quindi utilizzata come una normale funzione nell'ambito `let`.

#### Combinazione di Lambda con funzioni di ordine superiore

I Lambda brillano se combinati con funzioni di ordine superiore per eseguire trasformazioni di dati complesse.

#### Filtraggio dei numeri pari

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- `lambda` controlla se un numero è pari.
- La funzione `filter` utilizza lambda per mantenere solo i numeri pari dall'elenco.

### Vantaggi delle funzioni Lambda

- **Concisione:** i Lambda riducono il codice standard eliminando la necessità di definire funzioni con nome separate.
- **Flessibilità:** puoi definirli e utilizzarli ovunque siano necessari, rendendo il codice più modulare.
- **Leggibilità migliorata:** per attività brevi e specifiche, le lambda rendono chiaro l'intento senza ingombrare il codice con funzioni con nome aggiuntive.

### Quando utilizzare le funzioni Lambda

Utilizza le funzioni lambda quando:

- La logica è breve e autonoma.
- La funzione è necessaria solo temporaneamente o in un ambito specifico.
- Stai lavorando con funzioni di ordine superiore come `map`, `filter` o `reduce`.

Evitare di utilizzare lambda per logica complessa su più righe, poiché ciò può ridurre la leggibilità. Per operazioni più estese, utilizzare invece una funzione denominata.

### Conclusione

Le funzioni Lambda in Scheme forniscono un modo conciso e potente per definire funzioni anonime per attività specifiche. La loro flessibilità e facilità d'uso li rendono uno strumento essenziale per qualsiasi programmatore di Scheme. Capire come utilizzare `lambda` in modo efficace ti aiuterà a scrivere script più puliti, più modulari ed efficienti.