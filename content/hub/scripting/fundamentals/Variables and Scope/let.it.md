---
title: "permettere"
type: docs
weight: 4
---
Il nome `let` viene utilizzato perché riflette le sue origini matematiche relative all'introduzione di collegamenti temporanei, come in _"Let \( x = 2 \) e \( y = 3 \)"_.

Un'istruzione `let` in Scheme è un **costrutto vincolante** utilizzato per definire variabili all'interno di un ambito localizzato. Ti consente di creare associazioni temporanee per variabili e quindi eseguire un blocco di codice utilizzando tali associazioni. Ciò è particolarmente utile per mantenere il codice modulare ed evitare l'inquinamento variabile globale.

Esistono tre forme principali di `let` in Scheme:

- **`let`**: standard consente di creare semplici associazioni locali.
- **`let*`**: Let sequenziale, dove i collegamenti possono dipendere dai risultati dei collegamenti precedenti.
- **Nominato `let`**: una forma speciale di `let` che crea cicli ricorsivi o procedure con nome.

Nella sua forma più semplice, `let` crea collegamenti di variabili locali e valuta un'espressione con tali collegamenti.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Attacchi**: un elenco di coppie in cui ciascuna coppia assegna un `value` a un `variable`.
- **Espressione**: il corpo di `let`, che può utilizzare le variabili definite localmente.

### Esempio

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Ciò definisce due variabili locali, `x` (10) e `y` (20).
- Quindi calcola `(+ x y)` utilizzando queste variabili.

**Risultato**: `30`

---

## Il costrutto `let*`

Il costrutto `let*` è simile a `let`, ma i collegamenti vengono valutati **in sequenza**. Ciò significa che le associazioni successive possono dipendere da quelle precedenti.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Esempio

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- Il primo collegamento assegna `10` a `x`.
- La seconda associazione calcola `y` come `(+ x 5)`, utilizzando il valore di `x`.
- Il corpo calcola `(* x y)`.

**Risultato**: `150`

---

## Chiamato `let`

Un nome `let` è una forma speciale di `let` che fornisce un nome per il blocco `let` stesso, trasformandolo in una procedura ricorsiva. Ciò è utile per creare cicli o calcoli ricorsivi.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nome**: al blocco `let` viene assegnato un nome, definendo effettivamente una funzione.
- **Binding**: valori iniziali per le variabili, simili a uno standard `let`.
- **Corpo**: l'espressione può chiamare ricorsivamente il nome `let`.

### Esempio: ciclo con nome `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- La funzione `loop` inizia con `n = 5` e `result = 1`.
- Se `n` è `0`, restituisce `result`.
- Altrimenti, si chiama ricorsivamente con `n - 1` e `result * n`.

**Risultato**: `120` (fattoriale di 5)

---

## Tabella riassuntiva| Costruisci | Descrizione | Caso d'uso |
|------------|------------------------------------------|--------------------------------------------------------------------|
| **`let`** | Definisce i collegamenti locali per le variabili.    | Utilizzare quando tutti gli attacchi sono indipendenti e non si basano l'uno sull'altro.     |
| **`let*`** | Definisce associazioni locali sequenziali.       | Utilizzare quando le associazioni successive dipendono dai risultati di quelle precedenti.           |
| **Nominato `let`** | Definisce procedure locali ricorsive. | Utilizzare per cicli, calcoli iterativi o ricorsione in un contesto locale. |

---

## Esempi

### Utilizzo di `let` per il calcolo locale

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Risultato**: `13` (Calcola `x² + y²`)

---

### Utilizzo di `let*` per le dipendenze sequenziali

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Risultato**: `8` (Calcola `x³`)

---

### Utilizzo del nome `let` per il calcolo ricorsivo

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Risultato**: `120` (fattoriale di 5)

---

Utilizzando `let`, `let*` e denominato `let`, Scheme consente la programmazione modulare, ricorsiva e sequenziale con regole di ambito chiare.