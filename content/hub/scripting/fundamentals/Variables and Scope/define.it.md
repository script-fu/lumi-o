---
title: "definire"
type: docs
weight: 3
---
L'istruzione `define` in Scheme è un costrutto versatile utilizzato per creare associazioni globali o locali. Viene comunemente utilizzato per definire variabili e funzioni, rendendole riutilizzabili e accessibili in uno script o in un ambito specifico. Comprendere `define` è fondamentale per scrivere programmi Scheme modulari, riutilizzabili e leggibili.

### Scopo di `define`

Il costrutto `define` ha molteplici scopi:
- **Definizione delle variabili**: assegna valori ai nomi delle variabili, rendendoli disponibili per un uso successivo.
- **Definizione di funzioni**: crea procedure riutilizzabili che incapsulano una logica specifica.
- **Definizioni locali**: se utilizzato all'interno di una funzione, `define` crea associazioni locali che non influiscono sullo spazio dei nomi globale.

---

### Definizione delle variabili con `define`

Un utilizzo di base di `define` è creare variabili che contengano valori costanti o calcolati.

#### Sintassi
```scheme
(define variable-name value)
```

#### Esempio: definizione di una costante
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Risultato**: `6.28318`

---

### Definizione delle funzioni con `define`

È possibile utilizzare `define` per creare procedure riutilizzabili.

#### Sintassi
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Esempio: definizione di una funzione semplice
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Risultato**: `16`

---

### Definizioni locali con `define`

Se utilizzato all'interno di una funzione, `define` crea associazioni locali accessibili solo all'interno della funzione di inclusione. Ciò evita di inquinare lo spazio dei nomi globale e aiuta a organizzare il codice.

#### Esempio: funzioni di supporto locali
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Risultato**: `41` (Calcola \(2^2 + 3^3 + 4^2\))

---

### Caratteristiche principali di `define`

1. **Ambito globale o locale**:
   - Se utilizzato al livello più alto, `define` crea variabili o funzioni globali.
   - Se utilizzato all'interno di un'altra funzione, `define` crea associazioni locali.

2. **Riutilizzabilità**:
   - Le funzioni definite con `define` possono essere riutilizzate più volte in contesti diversi.

3. **Leggibilità migliorata**:
   - Suddividere la logica in funzioni più piccole e ben denominate migliora la chiarezza e la manutenibilità del codice.

---

### Differenze tra `define` e `let`

| **Aspetto** | **`define`** | **`let`** |
|-------------------------|----------------------------------------------------|----------------------------------------------|
| **Scopo** | Crea associazioni globali o locali per variabili o funzioni. | Crea associazioni temporanee in un ambito localizzato. |
| **Ambito** | Globale quando è al massimo livello; local quando si trova all'interno di un'altra funzione. | Sempre locale al blocco `let`.       |
| **Riutilizzabilità** | Le funzioni e le variabili possono essere riutilizzate in più posti. | Le variabili sono temporaneamente legate a un singolo blocco. |
| **Sintassi** | Definisce esplicitamente variabili o funzioni.       | Combina il legame delle variabili con la valutazione delle espressioni. |