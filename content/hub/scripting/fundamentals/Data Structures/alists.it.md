---
title: "Liste di associazione (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
---
Una **lista di associazioni** (o **alist**) è una struttura dati fondamentale in Scheme utilizzata per rappresentare raccolte di coppie chiave-valore. È implementato come un elenco di coppie, in cui ciascuna coppia associa una chiave (tipicamente un simbolo) a un valore. Gli elenchi sono semplici, flessibili e adatti a set di dati di piccole e medie dimensioni.

### Struttura di una lista di associazioni

Una alist è una lista in cui ogni elemento è una **coppia** (costruita con `cons`). Ogni coppia è composta da:

- **Chiave**: il primo elemento (tipicamente un simbolo).
- **Valore**: il secondo elemento, che può essere di qualsiasi tipo di dati.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Chiave**: `'name`, `'age`, `'city`
- **Valore**: `"Alice"`, `30`, `"Paris"`
- **Struttura**: un elenco di coppie:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Creazione di un elenco

È possibile creare un elenco costruendo manualmente coppie o costruendolo a livello di codice utilizzando `cons`.

#### Utilizzo della virgoletta singola (`'`)

La virgoletta singola (`'`) è una scorciatoia per **quoting**, che impedisce a Scheme di valutare l'espressione. Ciò lo rende ideale per creare elenchi statici in cui tutte le chiavi e i valori sono codificati.

```scheme
;; Definire manualmente un alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Aggiungere programmaticamente una nuova coppia
(define updated-alist (cons '(country . "France") alist))
```

**Risultato**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Utilizzo delle virgolette inverse (`` ` ``) e virgola (`,`)

L'operatore virgolette inverse (`` ` ``) è simile al singolo apice, ma consente di inserire dinamicamente espressioni valutate con la virgola (`,`). Ciò è utile per creare elenchi in cui chiavi o valori vengono calcolati in fase di esecuzione.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Risultato**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Esempio di confronto

Elenco statico utilizzando `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Elenco dinamico utilizzando `` ` `` e `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Accesso ai dati in un elenco

Per recuperare un valore da un elenco, è possibile utilizzare la funzione `assoc`, che cerca una coppia in base alla sua chiave.

```scheme
(assoc 'name alist)   ; Restituisce (name . "Alice")
(assoc 'country alist) ; Restituisce #f (chiave non trovata)
```

### Estrazione del valore

Una volta recuperata una coppia utilizzando `assoc`, utilizzare `cdr` per estrarre il valore:

```scheme
(cdr (assoc 'name alist))   ; Restituisce "Alice"
```

### Riepilogo delle caratteristiche principali

- **Citazione singola (`'`)**: crea un elenco statico in cui tutti gli elementi sono dati letterali.
- **Virgolette inverse (`` ` ``)**: Consente di creare alist dinamiche mescolando elementi statici ed espressioni valutate (con `,`).
- **Notazione punto (`.`)**: Utilizzato per costruire coppie, associando una chiave a un valore in un elenco.