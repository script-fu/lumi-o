---
title: "Simboli"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4ae0cc2f5749cbe997d6fa25315ee3fe54646eb065b4dba0114778c75a889ae5
---
I simboli sono uno dei tipi di dati fondamentali in Scheme e rappresentano identificatori univoci e immutabili. Vengono utilizzati principalmente come chiavi, marcatori o segnaposto nei programmi, rendendoli essenziali per scrivere codice pulito ed espressivo.

Un simbolo in Scheme è simile a una stringa ma differisce in quanto i simboli sono **unici** e **atomici**. Ciò significa che è garantito che due simboli con lo stesso nome siano lo stesso oggetto, consentendo controlli rapidi di uguaglianza e un uso efficiente nelle strutture dati.

### Sintassi

Un simbolo è scritto come una sequenza di caratteri:

- Inizia con una lettera, seguita da lettere, cifre o caratteri speciali come `-`, `+` o `*`.
- Per impostazione predefinita, i simboli fanno distinzione tra maiuscole e minuscole.

Esempi:

```scheme
'hello       ; Un simbolo chiamato `hello`
'foo-bar     ; Un simbolo chiamato `foo-bar`
'*special*   ; Un simbolo chiamato `*special*`
```

## Creazione di simboli

I simboli vengono generalmente creati utilizzando l'operatore **quote** (`'`), che dice a Scheme di trattare il nome come un simbolo anziché valutarlo come una variabile o una funzione.

### Esempio

```scheme
'my-symbol   ; Crea il simbolo `my-symbol`
```

È inoltre possibile creare simboli a livello di programmazione utilizzando la procedura `string->symbol`, che converte una stringa in un simbolo.

```scheme
(string->symbol "dynamic-symbol")
```

**Risultato**: `'dynamic-symbol`


## Simboli a confronto

Poiché i simboli sono unici, puoi confrontarli in modo efficiente utilizzando `eq?`.

### Esempio

```scheme
(eq? 'apple 'apple)   ; #t (stesso simbolo)
(eq? 'apple 'orange)  ; #f (simboli diversi)
```

Ciò rende i simboli ideali per l'uso come chiavi nelle strutture dati o marcatori nel codice.

## Utilizzo dei simboli

I simboli sono spesso usati in Schema per:

1. **Chiavi negli elenchi di associazioni:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Restituisce (name . "Alice")
```

2. **Identificatori nel codice:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedure per lavorare con i simboli

Scheme fornisce procedure integrate per lavorare con i simboli:

| Procedura | Descrizione |
|--------------------|----------------------------------------------------------------------|
| **`symbol?`** | Controlla se un oggetto è un simbolo.                                            |
| **`eq?`** | Confronta due simboli per l'identità (confronto veloce).                       |
| **`symbol->string`** | Converte un simbolo in una stringa (utile per la visualizzazione o il debug).          |
| **`string->symbol`** | Converte una stringa in un simbolo (utile per la creazione dinamica di identificatori). |

### Esempi

```scheme
(symbol? 'example)            ; #t (vero: è un simbolo)
(symbol->string 'example)     ; Risultato: "example"
(string->symbol "new-symbol") ; Risultato: 'new-symbol
```

## Riepilogo

I simboli rappresentano un modo leggero ed efficiente per rappresentare identificatori, chiavi e marcatori in Scheme. La loro immutabilità e i rapidi controlli di identità li rendono ideali per molte attività di programmazione. Comprendere come utilizzare i simboli in modo efficace migliorerà la tua capacità di scrivere codice Scheme pulito ed espressivo.