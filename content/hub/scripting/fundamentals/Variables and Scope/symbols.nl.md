---
title: "Symbolen"
type: docs
weight: 6
---
Symbolen zijn een van de belangrijkste gegevenstypen in Scheme en vertegenwoordigen unieke, onveranderlijke identificatiegegevens. Ze worden voornamelijk gebruikt als sleutels, markeringen of tijdelijke aanduidingen in programma's, waardoor ze essentieel zijn voor het schrijven van schone en expressieve code.

Een symbool in Schema lijkt op een tekenreeks, maar verschilt daarin dat symbolen **uniek** en **atomair** zijn. Dit betekent dat twee symbolen met dezelfde naam gegarandeerd hetzelfde object zijn, waardoor snelle gelijkheidscontroles en efficiënt gebruik in datastructuren mogelijk zijn.

### Syntaxis

Een symbool wordt geschreven als een reeks tekens:

- Begint met een letter, gevolgd door letters, cijfers of speciale tekens zoals `-`, `+`, of `*`.
- Symbolen zijn standaard hoofdlettergevoelig.

Voorbeelden:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## Symbolen maken

Symbolen worden doorgaans gemaakt met behulp van de **quote**-operator (`'`), die Scheme vertelt de naam als een symbool te behandelen in plaats van deze als een variabele of functie te evalueren.

### Voorbeeld

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

U kunt ook programmatisch symbolen maken met behulp van de `string->symbol` procedure, die een tekenreeks naar een symbool converteert.

```scheme
(string->symbol "dynamic-symbol")
```

**Resultaat**: `'dynamic-symbol`


## Symbolen vergelijken

Omdat symbolen uniek zijn, kunt u ze efficiënt vergelijken met `eq?`.

### Voorbeeld

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

Dit maakt symbolen ideaal voor gebruik als sleutels in datastructuren of markeringen in uw code.

## Symbolen gebruiken

In Scheme worden vaak symbolen gebruikt voor:

1. **Sleutels in associatielijsten:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **ID's in code:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedures voor het werken met symbolen

Schema biedt ingebouwde procedures voor het werken met symbolen:

| Werkwijze | Beschrijving |
|------------------|--------------------------------------------------------------------|
| **`symbol?`** | Controleert of een object een symbool is.                                            |
| **`eq?`** | Vergelijkt twee symbolen voor identiteit (snelle vergelijking).                       |
| **`symbol->string`** | Converteert een symbool naar een tekenreeks (handig voor weergave of foutopsporing).          |
| **`string->symbol`** | Converteert een tekenreeks naar een symbool (handig voor het dynamisch maken van identificatiegegevens). |

### Voorbeelden

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## Samenvatting

Symbolen zijn een lichtgewicht, efficiënte manier om identificatiegegevens, sleutels en markeringen in Scheme weer te geven. Hun onveranderlijkheid en snelle identiteitscontroles maken ze ideaal voor veel programmeertaken. Als u begrijpt hoe u symbolen effectief kunt gebruiken, vergroot u uw vermogen om schone en expressieve Scheme-code te schrijven.