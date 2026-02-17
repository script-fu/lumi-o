---
title: "Vectoren"
type: docs
weight: 5
---
In Scheme is een vector een andere fundamentele gegevensstructuur die wordt gebruikt om waarden te groeperen. In tegenstelling tot lijsten zijn vectoren geïndexeerde verzamelingen van elementen met een vaste grootte, waardoor snellere willekeurige toegang en updates mogelijk zijn. Elk element in een vector kan van elk type zijn, inclusief een andere vector. Vectoren worden weergegeven met # gevolgd door haakjes. `#(1 2 3)`

Hoewel vectoren en lijsten op elkaar lijken, dienen ze verschillende doeleinden bij het programmeren van schema's:

- Lijsten worden vaker gebruikt voor recursieve bewerkingen en dynamische structuren, omdat hun implementatie met gekoppelde knooppunten een efficiënte manipulatie van het begin en de doorloop ervan door middel van recursieve ontbinding mogelijk maakt.

- Vectoren zijn daarentegen geoptimaliseerd voor scenario's waarin willekeurige toegang tot elementen of updates op specifieke indices vereist is, waardoor ze geschikter zijn voor gebruiksscenario's zoals opzoektabellen, configuraties met een vaste grootte of prestatiekritische geïndexeerde bewerkingen.

In wezen zijn lijsten de logische keuze voor recursieve algoritmen en gegevens met een dynamische grootte, terwijl vectoren uitblinken wanneer toegangspatronen met een vaste grootte of geïndexeerde waarden van het grootste belang zijn.

### Eenvoudige vectoren

```scheme
(vector 1 2 3)
```

- Creëert een vector van drie elementen: `1`, `2` en `3`.

Resultaat: **`#(1 2 3)`**

#### Toegang tot vectorelementen

Elementen in een vector worden benaderd met behulp van de `vector-ref` procedure, deze haalt het element op bij een gespecificeerde index (beginnend bij `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iteratie: elk element in een vector verwerken

U kunt een vector herhalen met behulp van een lus of recursie. Schema biedt `vector-length` om de grootte van een vector te bepalen. Hier is een eenvoudige lus om elk element in een vector af te drukken:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Basisscenario:** Als de index `i` de lengte van de vector bereikt, stop dan de lus.
- **Recursief hoofdlettergebruik:** Druk het element af op index `i` en verhoog vervolgens `i`.

#### Voorbeeldgebruik

```scheme
(print-elements (vector 1 2 3))
```

Resultaat:

- `"1"`
- `"2"`
- `"3"`

Resultaat: "klaar"

### Gemengde vectoren

Vectoren kunnen elementen van verschillende typen bevatten, waaronder tekenreeksen, booleans, getallen, andere vectoren of zelfs het resultaat van uitdrukkingen:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Hierdoor ontstaat een vector met:
  - Een nummer (`42`)
  - Een string (`"hello"`)
  - Een Booleaanse (`#t`)
  - Nog een vector (`#(1 2)`)
  - Het resultaat van een uitdrukking (`(+ 3 4)`, die resulteert in `7`)

Resultaat: **`#(42 "hello" #t #(1 2) 7)`**

### Vectoren construeren

Vectoren worden gemaakt met `vector`, of door `make-vector` te gebruiken om een vector van een vaste grootte met een initiële waarde te maken.

```scheme
(make-vector 5 0)
```

Creëert een vector met de grootte `5` waarbij alle elementen zijn geïnitialiseerd naar `0`.

Resultaat: `#(0 0 0 0 0)`

### Vectoren bijwerken

De `vector-set!` procedure werkt een element in een vector bij op een gespecificeerde index.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Resultaat: `#(1 42 3)`

### Controleren op vectoren

De `vector?` procedure controleert of een gegeven waarde een vector is.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Resultaat:

- `(vector? (vector 1 2 3))` retourneert `#t` (waar)
- `(vector? 42)` retourneert `#f` (onwaar)

### Vectoren en pass-by-referentiegedragIn Schema zijn vectoren veranderlijk en worden ze door verwijzing doorgegeven. Dit betekent dat wanneer u een vector aan een functie doorgeeft, de functie de originele vector rechtstreeks kan wijzigen. Alle wijzigingen die binnen de functie aan de vector worden aangebracht, worden ook buiten de functie weerspiegeld. Dit gedrag is handig voor het efficiënt delen en bijwerken van gegevens over meerdere functies, maar vereist ook voorzichtigheid om onbedoelde bijwerkingen te voorkomen.

#### Voorbeeld: een vector in een functie wijzigen

Hier is een voorbeeld dat laat zien hoe vectoren ter referentie worden doorgegeven en gewijzigd:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Resultaat: `#(10 99 30)`

#### Stap-voor-stap uitleg

1. **Maak een vector:** `my-vector` wordt geïnitialiseerd met de waarden `10`, `20` en `30`.
2. **Doorgeven aan een functie:** `my-vector` wordt doorgegeven aan `modify-vector` samen met de index en de nieuwe waarde die moet worden bijgewerkt.
3. **Wijzigen in functie:** De `vector-set!` procedure werkt de waarde bij de opgegeven index rechtstreeks in de originele vector bij.
4. **Wijzigingen weerspiegelen:** Omdat vectoren door middel van referentie worden doorgegeven, worden wijzigingen die binnen de functie worden aangebracht, weerspiegeld in de originele vector.

#### Implicaties van pass-by-referentie

- **Prestaties:** Het doorgeven van vectoren via referentie is efficiënt omdat het kopiëren van grote structuren wordt vermeden.
- **Bijwerkingen:** Wees voorzichtig bij het delen van vectoren tussen functies om onbedoelde wijzigingen in gedeelde gegevens te voorkomen.

### Bewerkingen op vectoren

Scheme biedt verschillende ingebouwde procedures voor het werken met vectoren, waaronder:

- `vector-length`: Geeft het aantal elementen in een vector terug.
- `vector->list`: Converteert een vector naar een lijst.
- `list->vector`: Converteert een lijst naar een vector.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Resultaat:

- `(vector-length (vector 1 2 3))` retourneert `3`
- `(vector->list (vector 1 2 3))` retourneert `(1 2 3)`
- `(list->vector (list 1 2 3))` retourneert `#(1 2 3)`

### Geneste vectoren

Vectoren in Schema kunnen andere vectoren als elementen bevatten, waardoor een geneste structuur ontstaat.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Creëert een vector van drie elementen, die elk zelf een vector zijn.

Resultaat: **`#(#(1 2) #(3 4) #(5))`**

#### Toegang tot geneste gegevens

Om toegang te krijgen tot elementen binnen een geneste vector, gebruikt u `vector-ref` meerdere keren om door de structuur te navigeren.

#### Voorbeeld: toegang tot elementen

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Samenvatting

- **Vectoren** in Scheme zijn geïndexeerde datastructuren met een vaste grootte.
- Gebruik `vector` om een ​​vector te maken, `vector-ref` om toegang te krijgen tot elementen, en `vector-set!` om elementen bij te werken.
- Ingebouwde procedures zoals `vector-length`, `vector->list` en `list->vector` maken flexibele bewerkingen mogelijk.
- Geneste vectoren maken complexe, hiërarchische datastructuren mogelijk.