---
title: "Lijsten"
type: docs
weight: 4
---
In Scheme is een **lijst** een fundamentele gegevensstructuur die wordt gebruikt om waarden te groeperen. Lijsten zijn geordende verzamelingen elementen waarbij elk element van elk type kan zijn, inclusief een andere lijst. Lijsten worden in Scheme veel gebruikt voor zowel gegevensopslag als programmastructuur.

### Voorbeeld 1: Eenvoudige lijst

```scheme
(list 1 2 3)
```

- Creëert een lijst met drie elementen: `1`, `2` en `3`.

Resultaat: **`(1 2 3)`**

---

#### Lijstelementen openen

Elementen in een lijst zijn toegankelijk via de procedures `car` en `cdr`:

- `car` haalt het eerste element van een lijst op.
- `cdr` haalt de rest van de lijst op (alles behalve het eerste element).

#### Voorbeelden

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Resultaat:

- `(car my-list)` retourneert `1`
- `(cdr my-list)` retourneert `(2 3)`

---

#### Eenvoudige recursie: itereren door een lijst

Door `car` recursief aan te roepen op de `cdr` van een lijst, kunt u elk element één voor één verwerken totdat de lijst wordt doorlopen. Dit vormt de basis van veel algoritmen voor lijstverwerking.

#### Voorbeeld: elk element van een lijst afdrukken

Hier is een eenvoudige recursieve functie om elk element in een lijst af te drukken:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Basisscenario:** Als de lijst leeg is (`null? lst`), stop dan de recursie.
- **Recursief hoofdlettergebruik:** Druk het eerste element af (`car lst`) en roep vervolgens de functie op in de rest van de lijst (`cdr lst`).

#### Voorbeeldgebruik

```scheme
(print-elements (list 1 2 3))
```

Uitgang:

- `"1"`
- `"2"`
- `"3"`

Resultaat: "klaar"

---

#### Hoe het werkt

1. De functie haalt het eerste element van de lijst op met `car` en verwerkt dit.
2. Vervolgens roept het zichzelf aan met de rest van de lijst (`cdr`).
3. Dit proces herhaalt zich totdat de lijst leeg is (`null? lst`).

---

### Voorbeeld 2: Gemengde typen

Lijsten kunnen elementen van verschillende typen bevatten, waaronder tekenreeksen, booleans, getallen, andere lijsten of zelfs het resultaat van expressies:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Hierdoor ontstaat een lijst met:
  - Een nummer (`42`)
  - Een string (`"hello"`)
  - Een boolean (`#t`)
  - Nog een lijst (`(1 2)`)
  - Het resultaat van een uitdrukking (`(+ 3 4)`, die resulteert in `7`)

Resultaat: **`(42 "hello" #t (1 2) 7)`**

---

Deze voorbeelden demonstreren de veelzijdigheid van lijsten in Scheme, waardoor ze een krachtig hulpmiddel zijn voor het organiseren en manipuleren van gegevens.

### Lijsten samenstellen

De `cons` procedure wordt gebruikt om een nieuwe lijst samen te stellen door een element te combineren met een bestaande lijst.

```scheme
(cons new-element existing-list)
```

#### Voorbeeld

```scheme
(cons 0 (list 1 2 3))
```

- Voegt `0` toe aan het begin van de lijst `(1 2 3)`.

Resultaat: **`(0 1 2 3)`**

---

### Controleren op lijsten

De `list?` procedure controleert of een bepaalde waarde een lijst is.

```scheme
(list? value)
```

#### Voorbeeld: lijst?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Resultaat:

- `(list? (list 1 2 3))` retourneert `#t` (waar)
- `(list? 42)` retourneert `#f` (onwaar)

---

### Bewerkingen op lijsten

Scheme biedt verschillende ingebouwde procedures voor het werken met lijsten, waaronder:

- `length`: Geeft het aantal elementen in een lijst terug.
- `append`: Combineert twee of meer lijsten in één.
- `reverse`: Geeft een nieuwe lijst terug met elementen in omgekeerde volgorde.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Resultaat:

- `(length (list 1 2 3))` retourneert `3`
- `(append (list 1 2) (list 3 4))` retourneert `(1 2 3 4)`
- `(reverse (list 1 2 3))` retourneert `(3 2 1)`#### `list-ref` gebruiken

De procedure `list-ref` haalt het element op bij een gespecificeerde index van een lijst (op nul gebaseerde index).

```scheme
(list-ref lst index)
```

- **`lst`**: De lijst waaruit het element moet worden opgehaald.
- **`index`**: een op nul gebaseerde index die aangeeft welk element moet worden geretourneerd.

##### Voorbeeld: lijstref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Resultaat: `30`

---

### Geneste lijsten

Lijsten in Schema kunnen andere lijsten als elementen bevatten, waardoor een geneste structuur ontstaat.

#### Voorbeeld: een geneste lijst maken

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Creëert een lijst van drie elementen, waarvan elk zelf een lijst is.

Resultaat: **`((1 2) (3 4) (5))`**

---

#### Toegang tot geneste gegevens

Om toegang te krijgen tot elementen binnen een geneste lijst, kunt u combinaties van `car` en `cdr` gebruiken om door de structuur te navigeren.

#### Voorbeeld: toegang tot elementen

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Uitleg

1. **`car nested-list`**:
   - Haalt het eerste element van `nested-list` op, namelijk `(1 2)`.

2. **`car (car nested-list)`**:
   - Haalt het eerste element van `(1 2)` op, namelijk `1`.

3. **`cdr (car nested-list)`**:
   - Haalt de rest van `(1 2)` op, wat `(2)` is.

4. **`car (cdr (car nested-list))`**:
   - Haalt het eerste element van `(2)` op, namelijk `2`.

---

#### Voorbeeld: toegang krijgen tot elementen uit andere sublijsten

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Met deze aanpak kunt u systematisch navigeren en toegang krijgen tot specifieke elementen in een geneste lijst, wat een krachtige flexibiliteit biedt bij het werken met hiërarchische gegevens.

### Samenvatting

- **Lijsten** in Scheme zijn veelzijdige en essentiële datastructuren.
- Gebruik `list` om een ​​lijst te maken, `car` en `cdr` om toegang te krijgen tot elementen, en `cons` om lijsten samen te stellen.
- Ingebouwde procedures zoals `length`, `append`, `reverse` en `list-ref` maken lijstbewerkingen eenvoudig en efficiënt.
- Lijsten kunnen worden genest, waardoor complexe datastructuren voor geavanceerde gebruiksscenario's mogelijk worden.