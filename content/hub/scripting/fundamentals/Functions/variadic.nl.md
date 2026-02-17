---
title: "Variadische functies"
type: docs
weight: 2
---
**Variadische functies** in Schema zijn functies die een variabel aantal argumenten accepteren. Deze functies zijn zeer veelzijdig en stellen u in staat flexibele en herbruikbare code te creëren. Bij functioneel programmeren vereenvoudigen variadische functies bewerkingen waarbij een willekeurig aantal invoergegevens moet worden verwerkt, zoals het optellen van een lijst met getallen of het aaneenschakelen van tekenreeksen.

Variadische functies zijn vooral handig wanneer:

- Het aantal argumenten kan niet vooraf worden bepaald.
- U moet dezelfde bewerking toepassen op een dynamische lijst met ingangen.
- Schrijven van hulpprogramma's voor gegevensaggregatie of -transformatie.

### Syntaxis van variadische functies

Variadische functies worden gedefinieerd met het symbool `.` vóór de laatste parameternaam. Deze laatste parameter verzamelt alle resterende argumenten in een lijst.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Alle vereiste, vaste argumenten die de functie accepteert.
- **`variadic-parameter`:** Een speciale parameter voorafgegaan door `.` die aanvullende argumenten verzamelt als een lijst.
- **`body-expression`:** De logica die wordt uitgevoerd wanneer de functie wordt aangeroepen.

### Voorbeelden van variadische functies

#### Basisvariadische functie

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Uitleg**:
  - `numbers` verzamelt alle argumenten in een lijst.
  - `apply` past de functie `+` toe op alle elementen van de lijst.

**Gebruik**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Variadische functie met vaste parameters

U kunt vaste parameters combineren met een variabele parameter om flexibelere functies te creëren.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Uitleg**:
  - `prefix` is een vast argument.
  - `names` verzamelt de resterende argumenten in een lijst.
  - Elke naam wordt voorafgegaan door de opgegeven tekenreeks met `map` en `lambda`.

**Gebruik**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Vaste en variabele logica combineren

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Uitleg**:
  - `collection-name` is een vaste parameter.
  - `items` verzamelt aanvullende argumenten in een lijst.
  - De functie voegt de collectienaam en items samen in één enkele string.

**Gebruik**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Geavanceerde gebruiksscenario's

#### Willekeurige invoer verwerken

Variadische functies blinken uit in het verwerken van willekeurige gegevens. Hier is een voorbeeld voor het optellen van alleen positieve getallen:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtert niet-positieve getallen eruit voordat ze worden opgeteld.

**Gebruik**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Variadische functies met recursieve logica

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Uitleg**:
  - `first` verwerkt het eerste argument.
  - `rest` verzamelt de resterende argumenten in een lijst.
  - Berekent recursief de maximale waarde.

**Gebruik**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Voordelen van variadische functies

- **Flexibiliteit:** Ze verwerken een breed scala aan invoergevallen.
- **Beknoptheid:** Verminder de behoefte aan meerdere overbelaste functies.
- **Dynamische bewerkingen:** Schakel runtime-gegevensverwerking in zonder vooraf het aantal argumenten te kennen.

### Wanneer moet u variabele functies gebruiken?

Gebruik variadische functies wanneer:

- De functie moet een onbekend aantal argumenten verwerken.
- Eén enkele bewerking is van toepassing op alle invoer (bijvoorbeeld optellen, aaneenschakelen of in kaart brengen).
- Vereenvoudiging van logica van hogere orde met dynamische argumenten.

Vermijd variadische functies wanneer:

- Invoervalidatie of typecontrole is complex.
- Vaste argumenten zijn voldoende voor de vereiste logica.
- De leesbaarheid wordt aangetast door te complexe handelingen.

### ConclusieVariadische functies in Scheme bieden een robuust mechanisme voor het verwerken van dynamische invoer. Door de syntaxis en het gebruik ervan te begrijpen, kunt u flexibele en krachtige scripts maken die zich aan verschillende scenario's aanpassen. In combinatie met functies van hogere orde maken variadische functies uw code beknopter en expressiever.