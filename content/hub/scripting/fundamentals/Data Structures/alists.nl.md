---
title: "Verenigingslijsten (alisten)"
type: docs
weight: 6
---
Een **associatielijst** (of **alist**) is een fundamentele gegevensstructuur in Scheme die wordt gebruikt om verzamelingen sleutel-waardeparen weer te geven. Het wordt geïmplementeerd als een lijst met paren, waarbij elk paar een sleutel (meestal een symbool) associeert met een waarde. Alists zijn eenvoudig, flexibel en zeer geschikt voor kleine tot middelgrote datasets.

### Structuur van een associatielijst

Een alist is een lijst waarbij elk element een **paar** is (opgebouwd met `cons`). Elk paar bestaat uit:

- **Sleutel**: het eerste element (meestal een symbool).
- **Waarde**: het tweede element, dat van elk gegevenstype kan zijn.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Sleutel**: `'name`, `'age`, `'city`
- **Waarde**: `"Alice"`, `30`, `"Paris"`
- **Structuur**: een lijst met paren:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Een lijst maken

U kunt een alist maken door handmatig paren te construeren of deze programmatisch op te bouwen met `cons`.

#### Gebruik van één enkel citaat (`'`)

Het enkele aanhalingsteken (`'`) is een afkorting voor **quoting**, waardoor Scheme de expressie niet kan evalueren. Dit maakt het ideaal voor het maken van statische alisten waarbij alle sleutels en waarden hardgecodeerd zijn.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Resultaat**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Het aanhalingsteken gebruiken ("` ` ``) and Comma (`,`)

De backquote-operator ("` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Dit is handig voor het maken van alists waarbij sleutels of waarden tijdens runtime worden berekend.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Resultaat**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Voorbeeldvergelijking

Statische lijst met `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Dynamische lijst met `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Toegang tot gegevens in een lijst

Om een waarde uit een lijst op te halen, kunt u de functie `assoc` gebruiken, die een paar opzoekt aan de hand van de sleutel.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### De waarde extraheren

Zodra u een paar heeft opgehaald met `assoc`, gebruikt u `cdr` om de waarde te extraheren:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Samenvatting van de belangrijkste kenmerken

- **Enkel citaat (`'`)**: Creëert een statische lijst waarin alle elementen letterlijke gegevens zijn.
- **Backquote ("` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Puntnotatie (`.`)**: wordt gebruikt om paren te construeren, waarbij een sleutel wordt gekoppeld aan een waarde in een lijst.