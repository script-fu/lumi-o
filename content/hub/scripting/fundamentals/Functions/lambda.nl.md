---
title: "Lambda-functies"
type: docs
weight: 1
---
**Lambda-functies** in Scheme zijn anonieme functies, wat betekent dat het functies zonder naam zijn. Deze functies worden inline gedefinieerd en worden doorgaans gebruikt voor korte, eenmalige handelingen. De `lambda` constructie is een krachtig hulpmiddel bij functioneel programmeren, waarmee u in een handomdraai beknopte en flexibele logica kunt creëren.

Lambda-functies zijn vooral handig wanneer:

- U heeft een kleine functie nodig voor een specifiek, tijdelijk doel.
- Functies doorgeven als argumenten aan functies van hogere orde, zoals `map`, `filter`, of `fold`.
- Functies retourneren van andere functies.

### Syntaxis van Lambda-functies

Lambda-functies kunnen afzonderlijk worden gedefinieerd...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...of onmiddellijk aangeroepen:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** De parameters die de functie accepteert.
- **`body-expression`:** De logica die wordt uitgevoerd wanneer de functie wordt aangeroepen.
- **Onmiddellijke aanroep:** Het tweede formulier toont een lambda die onmiddellijk wordt aangeroepen met argumenten.

### Voorbeelden van Lambda-functies

#### Lambda gebruiken voor eenvoudige berekeningen

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Hier:

- Er is een lambda-functie gemaakt om twee getallen toe te voegen (`x` en `y`).
- De functie wordt onmiddellijk aangeroepen met de argumenten `3` en `5`.

#### Inline Lambda-functies

Het volgende voorbeeld laat zien hoe u `for-each` kunt gebruiken met zowel een benoemde functie als een lambda-functie:

**Een benoemde functie gebruiken:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Uitleg**:
  - `print-item` is een benoemde functie die een getal omzet in een string (`number->string`) en dit afdrukt met `lumi-message`.
  - `for-each` past `print-item` toe op elk element in de lijst `(1 2 3 4)`.

**Uitgang**: 1 2 3 4

**Een Lambda-functie gebruiken:**

Dezelfde logica kan inline met een lambda-functie worden geschreven, waardoor de noodzaak van een afzonderlijke benoemde functie wordt vermeden:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Uitleg**:
  - De `(lambda (x) (lumi-message (number->string x)))` definieert een anonieme functie.
  - Deze functie wordt toegepast op elk element van de lijst `(1 2 3 4)` door `for-each`.

**Uitgang**: 1 2 3 4

#### Lambda functioneert als argumenten

Lambda-functies worden vaak rechtstreeks doorgegeven aan functies van hogere orde, zoals `map` of `filter`.

#### Een lijst met getallen kwadrateren

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- De functie `lambda` maakt elk element van de lijst vierkant.
- De functie `map` past `lambda` toe op elk element.

#### Lambda functioneert als retourwaarden

U kunt een lambda-functie van een andere functie retourneren om dynamisch gedrag te creëren.

#### Een optelfunctie genereren

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` genereert een nieuwe lambda-functie die een specifiek nummer toevoegt (`n`).
- De geretourneerde lambda wordt opgeslagen in `add5`, dat `5` toevoegt aan de invoer.

#### Lambda gebruiken met `let`

Lambda's worden vaak gebruikt met `let` om lokaal bereikbare, tijdelijke functies te creëren.

#### Lokale Lambda voor toevoeging

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- De `let` bindt een lambda-functie aan de naam `add`.
- De lambda wordt dan gebruikt als een normale functie binnen het bereik `let`.

#### Lambda's combineren met functies van hogere orde

Lambda's schitteren in combinatie met functies van hogere orde om complexe datatransformaties uit te voeren.

#### Even getallen filteren

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- De `lambda` controleert of een getal even is.
- De functie `filter` gebruikt de lambda om alleen de even nummers uit de lijst te behouden.

### Voordelen van Lambda-functies

- ** Beknoptheid: ** Lambda's verminderen de standaardcode door de noodzaak weg te nemen om afzonderlijke benoemde functies te definiëren.
- **Flexibiliteit:** U kunt ze definiëren en gebruiken waar ze nodig zijn, waardoor de code modulairer wordt.
- **Verbeterde leesbaarheid:** Voor korte, specifieke taken maken lambdas de bedoeling duidelijk zonder de code te vervuilen met extra benoemde functies.

### Wanneer moet ik Lambda-functies gebruiken?

Gebruik lambdafuncties wanneer:

- De logica is kort en staat op zichzelf.
- De functie is slechts tijdelijk of binnen een bepaalde omvang nodig.
- Je werkt met hogere-orde functies zoals `map`, `filter`, of `reduce`.

Vermijd het gebruik van lambda's voor complexe logica met meerdere regels, omdat dit de leesbaarheid kan verminderen. Voor uitgebreidere bewerkingen gebruikt u in plaats daarvan een benoemde functie.

### Conclusie

Lambda-functies in Scheme bieden een beknopte en krachtige manier om anonieme functies voor specifieke taken te definiëren. Hun flexibiliteit en gebruiksgemak maken ze tot een essentieel hulpmiddel voor elke Scheme-programmeur. Als u begrijpt hoe u `lambda` effectief kunt gebruiken, kunt u schonere, meer modulaire en efficiënte scripts schrijven.