---
title: "laten"
type: docs
weight: 4
---
De naam `let` wordt gebruikt omdat deze de wiskundige oorsprong van het introduceren van tijdelijke bindingen weergeeft, zoals in _"Let \( x = 2 \) en \( y = 3 \)"_.

Een `let`-instructie in Scheme is een **bindende constructie** die wordt gebruikt om variabelen binnen een gelokaliseerd bereik te definiëren. Hiermee kunt u tijdelijke bindingen voor variabelen maken en vervolgens een codeblok uitvoeren met behulp van die bindingen. Dit is met name handig om de code modulair te houden en globale variabele vervuiling te voorkomen.

Er zijn drie hoofdvormen van `let` in het schema:

- **`let`**: Standaard toegestaan voor het maken van eenvoudige lokale bindingen.
- **`let*`**: Sequentiële verhuur, waarbij bindingen kunnen afhangen van de resultaten van eerdere bindingen.
- **Genaamd `let`**: een speciale vorm van `let` die recursieve lussen of benoemde procedures creëert.

In de eenvoudigste vorm maakt `let` lokale variabelebindingen en evalueert een expressie met die bindingen.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Bindingen**: een lijst met paren waarbij elk paar een `value` toewijst aan een `variable`.
- **Expressie**: de hoofdtekst van `let`, die de lokaal gedefinieerde variabelen kan gebruiken.

### Voorbeeld

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Dit definieert twee lokale variabelen, `x` (10) en `y` (20).
- Vervolgens berekent het `(+ x y)` met behulp van deze variabelen.

**Resultaat**: `30`

---

## Het `let*`-construct

De constructie `let*` is vergelijkbaar met `let`, maar bindingen worden **opeenvolgend** geëvalueerd. Dit betekent dat latere bindingen kunnen afhangen van eerdere bindingen.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Voorbeeld

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- De eerste binding wijst `10` toe aan `x`.
- De tweede binding berekent `y` als `(+ x 5)`, met behulp van de waarde `x`.
- Het lichaam berekent `(* x y)`.

**Resultaat**: `150`

---

## Genoemd `let`

Een genaamd `let` is een speciale vorm van `let` die een naam geeft voor het `let` blok zelf, waardoor het een recursieve procedure wordt. Dit is handig voor het maken van lussen of recursieve berekeningen.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Naam**: het `let` blok krijgt een naam, waarmee feitelijk een functie wordt gedefinieerd.
- **Bindingen**: initiële waarden voor variabelen, vergelijkbaar met een standaard `let`.
- **Body**: de expressie kan de genoemde `let` recursief aanroepen.

### Voorbeeld: herhalen met de naam `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- De functie `loop` begint met `n = 5` en `result = 1`.
- Als `n` `0` is, retourneert het `result`.
- Anders roept het zichzelf recursief aan met `n - 1` en `result * n`.

**Resultaat**: `120` (Factoriaal van 5)

---

## Overzichtstabel| Construct | Beschrijving | Gebruiksscenario |
|------------|-------------------------------------|--------- ---------------------------------------------------------|
| **`let`** | Definieert lokale bindingen voor variabelen.    | Te gebruiken wanneer alle bindingen onafhankelijk zijn en niet op elkaar vertrouwen.     |
| **`let*`** | Definieert opeenvolgende lokale bindingen.       | Gebruik dit wanneer latere bindingen afhankelijk zijn van de resultaten van eerdere bindingen.           |
| **Genaamd `let`** | Definieert recursieve lokale procedures. | Gebruik voor lussen, iteratieve berekeningen of recursie in een lokale context. |

---

## Voorbeelden

### `let` gebruiken voor lokale berekeningen

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Resultaat**: `13` (Berekent `x² + y²`)

---

### `let*` gebruiken voor opeenvolgende afhankelijkheden

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Resultaat**: `8` (Berekent `x³`)

---

### Named `let` gebruiken voor recursieve berekeningen

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Resultaat**: `120` (Factoriaal van 5)

---

Door `let`, `let*` te gebruiken en `let` te noemen, maakt Scheme modulaire, recursieve en sequentiële programmering mogelijk met duidelijke scopingregels.