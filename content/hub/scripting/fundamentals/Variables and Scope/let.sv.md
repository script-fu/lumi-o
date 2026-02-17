---
title: "låta"
type: docs
weight: 4
---
Namnet `let` används eftersom det återspeglar dess matematiska ursprung för att introducera temporära bindningar, som i _"Låt \( x = 2 \) och \( y = 3 \)"_.

En `let`-sats i Scheme är en **bindande konstruktion** som används för att definiera variabler inom ett lokaliserat omfång. Det låter dig skapa tillfälliga bindningar för variabler och sedan exekvera ett kodblock med dessa bindningar. Detta är särskilt användbart för att hålla koden modulär och undvika globala föroreningar.

Det finns tre huvudformer av `let` i Scheme:

- **`let`**: Standard för att skapa enkla lokala bindningar.
- **`let*`**: Sekventiell uthyrning, där bindningar kan bero på resultatet av tidigare bindningar.
- ** Namngiven `let`**: En speciell form av `let` som skapar rekursiva loopar eller namngivna procedurer.

I sin enklaste form skapar `let` lokala variabla bindningar och utvärderar ett uttryck med dessa bindningar.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Bindningar**: En lista med par där varje par tilldelar ett `value` till ett `variable`.
- **Uttryck**: Brödtexten i `let`, som kan använda de lokalt definierade variablerna.

### Exempel

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Detta definierar två lokala variabler, `x` (10) och `y` (20).
- Den beräknar sedan `(+ x y)` med hjälp av dessa variabler.

**Resultat**: `30`

---

## `let*` Konstruktionen

`let*`-konstruktionen liknar `let`, men bindningar utvärderas **sekventiellt**. Detta innebär att senare bindningar kan bero på tidigare.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Exempel

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- Den första bindningen tilldelar `10` till `x`.
- Den andra bindningen beräknar `y` som `(+ x 5)`, med värdet av `x`.
- Kroppen beräknar `(* x y)`.

**Resultat**: `150`

---

## Namngiven `let`

Ett namngivet `let` är en speciell form av `let` som ger ett namn för själva `let` blocket, vilket gör det till en rekursiv procedur. Detta är användbart för att skapa loopar eller rekursiva beräkningar.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Namn**: `let`-blocket får ett namn, vilket effektivt definierar en funktion.
- **Bindningar**: Initiala värden för variabler, liknande en standard `let`.
- **Body**: Uttrycket kan anropa den namngivna `let` rekursivt.

### Exempel: Looping med Named `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- Funktionen `loop` börjar med `n = 5` och `result = 1`.
- Om `n` är `0`, returnerar det `result`.
- Annars kallar den sig rekursivt med `n - 1` och `result * n`.

**Resultat**: `120` (Faktor av 5)

---

## Sammanfattningstabell| Konstruera | Beskrivning | Användningsfall |
|------------|--------------------------------------------------------------------------------------------------------------------------------|
| **`let`** | Definierar lokala bindningar för variabler.    | Använd när alla bindningar är oberoende och inte litar på varandra.     |
| **`let*`** | Definierar sekventiella lokala bindningar.       | Använd när senare bindningar beror på resultatet av tidigare.           |
| ** Namngiven `let`** | Definierar rekursiva lokala procedurer. | Använd för loopar, iterativa beräkningar eller rekursion i ett lokalt sammanhang. |

---

## Exempel

### Använda `let` för lokal beräkning

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Resultat**: `13` (beräknar `x² + y²`)

---

### Använda `let*` för sekventiella beroenden

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Resultat**: `8` (beräknar `x³`)

---

### Använder namngiven `let` för rekursiv beräkning

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Resultat**: `120` (Faktor av 5)

---

Genom att använda `let`, `let*`, och namngiven `let`, möjliggör Scheme modulär, rekursiv och sekventiell programmering med tydliga omfattningsregler.