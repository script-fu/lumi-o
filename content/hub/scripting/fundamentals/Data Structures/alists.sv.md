---
title: "Associationslistor (Alister)"
type: docs
weight: 6
---
En **associationslista** (eller **alist**) är en grundläggande datastruktur i Schema som används för att representera samlingar av nyckel-värdepar. Det implementeras som en lista med par, där varje par associerar en nyckel (vanligtvis en symbol) med ett värde. Alister är enkla, flexibla och väl lämpade för små till medelstora datamängder.

### Struktur för en föreningslista

En alist är en lista där varje element är ett **par** (konstruerad med `cons`). Varje par består av:

- **Key**: Det första elementet (vanligtvis en symbol).
- **Värde**: Det andra elementet, som kan vara av vilken datatyp som helst.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Nyckel**: `'name`, `'age`, `'city`
- **Värde**: `"Alice"`, `30`, `"Paris"`
- **Struktur**: En lista med par:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Skapa en Alist

Du kan skapa en lista genom att manuellt konstruera par eller genom att programmera bygga den med `cons`.

#### Använda det enda citatet (`'`)

Det enkla citatet (`'`) är en förkortning för **citat**, vilket hindrar Scheme från att utvärdera uttrycket. Detta gör den idealisk för att skapa statiska listor där alla nycklar och värden är hårdkodade.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Resultat**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Använda bakre citat (`` ` ``) and Comma (`,`)

Operatorn för bakåtcitat (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Detta är användbart för att skapa listor där nycklar eller värden beräknas under körning.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Resultat**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Jämförelseexempel

Statisk lista med `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Dynamisk lista med `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Åtkomst till data i en lista

För att hämta ett värde från en lista kan du använda `assoc`-funktionen, som slår upp ett par med sin nyckel.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Extrahera värdet

När du har hämtat ett par med `assoc`, använd `cdr` för att extrahera värdet:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Sammanfattning av nyckelfunktioner

- **Enstaka citat (`'`)**: Skapar en statisk lista där alla element är bokstavliga data.
- **Tillbakscitat (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Punktnotation (`.`)**: Används för att konstruera par, associera en nyckel med ett värde i en alist.