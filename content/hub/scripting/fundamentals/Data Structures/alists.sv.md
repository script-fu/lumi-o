---
title: "Associationslistor (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
---
En **associationslista** (eller **alist**) är en grundläggande datastruktur i Scheme som används för att representera samlingar av nyckel-värdepar. Det implementeras som en lista med par, där varje par associerar en nyckel (vanligtvis en symbol) med ett värde. Alister är enkla, flexibla och väl lämpade för små till medelstora datamängder.

### Struktur för en föreningslista

En alist är en lista där varje element är ett **par** (konstruerad med `cons`). Varje par består av:

- **Nyckel**: Det första elementet (vanligtvis en symbol).
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
;; Definiera en alist manuellt
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Lägg till ett nytt par programmatiskt
(define updated-alist (cons '(country . "France") alist))
```

**Resultat**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Använda bakre citat (`` ` ``) och komma (`,`)

Operatorn för bakåtcitat (`` ` ``) liknar enkla citattecken, men tillåter dynamisk infogning av utvärderade uttryck med kommatecken (`,`). Detta är användbart för att skapa listor där nycklar eller värden beräknas under körning.

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

Dynamisk lista med `` ` `` och `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Åtkomst till data i en lista

För att hämta ett värde från en lista kan du använda `assoc`-funktionen, som slår upp ett par med sin nyckel.

```scheme
(assoc 'name alist)   ; Returnerar (name . "Alice")
(assoc 'country alist) ; Returnerar #f (nyckeln hittades inte)
```

### Extrahera värdet

När du har hämtat ett par med `assoc`, använd `cdr` för att extrahera värdet:

```scheme
(cdr (assoc 'name alist))   ; Returnerar "Alice"
```

### Sammanfattning av nyckelfunktioner

- **Enstaka citat (`'`)**: Skapar en statisk lista där alla element är bokstavliga data.
- **Backcitat (`` ` ``)**: Möjliggör dynamisk skapande av alists genom att blanda statiska element med utvärderade uttryck (med `,`).
- **Punktnotation (`.`)**: Används för att konstruera par, associera en nyckel med ett värde i en alist.