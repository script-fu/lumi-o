---
title: "Listor"
type: docs
weight: 4
---
I Schema är en **lista** en grundläggande datastruktur som används för att gruppera värden. Listor är ordnade samlingar av element där varje element kan vara av vilken typ som helst, inklusive en annan lista. Listor används ofta i Scheme för både datalagring och programstruktur.

### Exempel 1: Enkel lista

```scheme
(list 1 2 3)
```

- Skapar en lista med tre element: `1`, `2` och `3`.

Resultat: **`(1 2 3)`**

---

#### Åtkomst till listelement

Element i en lista nås med hjälp av procedurerna `car` och `cdr`:

- `car` hämtar det första elementet i en lista.
- `cdr` hämtar resten av listan (allt utom det första elementet).

#### Exempel

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Resultat:

- `(car my-list)` returnerar `1`
- `(cdr my-list)` returnerar `(2 3)`

---

#### Enkel rekursion: Iteration genom en lista

Genom att rekursivt anropa `car` på `cdr` i en lista, kan du bearbeta varje element ett efter ett tills listan har passerats. Detta utgör grunden för många listbearbetningsalgoritmer.

#### Exempel: Skriva ut varje element i en lista

Här är en enkel rekursiv funktion för att skriva ut varje element i en lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Grundfall:** Om listan är tom (`null? lst`), stoppa rekursion.
- **Rekursiv skiftläge:** Skriv ut det första elementet (`car lst`), anropa sedan funktionen på resten av listan (`cdr lst`).

#### Exempel på användning

```scheme
(print-elements (list 1 2 3))
```

Utdata:

- `"1"`
- `"2"`
- `"3"`

Resultat: "klar"

---

#### Hur det fungerar

1. Funktionen hämtar det första elementet i listan med `car` och bearbetar det.
2. Den anropar sig sedan med resten av listan (`cdr`).
3. Denna process upprepas tills listan är tom (`null? lst`).

---

### Exempel 2: Blandade typer

Listor kan innehålla element av olika typer, inklusive strängar, booleaner, siffror, andra listor eller till och med resultatet av uttryck:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Detta skapar en lista med:
  - Ett nummer (`42`)
  - En sträng (`"hello"`)
  - En boolesk (`#t`)
  - En annan lista (`(1 2)`)
  - Resultatet av ett uttryck (`(+ 3 4)`, som utvärderas till `7`)

Resultat: **`(42 "hello" #t (1 2) 7)`**

---

Dessa exempel visar mångsidigheten hos listor i Scheme, vilket gör dem till ett kraftfullt verktyg för att organisera och manipulera data.

### Konstruera listor

`cons`-proceduren används för att konstruera en ny lista genom att kombinera ett element med en befintlig lista.

```scheme
(cons new-element existing-list)
```

#### Exempel

```scheme
(cons 0 (list 1 2 3))
```

- Lägger till `0` i början av listan `(1 2 3)`.

Resultat: **`(0 1 2 3)`**

---

### Söker efter listor

`list?` proceduren kontrollerar om ett givet värde är en lista.

```scheme
(list? value)
```

#### Exempel: lista?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Resultat:

- `(list? (list 1 2 3))` returnerar `#t` (sant)
- `(list? 42)` returnerar `#f` (falskt)

---

### Operationer på listor

Schema tillhandahåller flera inbyggda procedurer för att arbeta med listor, inklusive:

- `length`: Returnerar antalet element i en lista.
- `append`: Kombinerar två eller flera listor till en.
- `reverse`: Returnerar en ny lista med element i omvänd ordning.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Resultat:

- `(length (list 1 2 3))` returnerar `3`
- `(append (list 1 2) (list 3 4))` returnerar `(1 2 3 4)`
- `(reverse (list 1 2 3))` returnerar `(3 2 1)`#### Använda `list-ref`

`list-ref`-proceduren hämtar elementet vid ett specificerat index i en lista (nollbaserat index).

```scheme
(list-ref lst index)
```

- **`lst`**: Listan från vilken elementet ska hämtas.
- **`index`**: Ett nollbaserat index som anger vilket element som ska returneras.

##### Exempel: list-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Resultat: `30`

---

### Kapslade listor

Listor i Schema kan innehålla andra listor som element, vilket skapar en kapslad struktur.

#### Exempel: Skapa en kapslad lista

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Skapar en lista med tre element, som var och en är en lista.

Resultat: **`((1 2) (3 4) (5))`**

---

#### Åtkomst till kapslade data

För att komma åt element i en kapslad lista kan du använda kombinationer av `car` och `cdr` för att navigera genom strukturen.

#### Exempel: Åtkomst till element

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Förklaring

1. **`car nested-list`**:
   - Hämtar det första elementet i `nested-list`, vilket är `(1 2)`.

2. **`car (car nested-list)`**:
   - Hämtar det första elementet i `(1 2)`, vilket är `1`.

3. **`cdr (car nested-list)`**:
   - Hämtar resten av `(1 2)`, vilket är `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Hämtar det första elementet i `(2)`, vilket är `2`.

---

#### Exempel: Åtkomst till element från andra underlistor

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Detta tillvägagångssätt låter dig systematiskt navigera och komma åt specifika element i en kapslad lista, vilket ger kraftfull flexibilitet för att arbeta med hierarkiska data.

### Sammanfattning

- **Listor** i Scheme är mångsidiga och viktiga datastrukturer.
- Använd `list` för att skapa en lista, `car` och `cdr` för att komma åt element och `cons` för att konstruera listor.
- Inbyggda procedurer som `length`, `append`, `reverse` och `list-ref` gör listoperationer enkel och effektiv.
- Listor kan kapslas, vilket möjliggör komplexa datastrukturer för avancerade användningsfall.