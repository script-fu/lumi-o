---
title: "do"
type: docs
weight: 5
---
`do`-funktionen i Scheme är en loop-mekanism som tillåter iteration med initialiserings-, uppdaterings- och avslutningsvillkor. Det är särskilt användbart när du behöver utföra en sekvens av operationer ett visst antal gånger eller tills ett villkor är uppfyllt.

Den allmänna formen av `do` är:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabel**: Slingvariabeln(erna).
- **Initial-value**: Startvärdet för varje loopvariabel.
- **Update-expression**: Uttrycket för att uppdatera loopvariablerna i slutet av varje iteration.
- **Termination-condition**: Villkoret för att stoppa slingan.
- **Resultatuttryck**: Värdet som ska returneras när slingan avslutas.
- **Body**: Koden som ska köras i varje iteration.

---

### Exempel: Summa talen från 1 till 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- Slingvariabeln `i` börjar med 1 och ökar med 1 i varje iteration.
- Variabeln `sum` ackumulerar summan av `i`.
- Slingan avslutas när `i > 5`, returnerar det slutliga värdet av `sum`.

**Utdata**: `15`

---

### Hur det fungerar

1. **Initiering**:
   - Varje loopvariabel tilldelas sitt initiala värde.

2. **Uppsägningskontroll**:
   - I början av varje iteration kontrolleras avslutningsvillkoret. Om sant, slutar loopen och resultatuttrycket utvärderas.

3. **Iteration**:
   - Om avslutningsvillkoret är falskt exekveras texten och loopvariablerna uppdateras med sina respektive uppdateringsuttryck.

---

### Sammanfattning

- `do`-konstruktionen ger ett flexibelt sätt att implementera loopar med flera variabler och komplexa avslutningsvillkor.
- Det är användbart för uppgifter som kräver tillståndsuppdateringar över iterationer.
– Avslutningsvillkoret avgör när slingan slutar och kan returnera ett slutresultat.

Genom att använda `do` kan du implementera iterativa algoritmer i Scheme med exakt kontroll över initiering, uppdateringar och avslutning. Detta gör `do` till en kombination av en **omfattad bindningsmekanism** (som `let`) och en **iterativ kontrollstruktur**, vilket gör att den kan hantera looping och temporärt tillstånd på ett rent, kortfattat sätt.