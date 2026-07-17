---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
---
Funktionen `do` i Scheme är en loopmekanism som tillåter iteration med initiering, uppdatering och avslutningsvillkor. Den är särskilt användbar när en sekvens av operationer ska köras ett visst antal gånger eller tills ett villkor uppfylls.

Den allmänna formen av `do` är:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabel:** loopvariabel(n).
- **Initialvärde:** startvärdet för varje loopvariabel.
- **Uppdateringsuttryck:** uttrycket som uppdaterar loopvariabeln vid slutet av varje iteration.
- **Avslutningsvillkor:** villkoret som stoppar loopen.
- **Resultatuttryck:** värdet som returneras när loopen avslutas.
- **Kropp:** koden som körs vid varje iteration.

---

### Exempel: summera talen från 1 till 5

```scheme
(do ((i 1 (+ i 1))      ; Initiera i till 1, öka med 1
     (sum 0 (+ sum i))) ; Initiera sum till 0, lägg till i i sum
    ((> i 5) sum)       ; Avsluta när i > 5, returnera sum
  (lumi-message (number->string sum))) ; Skriv ut sum vid varje steg
```

- Loopvariabeln `i` börjar på 1 och ökas med 1 vid varje iteration.
- Variabeln `sum` ackumulerar summan av `i`.
- Loopen avslutas när `i > 5` och returnerar slutvärdet av `sum`.

**Utdata**: `15`

---

### Så fungerar det

1. **Initiering:**
   - Varje loopvariabel tilldelas sitt startvärde.

2. **Kontroll av avslutning:**
   - I början av varje iteration kontrolleras avslutningsvillkoret. Om det är sant stoppas loopen och resultatuttrycket utvärderas.

3. **Iteration:**
   - Om avslutningsvillkoret är falskt körs kroppen och loopvariablerna uppdateras med respektive uppdateringsuttryck.

---

### Sammanfattning

- Konstruktionen `do` ger ett flexibelt sätt att implementera loopar med flera variabler och komplexa avslutningsvillkor.
- Den är användbar för uppgifter som kräver tillståndsuppdateringar mellan iterationer.
- Avslutningsvillkoret avgör när loopen slutar och kan returnera ett slutresultat.

Med `do` kan du implementera iterativa algoritmer i Scheme med exakt kontroll över initiering, uppdateringar och avslutning. Det gör `do` till en kombination av en **omfattningsbunden bindningsmekanism** (som `let`) och en **iterativ kontrollstruktur**, vilket gör det möjligt att hantera loopar och tillfälligt tillstånd på ett rent och koncist sätt.
