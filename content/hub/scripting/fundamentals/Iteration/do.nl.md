---
title: "Doen"
type: docs
weight: 5
---
De functie `do` in Scheme is een lusmechanisme dat iteratie met initialisatie-, update- en beëindigingsvoorwaarden mogelijk maakt. Dit is met name handig wanneer u een reeks bewerkingen een bepaald aantal keren moet uitvoeren of totdat aan een voorwaarde is voldaan.

De algemene vorm van `do` is:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabele**: de lusvariabele(n).
- **Beginwaarde**: de startwaarde van elke lusvariabele.
- **Update-expressie**: de expressie om de lusvariabele(n) aan het einde van elke iteratie bij te werken.
- **Beëindigingsvoorwaarde**: de voorwaarde om de lus te stoppen.
- **Resultaatexpressie**: de waarde die moet worden geretourneerd wanneer de lus eindigt.
- **Body**: de code die in elke iteratie moet worden uitgevoerd.

---

### Voorbeeld: Tel de getallen van 1 tot en met 5 bij elkaar op

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- De lusvariabele `i` begint bij 1 en wordt bij elke iteratie met 1 verhoogd.
- De variabele `sum` telt de som op van `i`.
- De lus eindigt wanneer `i > 5` en retourneert de uiteindelijke waarde `sum`.

**Uitvoer**: `15`

---

### Hoe het werkt

1. **Initialisatie**:
   - Elke lusvariabele krijgt zijn initiële waarde toegewezen.

2. **Beëindigingscontrole**:
   - Aan het begin van elke iteratie wordt de beëindigingsvoorwaarde gecontroleerd. Indien waar, stopt de lus en wordt de resultaatexpressie geëvalueerd.

3. **Iteratie**:
   - Als de beëindigingsvoorwaarde onwaar is, wordt de hoofdtekst uitgevoerd en worden de lusvariabelen bijgewerkt met behulp van hun respectievelijke update-expressies.

---

### Samenvatting

- De `do` constructie biedt een flexibele manier om lussen met meerdere variabelen en complexe beëindigingsvoorwaarden te implementeren.
- Het is handig voor taken waarvoor statusupdates in verschillende iteraties nodig zijn.
- De beëindigingsvoorwaarde bepaalt wanneer de lus eindigt en kan een eindresultaat opleveren.

Door `do` te gebruiken, kunt u iteratieve algoritmen in Scheme implementeren met nauwkeurige controle over initialisatie, updates en beëindiging. Dit maakt `do` een combinatie van een **scoped bindingsmechanisme** (zoals `let`) en een **iteratieve controlestructuur**, waardoor het in staat is om looping en tijdelijke status op een schone, beknopte manier af te handelen.