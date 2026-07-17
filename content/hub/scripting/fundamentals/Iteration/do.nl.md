---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
---
De functie `do` in Scheme is een lusmechanisme dat iteratie mogelijk maakt met initialisatie, bijwerking en beëindigingsvoorwaarden. Het is vooral nuttig wanneer je een reeks bewerkingen een bepaald aantal keren moet uitvoeren of totdat aan een voorwaarde is voldaan.

De algemene vorm van `do` is:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabele:** De lusvariabele(n).
- **Startwaarde:** De startwaarde van elke lusvariabele.
- **Update-expressie:** De expressie die de lusvariabele(n) aan het einde van elke iteratie bijwerkt.
- **Stopvoorwaarde:** De voorwaarde om de lus te stoppen.
- **Resultaatexpressie:** De waarde die wordt geretourneerd wanneer de lus eindigt.
- **Body:** De code die bij elke iteratie wordt uitgevoerd.

---

### Voorbeeld: som van de getallen 1 tot 5

```scheme
(do ((i 1 (+ i 1))      ; Initialiseer i op 1, verhoog met 1
     (sum 0 (+ sum i))) ; Initialiseer som op 0, tel i op bij som
    ((> i 5) sum)       ; Beëindigen wanneer i > 5, retourneer sum
  (lumi-message (number->string sum))) ; Print de som bij elke stap
```

- De lusvariabele `i` start op 1 en neemt bij elke iteratie met 1 toe.
- De variabele `sum` accumuleert de som van `i`.
- De lus eindigt wanneer `i > 5`, met als retourwaarde de uiteindelijke waarde van `sum`.

**Output**: `15`

---

### Hoe het werkt

1. **Initialisatie:**
   - Elke lusvariabele krijgt zijn startwaarde toegewezen.

2. **Beëindigingscontrole:**
   - Aan het begin van elke iteratie wordt de beëindigingsvoorwaarde gecontroleerd. Als die waar is, stopt de lus en wordt de resultatexpressie geëvalueerd.

3. **Iteratie:**
   - Als de beëindigingsvoorwaarde onwaar is, wordt de body uitgevoerd en worden de lusvariabelen bijgewerkt met hun respectievelijke update-expressies.

---

### Samenvatting

- Het `do`-construct biedt een flexibele manier om lussen te implementeren met meerdere variabelen en complexe beëindigingsvoorwaarden.
- Het is nuttig voor taken die statusupdates over iteraties heen vereisen.
- De beëindigingsvoorwaarde bepaalt wanneer de lus eindigt en kan een eindresultaat retourneren.

Met `do` kun je iteratieve algoritmen in Scheme implementeren met precieze controle over initialisatie, bijwerkingen en beëindiging. `do` combineert een **scope-gebonden bindingsmechanisme** (zoals `let`) met een **iteratieve controlestructuur**, waardoor lussen en tijdelijke status op een nette, beknopte manier afgehandeld kunnen worden.
