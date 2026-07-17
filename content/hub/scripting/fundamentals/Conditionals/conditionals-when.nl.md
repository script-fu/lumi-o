---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
In Scheme is `if` veelzijdig, maar zonder expliciete `else`-tak wordt het snel verwarrend — vooral wanneer alleen de ware tak moet worden uitgevoerd en er geen alternatief is voor het false-geval. In zo'n situatie is `when` duidelijker en compacter.

De basisvorm van `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Als `test` waar is (`#t`), worden alle expressies in de body van `when` achtereenvolgens uitgevoerd.
- Als `test` onwaar is (`#f`), gebeurt er niets en wordt geen waarde teruggegeven.

### Voorbeeld

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### `if` en `when` vergeleken

Beide in hetzelfde voorbeeld:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing additional actions within 'when'.")))
```

#### Uitleg

1. **`if`-voorwaarde:**
   - De test `(= 0 1)` controleert of 0 gelijk is aan 1.
   - Omdat dit onwaar is (`#f`), wordt de `else`-tak uitgevoerd.

2. **`when` in de `else`-tak:**
   - De test `(< 0 1)` controleert of 0 kleiner is dan 1.
   - Omdat dit waar is (`#t`), worden alle expressies in de body van `when` achtereenvolgens uitgevoerd.

#### Waarom `when`?

- Geen lege of dummy-`else` nodig.
- Maakt duidelijk dat alleen de ware tak relevant is.

### Samenvatting

- Gebruik `if` wanneer zowel de ware als de onware tak nodig is.
- Gebruik `when` wanneer er alleen een ware tak is, vooral bij meerdere acties.
- `if` en `when` combineren helpt complexe voorwaarden overzichtelijk te structureren.
