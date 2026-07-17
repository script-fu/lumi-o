---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 32d7e6d0c54bc515f245b0c108d23441754f7248c2510c61a552c693f37d0382
---
In Scheme wählt das bedingte `cond` anhand mehrerer Tests einen von mehreren Codeblöcken — wie ein mehrgliedriges `if`, geprüft in Reihenfolge bis zum ersten Treffer.

### Syntax

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Tests in Schreibreihenfolge.
- Erster `#t`-Test: **consequent** läuft, `cond` stoppt.
- `else` optional als Fallback.

### So funktioniert es

1. **Jede Bedingung testen** in Reihenfolge.
2. **Passenden consequent ausführen**; sonst `else` falls vorhanden.

### Beispiele

#### Beispiel 1: Einzeilige consequents

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

Ergebnis: **"This will run"**

#### Beispiel 2: Mehrere Aktionen mit `begin`

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

Ergebnis: **Gibt "Condition met" aus und liefert 25.**

#### Beispiel 3: `let` im consequent

```scheme
(cond
  ;; Fall 1: Wenn 0 kleiner als -1 ist
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Fall 2: Wenn 0 größer als -1 ist
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Standardfall: Wenn keine der obigen Bedingungen zutrifft
  (else
    (let ((z 0))
      z)))
```

Ergebnis: **Gibt "Positive condition met" aus und liefert 40.**

#### Beispiel 4: Fallback mit `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

Ergebnis: **"Fallback value"**

### Zusammenfassung

- `cond` für mehrere Bedingungen klar und kompakt.
- consequents einzeln oder mit `begin` gruppiert.
- `let` für lokale Variablen; `else` als Fallback empfohlen.