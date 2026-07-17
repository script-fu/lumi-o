---
title: "Symbole"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4ae0cc2f5749cbe997d6fa25315ee3fe54646eb065b4dba0114778c75a889ae5
---
Symbole sind einer der Kerndatentypen in Scheme und stellen eindeutige, unveränderliche Bezeichner dar. Sie werden hauptsächlich als Schlüssel, Markierungen oder Platzhalter in Programmen verwendet und sind daher für das Schreiben von sauberem und ausdrucksstarkem Code unerlässlich.

Ein Symbol in Scheme ähnelt einer Zeichenfolge, unterscheidet sich jedoch dadurch, dass Symbole **einzigartig** und **atomar** sind. Dies bedeutet, dass es sich bei zwei Symbolen mit demselben Namen garantiert um dasselbe Objekt handelt, was eine schnelle Gleichheitsprüfung und eine effiziente Verwendung in Datenstrukturen ermöglicht.

### Syntax

Ein Symbol wird als eine Folge von Zeichen geschrieben:

– Beginnt mit einem Buchstaben, gefolgt von Buchstaben, Ziffern oder Sonderzeichen wie `-`, `+` oder `*`.
- Bei Symbolen wird standardmäßig die Groß-/Kleinschreibung beachtet.

Beispiele:

```scheme
'hello       ; Ein Symbol namens `hello`
'foo-bar     ; Ein Symbol namens `foo-bar`
'*special*   ; Ein Symbol namens `*special*`
```

## Symbole erstellen

Symbole werden normalerweise mit dem **Anführungszeichen**-Operator (`'`) erstellt, der Scheme anweist, den Namen als Symbol zu behandeln, anstatt ihn als Variable oder Funktion auszuwerten.

### Beispiel

```scheme
'my-symbol   ; Erstellt das Symbol `my-symbol`
```

Sie können Symbole auch programmgesteuert mit der Prozedur `string->symbol` erstellen, die eine Zeichenfolge in ein Symbol konvertiert.

```scheme
(string->symbol "dynamic-symbol")
```

**Ergebnis**: `'dynamic-symbol`


## Vergleich von Symbolen

Da Symbole einzigartig sind, können Sie sie mithilfe von `eq?` effizient vergleichen.

### Beispiel

```scheme
(eq? 'apple 'apple)   ; #t (gleiches Symbol)
(eq? 'apple 'orange)  ; #f (verschiedene Symbole)
```

Dadurch eignen sich Symbole ideal für die Verwendung als Schlüssel in Datenstrukturen oder Markierungen in Ihrem Code.

## Verwendung von Symbolen

Symbole werden in Scheme häufig verwendet für:

1. **Schlüssel in Assoziationslisten:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Gibt (name . "Alice") zurück
```

2. **Bezeichner im Code:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Verfahren zum Arbeiten mit Symbolen

Scheme bietet integrierte Verfahren zum Arbeiten mit Symbolen:

| Vorgehensweise | Beschreibung |
|------|----------------------------------|
| **`symbol?`** | Überprüft, ob ein Objekt ein Symbol ist.                                            |
| **`eq?`** | Vergleicht zwei Symbole auf Identität (schneller Vergleich).                       |
| **`symbol->string`** | Konvertiert ein Symbol in eine Zeichenfolge (nützlich zur Anzeige oder zum Debuggen).          |
| **`string->symbol`** | Konvertiert eine Zeichenfolge in ein Symbol (nützlich für die dynamische Erstellung von Bezeichnern). |

### Beispiele

```scheme
(symbol? 'example)            ; #t (wahr: es ist ein Symbol)
(symbol->string 'example)     ; Ergebnis: "example"
(string->symbol "new-symbol") ; Ergebnis: 'new-symbol
```

## Zusammenfassung

Symbole sind eine einfache und effiziente Möglichkeit, Bezeichner, Schlüssel und Markierungen in Scheme darzustellen. Ihre Unveränderlichkeit und schnelle Identitätsprüfung machen sie ideal für viele Programmieraufgaben. Wenn Sie verstehen, wie Sie Symbole effektiv nutzen, können Sie sauberen und ausdrucksstarken Scheme-Code schreiben.