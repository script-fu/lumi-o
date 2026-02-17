---
title: "Variablen und Umfang"
type: docs
weight: 1
---
In Scheme ist die Verwaltung von Variablen und ihrem Umfang ein Kernkonzept zum Schreiben effizienter und wartbarer Skripte. Variablen speichern Datenwerte, die Ihr Skript bearbeiten kann, während der Bereich definiert, wo auf diese Variablen zugegriffen werden kann. Wenn Sie verstehen, wie Variablen effektiv definiert und verwendet werden, können Sie strukturierten, wiederverwendbaren und fehlerfreien Code erstellen.

### Dynamisches Tippen

Das Schema ist dynamisch typisiert: Sie deklarieren keine Typen im Voraus und eine Variable kann im Laufe der Zeit Werte unterschiedlicher Art enthalten.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### Die Rolle von Variablendefinitionen und Umfang im Schema

Das Definieren von Variablen und das Verwalten ihres Gültigkeitsbereichs dient mehreren Zwecken:
- **Daten organisieren:** Variablen speichern Informationen und machen Ihre Skripte dadurch besser lesbar und verwaltbar.
- **Verbesserung der Wiederverwendbarkeit:** Durch die Verwendung von Bereichsvariablen können Sie Codeabschnitte ohne Konflikte wiederverwenden.
- **Kapselung:** Der lokalisierte Bereich verhindert unbeabsichtigte Interaktionen zwischen Variablen in verschiedenen Teilen Ihres Skripts.
- **Vereinfachung der Logik:** Temporäre Variablen in einem begrenzten Umfang reduzieren die Komplexität bei größeren Berechnungen oder Arbeitsabläufen.

### Arten von Variablendefinitionen und Umfang

Scheme bietet mehrere Konstrukte zum Definieren und Festlegen von Variablen:
- **`let`:** Erstellt lokale Bindungen für Variablen innerhalb eines bestimmten Codeblocks.
- **`let*`:** Eine sequentielle Version von `let`, bei der jede Bindung von den vorherigen abhängen kann.
- **Mit Namen `let`:** Ein leistungsstarkes Konstrukt zum Definieren rekursiver lokaler Prozeduren oder Schleifen.
- **`define`:** Erstellt globale Variablen oder Funktionen, auf die in Ihrem gesamten Skript zugegriffen werden kann.

### Wie Variablendefinitionen und Umfang funktionieren

Variablendefinitionen und -umfang umfassen typischerweise Folgendes:
1. **Variablen deklarieren:** Einer Variablen in einem bestimmten Kontext einen Wert zuweisen.
2. **Einschränkender Geltungsbereich:** Steuern, wo auf die Variable zugegriffen werden kann (z. B. innerhalb eines `let`-Blocks oder global).
3. **Variablen verwenden:** Auf Variablenwerte zugreifen und diese ändern, um Berechnungen, Logik oder prozedurale Operationen durchzuführen.

### Beispiel: Verwendung von `let` für lokale Variablen

Mit dem `let`-Konstrukt können Sie temporäre Variablen definieren, die nur innerhalb eines bestimmten Blocks verfügbar sind:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

– Dieses Beispiel deklariert `x` und `y` mit lokalen Werten und berechnet deren Summe.

### Beispiel: Verwendung von `define` für globale Variablen

Das `define`-Konstrukt erstellt Variablen oder Funktionen mit globalem Gültigkeitsbereich:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

– Dieses Skript definiert eine globale Konstante `pi` und eine Funktion `circle-area`, die sie verwendet.

### Scope-Vergleich: Lokal vs. Global

| Funktion | Lokaler Geltungsbereich (`let`, `let*`) | Globaler Geltungsbereich (`define`) |
|------------------|-------------|----------------------------------------------|
| **Barrierefreiheit** | Beschränkt auf den Block, in dem es definiert ist | Im gesamten Skript zugänglich |
| **Kapselung** | Verhindert unbeabsichtigte Interaktionen | Kann mit anderen global definierten Variablen in Konflikt geraten |
| **Anwendungsfall** | Temporäre Variablen für bestimmte Aufgaben | Gemeinsam genutzte Variablen oder Funktionen, die überall verwendet werden |

### Zusammenfassung- **Variablendefinitionen und -bereich** sind die Grundlage für die Organisation und Verwaltung von Daten in Ihren Scheme-Skripten.
- Verwenden Sie den **lokalen Bereich** (`let`, `let*`, mit dem Namen `let`), um temporäre Variablen zu kapseln und Konflikte zu vermeiden.
- Verwenden Sie den **globalen Bereich** (`define`) für wiederverwendbare Funktionen oder Konstanten, die in Ihrem Skript gemeinsam genutzt werden.
- Ein klares Verständnis dieser Konstrukte verbessert die Lesbarkeit, Wartbarkeit und Zuverlässigkeit Ihres Codes.