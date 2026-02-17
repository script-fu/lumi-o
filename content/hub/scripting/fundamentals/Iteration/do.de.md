---
title: "Tun"
type: docs
weight: 5
---
Die Funktion `do` in Scheme ist ein Schleifenmechanismus, der eine Iteration mit Initialisierungs-, Aktualisierungs- und Beendigungsbedingungen ermöglicht. Dies ist besonders nützlich, wenn Sie eine Abfolge von Vorgängen eine bestimmte Anzahl von Malen oder bis eine Bedingung erfüllt ist, ausführen müssen.

Die allgemeine Form von `do` ist:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable**: Die Schleifenvariable(n).
- **Anfangswert**: Der Startwert jeder Schleifenvariablen.
- **Update-Ausdruck**: Der Ausdruck zum Aktualisieren der Schleifenvariablen am Ende jeder Iteration.
- **Beendigungsbedingung**: Die Bedingung zum Stoppen der Schleife.
- **Ergebnisausdruck**: Der Wert, der zurückgegeben werden soll, wenn die Schleife endet.
- **Body**: Der Code, der in jeder Iteration ausgeführt werden soll.

---

### Beispiel: Summiere die Zahlen von 1 bis 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

– Die Schleifenvariable `i` beginnt bei 1 und erhöht sich bei jeder Iteration um 1.
- Die Variable `sum` akkumuliert die Summe von `i`.
– Die Schleife endet, wenn `i > 5` und gibt den Endwert von `sum` zurück.

**Ausgabe**: `15`

---

### Wie es funktioniert

1. **Initialisierung**:
   - Jeder Schleifenvariablen wird ihr Anfangswert zugewiesen.

2. **Beendigungsprüfung**:
   - Zu Beginn jeder Iteration wird die Abbruchbedingung überprüft. Bei „true“ stoppt die Schleife und der Ergebnisausdruck wird ausgewertet.

3. **Iteration**:
   – Wenn die Beendigungsbedingung falsch ist, wird der Hauptteil ausgeführt und die Schleifenvariablen werden mithilfe ihrer jeweiligen Aktualisierungsausdrücke aktualisiert.

---

### Zusammenfassung

– Das `do`-Konstrukt bietet eine flexible Möglichkeit, Schleifen mit mehreren Variablen und komplexen Beendigungsbedingungen zu implementieren.
– Dies ist nützlich für Aufgaben, die Statusaktualisierungen über Iterationen hinweg erfordern.
– Die Beendigungsbedingung bestimmt, wann die Schleife endet und kann ein Endergebnis zurückgeben.

Durch die Verwendung von `do` können Sie iterative Algorithmen in Scheme mit präziser Kontrolle über Initialisierung, Aktualisierungen und Beendigung implementieren. Dies macht `do` zu einer Kombination aus einem **bereichsbezogenen Bindungsmechanismus** (wie `let`) und einer **iterativen Kontrollstruktur**, die es ihm ermöglicht, Schleifen und temporäre Zustände sauber und präzise zu handhaben.