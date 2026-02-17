---
title: "Datenstrukturen"
type: docs
weight: 3
---
In Scheme sind **Datenstrukturen** wesentliche Werkzeuge zum Organisieren, Speichern und Bearbeiten von Daten. Sie ermöglichen Entwicklern die Erstellung effizienter, lesbarer und wiederverwendbarer Skripte. Durch die Auswahl der richtigen Datenstruktur für ein bestimmtes Problem können Sie sowohl die Leistung als auch die Klarheit Ihres Codes optimieren.

## Wichtige Datenstrukturen im Schema

Scheme bietet mehrere leistungsstarke und vielseitige Datenstrukturen, die jeweils für bestimmte Aufgaben geeignet sind. Zu den primären Datenstrukturen gehören:

### Listen
Listen sind geordnete Sammlungen von Elementen, die dynamisch wachsen oder schrumpfen können. Sie eignen sich ideal für sequentielle oder hierarchische Daten und werden häufig in der funktionalen Programmierung verwendet.

Hauptmerkmale:
- Dynamische Größe.
- Elemente können gemischter Art sein.
– Wird häufig für rekursive Algorithmen und zur Darstellung baumartiger Strukturen verwendet.

Anwendungsbeispiele:
- Verwalten von Artikelsammlungen.
- Darstellung von Abfolgen oder Hierarchien.

---

### Vektoren
Vektoren sind Sammlungen von Elementen fester Größe, die für einen schnellen Zugriff indiziert sind. Sie eignen sich am besten für Szenarien, in denen Leistung und Positionszugriff von entscheidender Bedeutung sind.

Hauptmerkmale:
- Feste Größe bei der Erstellung.
- Auf Elemente wird über ihren Index zugegriffen.
- Schneller als Listen für bestimmte Vorgänge wie Direktzugriff.

Anwendungsbeispiele:
- Speichern von Konfigurationen oder Daten fester Größe.
- Schnelle Suche und Aktualisierung basierend auf der Position.

---

### Auswahl der richtigen Datenstruktur

Die Entscheidung, eine **Liste** oder einen **Vektor** zu verwenden, hängt von den spezifischen Anforderungen Ihres Skripts ab. Hier sind einige Richtlinien:

| Funktion | Listen | Vektoren |
|-----------|---------------|--------------------------------|
| **Größenflexibilität** | Dynamisch | Behoben |
| **Zugriffsgeschwindigkeit** | Langsamer (sequentieller Zugriff) | Schneller (indizierter Zugriff) |
| **Einfache Modifikation**| Einfacher | Härter (erfordert Neuzuweisung)|
| **Anwendungsfälle** | Dynamische Daten, Rekursion | Statische Daten, schnelle Suche |

---