---
title: "Bedingungen"
type: docs
weight: 2
---
Bedingungen sind ein grundlegender Bestandteil der Programmierung und ermöglichen es Skripten, Entscheidungen zu treffen und ihren Ablauf auf der Grundlage spezifischer Kriterien zu steuern. In Scheme, das auf der Programmiersprache Scheme basiert, ermöglichen Ihnen Bedingungen die Erstellung dynamischer und intelligenter Skripte, die sich an sich ändernde Eingaben, Umgebungen oder Benutzeraktionen anpassen.

### Die Rolle von Bedingungen im Schema

Bedingungen dienen in Ihren Skripten mehreren wichtigen Zwecken:
- **Steuerungslogik:** Sie ermöglichen die Ausführung verschiedener Codeteile, je nachdem, ob bestimmte Bedingungen wahr oder falsch sind.
- **Verbesserung der Flexibilität:** Durch die dynamische Reaktion auf Eingaben oder Zustände helfen Bedingungen Ihrem Skript, eine Vielzahl von Szenarien zu bewältigen.
- **Vereinfachung der Komplexität:** Sie unterteilen die Entscheidungsfindung in überschaubare Strukturen und machen so das Lesen, Debuggen und Warten des Codes einfacher.

### Verfügbare Arten von Bedingungen

Scheme bietet mehrere bedingte Konstrukte, die jeweils für unterschiedliche logische Anforderungen geeignet sind:
- **`if`:** Zum Treffen einfacher binärer Entscheidungen, indem ein Codeblock ausgeführt wird, wenn eine Bedingung wahr ist, und ein anderer, wenn sie falsch ist.
- **`cond`:** Ein leistungsstarkes Mehrfachverzweigungskonstrukt für die klare, strukturierte Handhabung mehrerer Bedingungen.
- **`and` / `or`:** Logische Operatoren, die Kombinationen von Bedingungen auswerten und so eine komplexere Entscheidungsfindung ermöglichen.
- **`else`:** Ein Catch-All, das das Fallback-Verhalten definiert, wenn keine der angegebenen Bedingungen erfüllt ist.

### Wie Bedingungen funktionieren

Konditionale beinhalten typischerweise:
1. **Auswerten einer Bedingung:** Ein Testausdruck bestimmt, ob eine Bedingung wahr oder falsch ist.
2. **Verzweigungsausführung:** Basierend auf der Auswertung wählt das Skript aus, welcher Codeblock ausgeführt werden soll.
3. **Rückgabe eines Werts (optional):** In einigen Fällen können Bedingungen auch einen Wert erzeugen, den andere Teile des Skripts verwenden können.