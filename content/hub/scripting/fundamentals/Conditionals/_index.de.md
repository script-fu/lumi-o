---
title: "Bedingungen"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals/_index"
---
Bedingungen sind ein grundlegender Bestandteil der Programmierung: Sie ermöglichen Skripten, Entscheidungen zu treffen und ihren Ablauf anhand bestimmter Kriterien zu steuern. In Scheme, basierend auf der Programmiersprache Scheme, helfen Ihnen Bedingungen dabei, dynamische und intelligente Skripte zu schreiben, die sich an veränderte Eingaben, Umgebungen oder Benutzeraktionen anpassen.

### Die Rolle von Bedingungen in Scheme

Bedingungen erfüllen in Ihren Skripten mehrere zentrale Aufgaben:
- **Steuerung der Logik:** Sie führen je nachdem unterschiedliche Codeteile aus, ob bestimmte Bedingungen wahr oder falsch sind.
- **Mehr Flexibilität:** Durch dynamische Reaktion auf Eingaben oder Zustände bewältigen Ihre Skripte eine Vielzahl von Szenarien.
- **Komplexität reduzieren:** Sie zerlegen Entscheidungen in überschaubare Strukturen und machen Code leichter lesbar, debugbar und wartbar.

### Verfügbare bedingte Konstrukte

Scheme bietet mehrere bedingte Konstrukte, die jeweils für unterschiedliche logische Anforderungen geeignet sind:
- **`if`:** Für einfache binäre Entscheidungen — ein Codeblock, wenn eine Bedingung wahr ist, ein anderer, wenn sie falsch ist.
- **`cond`:** Ein leistungsstarkes Mehrfachverzweigungskonstrukt für mehrere Bedingungen in klarer, strukturierter Form.
- **`and` / `or`:** Logische Operatoren, die Kombinationen von Bedingungen auswerten und komplexere Entscheidungen ermöglichen.
- **`else`:** Ein Fallback, der das Verhalten definiert, wenn keine der angegebenen Bedingungen zutrifft.

### So funktionieren Bedingungen

Bedingungen umfassen typischerweise:
1. **Bedingung auswerten:** Ein Testausdruck bestimmt, ob eine Bedingung wahr oder falsch ist.
2. **Verzweigte Ausführung:** Je nach Ergebnis wählt das Skript den auszuführenden Codeblock.
3. **Wert zurückgeben (optional):** In manchen Fällen liefern Bedingungen einen Wert, den andere Teile des Skripts nutzen können.