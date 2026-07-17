---
title: "KI-unterstützte Entwicklung"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Moderne KI-Tools können die Entwicklung von Lumi-Plug-Ins erheblich beschleunigen, indem sie als kollaborativer Codierungspartner fungieren.

## VS Code im Agent-Modus

Mit Visual Studio Code und einem KI-Assistenten im **Agent-Modus** (z. B. GitHub Copilots Agent-Modus oder andere toolfähige Assistenten) können Sie komplexe, mehrstufige Aufgaben in natürlicher Sprache ausführen.

Statt nur eine einzelne Codezeile zu vervollständigen, kann ein Agent:
- den gesamten Arbeitsbereich lesen, um den Kontext zu verstehen
- neue Dateien und Verzeichnisse anlegen
- Terminalbefehle ausführen, um Skripte zu testen oder zu validieren
- nach vorhandenen Mustern in der Codebasis suchen

## Repository-Zugriff

KI-Unterstützung ist am effektivsten, wenn der Agent Zugriff auf **lumi-dev** oder Ihr Projekt-Repository hat. Mit Einblick in die vorhandene Codebasis kann der Agent:
- die **[Utility Libraries]({{< ref "/hub/scripting/reference/utility-browser" >}})** als Referenz für Hilfsfunktionen nutzen
- bestehende Muster für GEGL-Operationen und Ebenenverwaltung befolgen
- Boilerplate-Code aus etablierten Plug-Ins wiederverwenden

## Beispiel-Workflow

Sie können den Agenten direkt bitten, ein vollständiges Plug-In zu erzeugen, indem Sie das gewünschte funktionale Ergebnis beschreiben:

> „Schreiben Sie mithilfe der verfügbaren Scheme-Utilities und Beispiele im Arbeitsbereich ein neues Plug-In, das eine horizontale Hilfslinie bei 50 % auf dem aktiven Bild erstellt und sie ‚Center Guide‘ nennt.“

Der Agent sucht nach der Vorgehensweise zum Erstellen von Hilfslinien, findet die passende Utility-Funktion (z. B. `lumi-image-add-hguide-percent` aus `common.scm`) und erzeugt die vollständige `.scm`-Datei mit dem korrekten Registrierungs-Boilerplate.

## Bewährte Praktiken

- **Seien Sie präzise**: Beschreiben Sie genau, was das Plug-In tun soll.
- **Utilities referenzieren**: Weisen Sie den Agenten auf das Verzeichnis `share/lumi/scripts/` hin, um High-Level-Helfer zu finden.
- **Prüfen und testen**: Testen Sie jedes von der KI erzeugte Plug-In — oft ist das ein iterativer, kreativer Prozess.
