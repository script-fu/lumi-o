---
title: "KI-unterstützte Entwicklung"
type: docs
---
Moderne KI-Tools können die Entwicklung von Lumi-Plug-ins erheblich beschleunigen, indem sie als kollaborativer Codierungspartner fungieren.

## VS-Code im Agentenmodus

Durch die Verwendung von Visual Studio Code mit einem KI-Assistenten im **Agent-Modus** (z. B. dem Agent-Modus von GitHub Copilot oder anderen Tool-fähigen Assistenten) können Sie komplexe, mehrstufige Aufgaben in natürlicher Sprache ausführen.

Anstatt nur eine einzige Codezeile fertigzustellen, kann ein Agent:
- Lesen Sie Ihren gesamten Arbeitsbereich, um den Kontext zu verstehen.
- Erstellen Sie neue Dateien und Verzeichnisse.
- Führen Sie Terminalbefehle aus, um Skripte zu testen oder zu validieren.
- Suchen Sie nach vorhandenen Mustern in Ihrer Codebasis.

## Repository-Zugriff

Die KI-Unterstützung ist am effektivsten, wenn der Agent Zugriff auf **lumi-dev** oder Ihr spezifisches Projekt-Repository hat. Mit Einblick in die vorhandene Codebasis kann der Agent:
- Verwenden Sie **[Utility Libraries](@@LUMI_TOKEN_4@@)** als Referenz für Hilfsfunktionen.
- Befolgen Sie bestehende Muster für GEGL-Operationen und Layer-Management.
- Boilerplate-Code aus etablierten Plug-Ins wiederverwenden.

## Beispiel-Workflow

Sie können den Agenten direkt bitten, ein vollständiges Plug-in zu generieren, indem Sie das gewünschte funktionale Ergebnis beschreiben:

> „Schreiben Sie mithilfe der verfügbaren Scheme-Dienstprogramme und Beispiele im Arbeitsbereich ein neues Plug-In, das eine 50 % horizontale Hilfslinie auf dem aktiven Bild erstellt und es „Center Guide“ nennt.“

Der Agent sucht nach Anleitungen zum Erstellen von Anleitungen, identifiziert die richtige Dienstprogrammfunktion (z. B. `lumi-image-add-hguide-percent` aus `common.scm`) und generiert die vollständige `.scm`-Datei mit dem richtigen Registrierungsbaustein.

## Best Practices

- **Seien Sie genau**: Beschreiben Sie genau, was das Plug-in tun soll.
- **Referenzdienstprogramme**: Ermutigen Sie den Agenten, im Verzeichnis `share/lumi/scripts/` nach hochrangigen Hilfsprogrammen zu suchen.
- **Überprüfen und testen**: Testen Sie immer das von der KI generierte Plug-in, es ist oft ein iterativer und kreativer Prozess.