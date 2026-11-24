---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## Was ist Cursor?

[Cursor](https://cursor.com/) ist ein Code-Editor mit KI-Hilfe im selben Fenster. Er beruht auf Visual Studio Code, deshalb bleiben Syntaxhervorhebung, Erweiterungen, Arbeitsbereiche und Git erhalten. Cursor ist der Editor, der hier für Lumi-Skripte verwendet wird, weil diese KI-Hilfe deutlich günstiger ist.

- **Syntaxhervorhebung**: Die Syntaxhervorhebung färbt den Code ein. Fehler fallen leichter auf, die Struktur wird klarer, und komplexer Code lässt sich besser lesen.
- **Erweiterungen**: Cursor führt Erweiterungen von Visual Studio Code aus. Ob Sprache, Thema oder Werkzeug: Es gibt eine passende Erweiterung.
- **Integration der Versionskontrolle**: Git ist eingebaut. Änderungen lassen sich verfolgen und Zweige verwalten, mit Diensten wie [GitHub](https://github.com/) oder [GitLab](https://gitlab.com/).
- **KI im Editor**: Eine Suche, ein Refactoring oder eine kleine Änderung lässt sich direkt neben der geöffneten Datei anfragen. Die Kosten dieser Hilfe sind niedrig genug, um sie beim gewöhnlichen Schreiben von Skripten zu nutzen.

### Hauptfunktionen für Anfänger

- **Arbeitsbereiche**: Cursor fasst mehrere Projekte in „Arbeitsbereichen“ zusammen. Zusammengehörige Dateien und Ordner bleiben beieinander, die Navigation und die Projektverwaltung werden leichter.

- **Profile**: Mit Profilen lassen sich verschiedene Einrichtungen für Projekte oder Aufgaben anlegen. Ein Profil für die Webentwicklung, ein anderes für Scheme, jeweils mit eigenen Erweiterungen, Themen und Einstellungen.

### Erste Schritte mit Cursor

So starten Sie mit Cursor:

- **Herunterladen und installieren**: Gehen Sie zur [Cursor-Website](https://cursor.com/) und laden Sie die Linux-Version herunter.

- **Die Oberfläche erkunden**: Beim Öffnen von Cursor sehen Sie mehrere Bereiche. Links liegt die **Aktivitätsleiste**. Von dort wechseln Sie zwischen Dateien, Suche, Git und Erweiterungen.

- **Eine Datei oder einen Ordner öffnen**: Richten Sie den Arbeitsbereich ein, indem Sie den Ordner des Projekts hinzufügen, und speichern Sie die aktive Konfiguration in einer Arbeitsbereichsdatei.

Ein Arbeitsbereich ist nur eine gespeicherte Ordneransicht plus die Editoreinstellungen des Projekts.

### Erweiterungen

- **Erweiterungen installieren**: Klicken Sie in der Aktivitätsleiste auf das Symbol **Erweiterungen**, um nützliche Werkzeuge für Scheme zu suchen und zu installieren. Die wichtigste ist **„vscode-scheme“** von Allen Huang. Sie liefert Syntaxhervorhebung und eine grundlegende Unterstützung für Scheme, die Sprache, in der Lumi-Plug-ins geschrieben werden.

Sie können die Erweiterung auch anpassen, indem Sie die Sprachdatei bearbeiten und weitere Scheme-Schlüsselwörter oder Begriffe aus dem Lumi-Skripting aufnehmen.

Wenn Sie eine eigene Hilfsbibliothek pflegen, macht das Eintragen der Funktionsnamen in die Syntaxhervorhebung Navigation und Refactoring schneller.

### Fazit

Nutzen Sie einen Editor, in dem Suche, Refactoring und kleine Änderungen leicht sind und die KI in Reichweite bleibt. Cursor zusammen mit einer Scheme-Erweiterung ist hier die Grundlage.
