---
title: "Batch-Prozess"
type: docs
---
Ein praktisches, durchgängiges Beispiel für die Verarbeitung vieler Dateien auf einmal.

## Wo es lebt

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Wo es in Lumi erscheint

- **Datei → Stapelverarbeitung**

## Was es zeigt

- `SF-DIRNAME` Parameter für Quell-/Zielverzeichnisse
- Validierung von GUI-Pfaden mit Fallbacks (`validate-path-and-dir`)
- Rekursives Scannen und Iterieren von Verzeichnissen
- Fortschrittsberichte für lang laufende Vorgänge