---
title: "Batch-Prozess"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
Ein praktisches, durchgängiges Beispiel für die Verarbeitung vieler Dateien auf einmal.

## Quellcode

- [Quellcode ansehen](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menü in Lumi

- **Datei → Stapelverarbeitung**

## Was gezeigt wird

- `SF-DIRNAME` Parameter für Quell-/Zielverzeichnisse
- Validierung von GUI-Pfaden mit Fallbacks (`validate-path-and-dir`)
- Rekursives Scannen und Iterieren von Verzeichnissen
- Fortschrittsberichte für lang laufende Vorgänge
