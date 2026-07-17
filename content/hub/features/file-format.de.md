---
title: "Dateiformat (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---
Lumis natives Dateiformat ist für mehrschichtige Malprojekte gedacht, die zuverlässig, nachvollziehbar und langfristig wiederherstellbar bleiben müssen. Es orientiert sich an Illustrationsarbeit: viele Ebenen, große Leinwände, eingebettete Farbinformationen, Masken, Effekte und Wiederherstellungsdaten.

Statt ein Projekt als undurchsichtigen Block zu behandeln, bleibt die Kunstwerkstruktur für die Anwendung sichtbar. So speichert, lädt und stellt Lumi große Bilder intelligenter wieder her — und bewahrt die Organisation, auf die Künstler angewiesen sind.

## Offene Projektstruktur

Ein Lumi-Projekt hält die Teile des Kunstwerks getrennt: Bildstruktur, Ebeneninhalt, Masken, Farbdaten, Metadaten und Wiederherstellungsinformationen haben jeweils eine klare Rolle. Das Format ist leichter nachvollziehbar und für langfristigen Zugriff besser geeignet als ein geschlossener Monolith.

Ziel ist nicht nur, Pixel zu speichern, sondern den Arbeitszustand einer Illustration. Ebenen bleiben Ebenen, Masken bleiben Masken — die Datei spiegelt weiter, wie das Kunstwerk aufgebaut ist.

## Für große Bilder ausgelegt

Große Ebenenbilder werden schnell schwer. Lumis Format unterstützt Workflows, bei denen nicht alle Bilddaten auf einmal in den Speicher müssen. Projekte bleiben reaktionsschnell, indem nur die Teile geladen werden, die Anzeige, Bearbeitung, Komposition oder Export brauchen.

So fühlen sich komplexe Dateien handhabbar an — besonders bei vielen verborgenen, archivierten, experimentellen oder gruppierten Ebenen.

## Speichern ohne Unterbrechung

Das Format unterstützt normales Projektspeichern und leichte Wiederherstellungs-Snapshots. Künstler können Arbeit häufig schützen, ohne jeden Kontrollpunkt in ein vollständiges Duplikat des gesamten Bildes zu verwandeln.

Weil Wiederherstellungsinformationen zur Projektstruktur gehören, kann Lumi nützlichen Verlauf nah am Kunstwerk halten und automatische Sicherungsspeicherungen getrennt von der Arbeitsdatei ablegen.

## Austausch und Export

Das native Format ist für laufende Lumi-Arbeit gedacht; Exportformate dienen der Weitergabe abgeflachter oder kompatibilitätsorientierter Ergebnisse. Import bringt bestehende Kunstwerke in Lumis Ebenenumgebung; Export lässt fertige Stücke das Projektformat verlassen, wenn sie veröffentlicht, geliefert oder weiterverarbeitet werden.

Die Arbeitsdatei bleibt reich und bearbeitbar; Endbilder entstehen in gängigen externen Formaten.

## Langfristige Zuverlässigkeit

Kurz gesagt: Das `.lum`-Format ist ein praktischer Behälter für ernsthafte Malarbeit — offen genug zum Inspizieren, strukturiert genug zur Wiederherstellung, flexibel genug für komplexe Ebenenbilder ohne unnötigen Overhead.
