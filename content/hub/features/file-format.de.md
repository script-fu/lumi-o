---
title: "Dateiformat (.lum)"
type: docs
---
Lumi verwendet ein offenes, verzeichnisbasiertes Dateiformat (`.lum`), das auf Leistung, Zuverlässigkeit und langfristige Zugänglichkeit ausgelegt ist.

## Übersicht

Eine `.lum`-Datei ist eigentlich ein Verzeichnis, das Folgendes enthält:
- **Metadaten** (Ebenen, Mischmodi, Eigenschaften).
- **Ebenenpuffer** (einzelne Pixeldaten für jede Ebene).
- **Masken** (Graustufendaten für Ebenenmasken).
- **Wiederherstellungsverlauf** (inkrementelle Snapshots).

Diese Struktur ermöglicht schnelles Speichern, verzögertes Laden großer Dateien und Wiederherstellung der Arbeit auch nach einem Absturz.

## Schlüsseleigenschaften

### Offen und lesbar

Das Format `.lum` verwendet XML-Metadaten und komprimierte Binärpuffer. Sie können Ebenenstruktur, Eigenschaften und Mischmodi im Klartext überprüfen. Kein proprietärer Codec; Pixeldaten werden im Standard-GEGL-Pufferformat gespeichert.

### Inkrementelles Speichern

Das inkrementelle Speichern muss pro Projekt im Dialogfeld **Speichern unter** aktiviert werden (ein Kontrollkästchen **Inkrementelles Speichern** und eine Drehschaltfläche **Maximale Speicherung**). Sobald die Tastenkombination Strg+S aktiviert ist, werden nur die geänderten Ebenen geschrieben, anstatt das gesamte Projekt neu zu schreiben, was die Speicherzeit drastisch verkürzt. Die Einstellung wird mit dem Projekt gespeichert und bleibt sitzungsübergreifend bestehen.

### Lazy Loading

Große Projekte werden schnell eröffnet. Ebenenpixel werden nur dann von der Festplatte geladen, wenn:
- Die Ebene wird sichtbar gemacht.
- Sie malen auf der Ebene.
- Die Ebene wird exportiert oder zusammengesetzt.

Sehr große Projekte (mehr als 500 Schichten, mehrere Gigabyte Daten) bleiben reaktionsfähig. Lazy Loading ist standardmäßig aktiviert und kann unter **Bearbeiten → Einstellungen → Leistung → Speicherressourcen** umgeschaltet werden.

### Automatisch speichern

Lumi speichert Änderungen automatisch in regelmäßigen Abständen an einem **separaten Cache-Speicherort** (`~/.cache/lumi/autosave/`). Automatische Speicherungen sind unabhängig von der Arbeitsdatei und verändern diese nicht. Das Intervall und der Cache-Speicherort können unter **Bearbeiten → Einstellungen → Leistung** konfiguriert werden.

## Zugriff

### Speichern und Speichern unter

- **Datei** → **Speichern** (Strg+S): Im aktuellen `.lum` Verzeichnis speichern.
- **Datei** → **Speichern unter** (Umschalt+Strg+S): In einer neuen `.lum` Datei speichern. Das Dialogfeld „Speichern unter“ enthält Optionen für den Komprimierungstyp und einen Umschalter „Inkrementelles Speichern“ (mit einer Beschränkung für „maximale Speicherung“), um das inkrementelle Speichern für dieses Projekt zu aktivieren oder zu deaktivieren.

Nicht gespeicherte Änderungen werden durch ein Sternchen (*) im Fenstertitel gekennzeichnet.

### Exportieren

- **Datei** → **Exportieren als** (Umschalt+Strg+E): Export in PNG, JPEG, TIFF oder andere Formate.
- **Datei** → **Überschreiben** (Strg+E): Erneuter Export zur zuletzt exportierten Datei.

Durch den Export werden sichtbare Ebenen abgeflacht und vom Spektralfarbraum in den sRGB-Farbraum konvertiert.

### Importieren

- **Datei** → **Öffnen** (Strg+O): Laden Sie ein `.lum` Projekt.
- **Datei** → **Als Ebenen öffnen** (Umschalt+Strg+O): Importieren Sie `.lum`-, XCF- oder PSD-Dateien als neue Ebenen.
- **Datei** → **Zuletzt verwendete Dateien**: Schneller Zugriff auf kürzlich geöffnete Projekte.

PSD- und XCF-Dateien werden beim Import in das native Format von Lumi konvertiert.

## Import- und Exportkompatibilität

### Unterstützte Importformate
- **.lum**: Natives Lumi-Format.
- **.xcf**: natives GIMP-Format (Ebenen und grundlegende Eigenschaften bleiben erhalten).
- **.psd**: Photoshop-Format (Ebenen und Mischmodi bleiben erhalten).
- **PNG, JPEG, TIFF usw.**: Reduzierter Bildimport.

### Unterstützte Exportformate
- **PNG**: Verlustfrei, mit Alpha-Transparenz.
- **JPEG**: Verlustbehaftet, abgeflacht.
- **TIFF**: Verlustfrei oder LZW-komprimiert.
- **XCF**: GIMP-Kompatibilitätsformat. Nur Export; Schichten und Grundeigenschaften bleiben erhalten.

## ProjektwiederherstellungLumi verfügt über automatische Hintergrundspeicherungen und manuelle inkrementelle Prüfpunkte, die beide über **Datei** → **Bild wiederherstellen** zugänglich sind. Ausführliche Informationen finden Sie auf der Seite [File Recovery](../recovery).

## Organisation

Eine `.lum` Datei ist ein Verzeichnis mit einer festen Struktur:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Layer-Puffer werden nach dem Layer benannt (`layer-Background.geglbuf`) und nicht fortlaufend nummeriert. Leerzeichen in Ebenennamen werden als Unterstriche gespeichert; Gruppenebenen erhalten das Suffix `-GROUP`. Masken teilen sich den Ebenennamen (`mask-Background.geglbuf`).

Jedes `recovery/primary-NN.lum/` ist eine vollständige Grundsicherung. Durch nachfolgendes Drücken von Strg+S werden `delta-NNNN.lum/` Unterverzeichnisse angehängt, die nur die seit der letzten Baseline geänderten Puffer enthalten, sodass Prüfpunktspeicherungen unabhängig von der Projektgröße schnell erfolgen.

Autosaves folgen derselben Struktur, werden jedoch separat in `~/.cache/lumi/autosave/` gespeichert, sodass die Arbeitsdatei unberührt bleibt.
- **Sehr große Projekte**: Ein Projekt mit mehr als 1000 Ebenen und Terabytes an Daten profitiert am meisten vom Lazy Loading; Der endgültige Export in das flache Bildformat kann jedoch einige Zeit dauern.
- **Netzwerklaufwerke**: Das Speichern in im Netzwerk bereitgestellten Verzeichnissen wird unterstützt, ist jedoch aufgrund der E/A-Latenz langsamer als lokaler Speicher.