---
title: "Dateiformat (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Lumis natives `.lum`-Format ist ein Projektverzeichnis, keine einzelne abgeschlossene Datei. Es ist für Illustration mit Ebenen ausgelegt: tiefe Ebenenbäume, große Leinwände, Masken, zerstörungsfreie Effekte und Kontrollpunkte, die nicht das ganze Bild duplizieren müssen.

Aufgabe des Formats ist, diese Arbeitsstruktur unversehrt zu halten — damit ein Projekt getreu wieder geöffnet, bei Problemen geprüft und aus einem aktuellen Kontrollpunkt wiederhergestellt werden kann, ohne das Kunstwerk als undurchsichtigen Block zu behandeln.

## Absichtlich getrennte Teile

Ein `.lum`-Projekt ist ein Ordner. Ebenenbaum und Bildeigenschaften liegen in lesbarem XML. Jede Ebene und jede Maske behält ihren eigenen Pixelpuffer, benannt nach dem Kunstwerk statt nach einer internen ID. Vektorpfade werden als gewöhnliches SVG gespeichert. Umfangreiche Filtereinstellungen liegen in eigenen Dateien neben dem Bild. ICC-Profile stehen einmal im Projektordner, sodass Wiederherstellungsstände darauf verweisen statt sie zu kopieren.

Diese Trennung macht den Rest des Formats möglich. Unveränderte Ebenen können auf der Festplatte unangetastet bleiben. Ein beschädigter Puffer scheitert für sich, statt die ganze Datei mitzureißen. Fehlende Ebenenpixel werden zu leeren Ebenen, die Namen, Positionen und Überblendungseinstellungen behalten; fehlt das gespeicherte Gruppenbild, wird es aus den Kindern neu aufgebaut. Das Projekt bleibt eine Karte des Bildaufbaus.

Pigmentpaletten gehören zu Lumis Farbwerkzeugen. Ein Projekt kann merken, welche Palette dem Bild zugeordnet war — die Palettenbibliothek selbst liegt außerhalb von `.lum`.

## Bearbeitbarer Zustand, kein Abflachen

Die Datei speichert das Arbeitsbild. Ebenen bleiben Ebenen, Gruppen bleiben Gruppen, Masken bleiben Masken — einschließlich Versätzen, Sperren, Überblendungsverhalten und Filterstapeln. Zerstörungsfreie Filter werden als Operationen und Parameter gespeichert, nicht als festgeschriebene Pixel. Eine Ebene in einer einzigen Flächenfarbe braucht gar keine Pixeldatei.

Zugeklappte Gruppen behalten außerdem eine zusammengesetzte Ansicht ihrer selbst. Dieses gespeicherte Gruppenbild erscheint auf der Leinwand, wenn eine Gruppe geschlossen ist — die Kinder müssen nicht rekonstruiert werden, nur um das Bild zu sehen. Reine Inspektionsansichten bleiben außerhalb dieses Zwischenspeichers: Maske oder Alpha zur Bearbeitung anzuzeigen wird als Metadaten wiederhergestellt, nicht fest in die gespeicherte Gruppe übernommen.

## Große Dateien können teilweise auf der Festplatte bleiben

Beim Öffnen einer `.lum` müssen nicht alle Pixel geladen werden. Inhalt in zugeklappten Gruppen kann auf der Festplatte bleiben, während das gespeicherte Gruppenbild sofort angezeigt wird. Beim Aufklappen kommen diese Ebenen, Masken und verschachtelten Gruppen in den Speicher. Gruppen, die geschlossen bleiben, belasten den Speicher kaum.

Die Datei hält außerdem fest, welche Gruppen tatsächlich in Gebrauch waren. Gruppen auf dem Weg zur aktiven Auswahl können aufgeklappt wieder geöffnet werden; andere Ordner werden zugeklappt gespeichert, auch wenn sie in der letzten Sitzung offen waren. So muss eine tiefe Datei nicht jeden ungenutzten Zweig sofort in den Speicher laden.

Gruppieren ist deshalb ebenso eine Frage der Leistung wie der Organisation. Große Hintergründe, archivierte Experimente und ungenutzte Varianten können in geschlossenen Gruppen liegen, ohne denselben Speicher zu belegen wie die Ebenen, an denen gemalt wird. Das Speichern folgt derselben Regel: noch verborgene Puffer werden als Dateien kopiert oder übersprungen, nicht erst wieder in den Speicher geholt, nur um erneut geschrieben zu werden.

## Kontrollpunkte, die nur Änderungen schreiben

Datei → Speichern aktualisiert das Arbeitsprojekt. Inkrementelle Speicherungen und Automatisch speichern schreiben in einen Wiederherstellungsbaum und schreiben nur geänderte Daten — veränderte Ebenenpuffer, keine zweite Kopie des gesamten Bildes. Jeder Kontrollpunkt trägt trotzdem eine vollständige Beschreibung des Ebenenbaums, sodass jeder Stand in diesem Verlauf geöffnet werden kann, indem unveränderte Pixel aus älteren Kontrollpunkten und nötigenfalls aus der Arbeitsdatei selbst ergänzt werden.

Automatisch speichern nutzt dasselbe Muster in einem getrennten Zwischenspeicher, sodass der automatische Schutz die Datei auf der Festplatte nicht umschreiben muss. Wird ein Projekt geöffnet, während neuere Kontrollpunkte als der letzte vollständige Speicherstand existieren, kann Lumi sie anbieten, statt die neuere Arbeit stillschweigend zu verwerfen. Wiederhergestellte Bilder öffnen sich unter einem eigenen Namen, damit ein schnelles Speichern das Original nicht überschreibt.

## Ein Arbeitsformat

`.lum` dient dazu, ein Bild in Lumi weiterzumalen. Abgeflachte Formate und Kompatibilitätsformate sind für Veröffentlichung, Übergabe und andere Anwendungen. Weil ein Projekt ein Verzeichnis vieler Dateien ist, sollte es archiviert werden, wenn es transportiert werden muss.

Die Arbeitsdatei bleibt reich und bearbeitbar. Exporte sind der Weg, auf dem ein fertiges oder geteiltes Bild diese Struktur verlässt.
