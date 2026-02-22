---
title: "Utility-Browser"
type: docs
---
Mit dem Utility-Browser können Sie das integrierte Scheme-Dienstprogramm stdlib erkunden, das im Lieferumfang von Lumi enthalten ist, ohne die App verlassen oder Quelldateien durchsuchen zu müssen.

## Öffnen des Utility-Browsers

Gehen Sie zu **Hilfe → Programmierung → Utility-Browser**.

Das Fenster öffnet sich sofort; Es muss kein Plug-In vorab geladen werden.

## Was es zeigt

Der Browser listet alle Prozeduren, Variablen und Syntaxformen auf, die von den sieben Dienstprogrammbibliotheken exportiert werden, die Lumi beim Start automatisch lädt:

| Bibliothek | Was es abdeckt |
|---|---|
| `common.scm` | Allzweck-Helfer (Dienstprogramme für Zeichenfolgen, Zahlen, Listen) |
| `files.scm` | Datei- und Pfad-Helfer |
| `gegl.scm` | GEGL-Puffer und Farbhelfer |
| `images.scm` | Helfer auf Bildebene (`image-get-open-list` usw.) |
| `layers.scm` | Ebenen- und Zeichenhilfen |
| `parasites.scm` | Lese-/Schreibhilfen für Parasiten |
| `paths.scm` | Pfad- und Vektorhelfer |

All dies ist in jedem Scheme-Plug-in oder in der Scheme-Konsole verfügbar.

## Suchen und Filtern

- **Suchfeld**: Filtert während der Eingabe nach Namen (Groß-/Kleinschreibung wird bei der Teilzeichenfolgenübereinstimmung nicht berücksichtigt).
- **Typfilter**: Ergebnisse auf `procedure`, `variable` oder `syntax` eingrenzen.

Wenn Sie auf einen Eintrag klicken, werden dessen vollständiger Dokumentstring und die Bibliothek angezeigt, aus der er stammt.

## Die Stdlib als Wrapper

Die Dienstprogrammbibliotheken sind eine praktische Anwendung des Umbruchmusters: Jeder Helfer gibt einem Low-Level-Vorgang einen eindeutigen Namen, verbirgt Boilerplate und bietet einen einzigen Ort zum Aktualisieren, wenn sich der zugrunde liegende Befehl ändert. Wenn Sie den Designansatz dahinter verstehen möchten, sehen Sie sich das Tutorial **[Wrapping](@@LUMI_TOKEN_11@@)** an.

## Beziehung zum Prozedurbrowser

Der Utility-Browser ist getrennt von **Filter → Script-Fu → Konsole → Durchsuchen** (dem Prozedur-Browser). Der Prozedurenbrowser listet PDB-registrierte Prozeduren auf. Der Utility-Browser listet Hilfsdefinitionen auf, die absichtlich *außerhalb* der PDB leben: Sie sind nur Scheme-definiert und haben keine C-Bindung.