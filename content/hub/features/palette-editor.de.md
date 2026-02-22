---
title: "Paletteneditor"
type: docs
---
Im Paletteneditor erstellen und verwalten Sie eine Lumi-Palette. Es enthält Ihren Pigmentsatz, speichert die Mischungen, die Sie im Palettenmixer gespeichert haben, zeichnet die Farben auf, die Sie tatsächlich beim Malen verwendet haben, und ermöglicht Ihnen die Konfiguration der Wertestruktur und Farbverläufe für die Palette.

## Auswählen einer Palette

Eine Palette ist mehr als eine Sammlung von Pigmenten: Sie ist eine stilistische Verpflichtung. Viele Künstler arbeiten mit einem kleinen, festen Satz von Pigmenten, die sie genau kennen: die Art und Weise, wie sie sich mischen, die Neutraltöne, die sie erzeugen, die Temperaturunterschiede zwischen ihnen. Diese Vertrautheit wird Teil ihrer visuellen Stimme. Ein Maler könnte eine warme Palette mit niedrigem Chroma für Figurenarbeiten und eine separate High-Key-Palette für Landschaften beibehalten, oder er könnte seine gesamte Arbeit innerhalb eines einzigen Vier-Pigment-Sets erledigen, als bewusste Einschränkung, die ein Gesamtwerk vereinheitlicht.

Lumi unterstützt diese Arbeitsweise. Jede Palette hat ihre eigenen Pigmente, Mischungen, Wertestrukturen und Farbverläufe. Durch das Wechseln der Paletten ändert sich das gesamte Farbsystem: Die Karte, der Mixer und die verfügbaren Mischungen werden alle aktualisiert, um den neuen Satz widerzuspiegeln.

Ein Dropdown-Menü oben im Paletteneditor wählt die aktive Palette aus. Lumi wird mit drei Paletten in der **Standard**-Gruppe ausgeliefert:

| Palette | Charakter |
| :--- | :--- |
| **Standard** | Eine vielseitige Palette mit warmem Farbton, die das gesamte Farbspektrum abdeckt. Guter Ausgangspunkt für die meisten Themen. |
| **Meister** | Eine große Vollspektrumpalette für Maler, die eine maximale Farbtonabdeckung und explizite Kontrolle über die Vergrauungsachsen wünschen. |
| **Zorn** | Eine auf vier Pigmente begrenzte Palette, die auf dem Ansatz von Anders Zorn basiert. Deckt eine überraschend große Auswahl an warmen Hauttönen und Neutraltönen mit geringem Chroma aus einem minimalen Pigmentsatz ab. |

Paletten können auch über die Registerkarte „Paletten“ erstellt, importiert oder dupliziert werden.

## Palettenpigmente

Der Abschnitt **Palettenpigmente** oben in der Palettenansicht listet Ihre primären Einträge auf: die Basispigmente, aus denen der Rest der Palette besteht. Dies sind die Eingaben für das Spektralmischsystem. Sekundäre und tertiäre Elemente werden daraus automatisch generiert und zum Füllen der Palettenzuordnung verwendet

## Gespeicherte Mischungen

Der Abschnitt **Gespeicherte Mischungen** enthält Farben, die Sie mit **Zur Palette hinzufügen** explizit aus dem Palettenmixer behalten haben. Dies sind Ihre abgeleiteten Farben: die Ergebnisse der Spektralmischung, Ton- und Chroma-Anpassungen, die zur Wiederverwendung gespeichert werden.

Gespeicherte Mixe sind in fünf Wertebereiche unterteilt:

| Band | Standardhelligkeitsbereich |
| :--- | :--- |
| Hoher Schlüssel | 80 – 100 % |
| Obere Mitte | 60 – 80 % |
| Mitte | 40 – 60 % |
| Untere Mitte | 20 – 40 % |
| Tief | 0 – 20 % |

Lumi ordnet jede gespeicherte Mischung basierend auf ihrer wahrnehmbaren Helligkeit (CIE L\*) automatisch dem entsprechenden Band zu. Dies organisiert Ihre Mischungen nach Wert, anstatt eine flache Liste zu durchsuchen, und entspricht normalerweise der Art und Weise, wie ein Künstler über Farben denkt.

Gespeicherte Mischungen können über die Schaltfläche **Benutzerdefiniert umbenennen** oder das Kontextmenü umbenannt werden.

## Gebrauchte Mischungen

Der Abschnitt **Gebrauchte Mischungen** ist eine durch Farbe ausgelöste Geschichte. Jedes Mal, wenn eine Farbe aus der Palette auf die Leinwand aufgetragen wird, wird dies hier aufgezeichnet. Gebrauchte Mischungen werden von der neuesten zur neuesten Version sortiert.

Dieser Abschnitt ist nützlich, um eine Farbe abzurufen, mit der Sie gemalt, aber nicht explizit gespeichert haben. Um einen verwendeten Mix dauerhaft zu behalten, wählen Sie ihn aus und klicken Sie auf **Heraufstufen**. Anschließend wird er in die Liste „Gespeicherte Mixe“ im entsprechenden Wertebereich verschoben.

Verwendete Mischungen werden pro Palette gespeichert und bleiben zwischen Sitzungen bestehen.

## WertebänderWertebänder definieren, wo die Grenzen zwischen den fünf Helligkeitszonen liegen. Standardmäßig verteilen sie die Helligkeit gleichmäßig über den Bereich von 0–100 %, Sie können sie jedoch anpassen, um sie an die Tonstruktur Ihres Motivs anzupassen. Für Maler ist es nützlich, Wertebereiche und die Lücken zwischen ihnen zu definieren und zu verwalten.

### Der Werteband-Schieberegler

Der **Wertebänder-Expander** im Paletteneditor enthält einen Schieberegler mit fünf verschiebbaren Trennlinien. Ziehen Sie eine beliebige Trennlinie, um die Grenze zwischen benachbarten Bändern zu verschieben. Die Beschriftung über dem Schieberegler zeigt den Namen und den genauen Prozentbereich des aktiven Bandes.

**Tasten:**

| Knopf | Wirkung |
| :--- | :--- |
| **Abbrechen** | Setzt den Schieberegler auf den zuletzt angewendeten Zustand zurück |
| **Kopieren** | Kopiert die aktuelle Bandkonfiguration in die Zwischenablage |
| **Einfügen** | Fügt eine kopierte Bandkonfiguration aus einer anderen Palette ein |
| **Standardeinstellungen** | Stellt die werkseitigen Standardeinstellungen für gleiche Divisionen wieder her |
| **Anwenden** | Übernimmt die Änderungen und generiert die Palette neu |

**Übernehmen** ist erforderlich, um die Änderungen dauerhaft zu machen. Es löst eine vollständige Regeneration der Palette aus und entfernt alle gespeicherten Mischungen, deren Helligkeit nicht mehr in ein Band fällt. Lumi zeigt einen Bestätigungsdialog an, in dem aufgeführt wird, wie viele Mischungen entfernt werden, bevor fortgefahren wird.

### Wertebänder und die Palettenzuordnung

Die Palettenkarte zeigt die Palette als Farbtonrad mit 36 Farbtonsektoren (jeweils 10°) und 15 Helligkeitszellen an, die als konzentrische Ringe angeordnet sind. Jedes Band entspricht drei Ringen: die fünf Bänder × 3 Ringe = insgesamt 15 Zellen.

Durch Anpassen der Wertebänder wird verschoben, welche Helligkeitswerte in den einzelnen Ringebenen landen. Durch ein zum dunklen Ende hin verdichtetes Band decken die drei Ringe einen engeren Tonumfang ab; Ein breites Band verleiht den drei Ringen eine größere Klangverbreitung. Auf diese Weise passt sich dieselbe Palettenzuordnungsstruktur an Paletten an, die auf unterschiedliche Tonprioritäten abgestimmt sind.

## Palettenverläufe

Jede Palette kann einen oder mehrere **Verläufe** speichern: sanfte Verläufe, die aus Paletteneinträgen abgeleitet werden und als Verlaufsfüllungen auf die Leinwand angewendet oder als Referenzstreifen verwendet werden können.

Farbverläufe werden im **Verläufe-Expander** verwaltet. Die Kombination oben listet die Farbverläufe in der aktuellen Palette auf. **Hinzufügen** erstellt einen neuen Farbverlauf. **Entfernen** löscht das ausgewählte. **Umbenennen** benennt es um.

### Verlaufseditor

Der **Verlaufseditor-Expander** konfiguriert den ausgewählten Verlauf. Jeder Farbverlauf verfügt über drei Endpunkte (**A**, **B** und **C**), die als Farbfelder angezeigt werden. Klicken Sie auf ein Farbfeld, um es zum aktiven Endpunkt für die Bearbeitung zu machen.

Jeder Endpunkt kann festgelegt werden, indem Sie auf **Auswählen** und dann auf einen Paletteneintrag in der Palettenübersicht oder der Palettenansicht klicken. Der Endpunkt ist über die UID mit diesem Paletteneintrag verknüpft. Wenn sich der Eintrag ändert, wird der Farbverlauf aktualisiert.

**Kontrollen pro Endpunkt:**

| Kontrolle | Wirkung |
| :--- | :--- |
| **Stärke** | Wie stark die Endpunktfarbe im Verhältnis zu ihren Nachbarn beiträgt |
| **Deckkraft** | Alpha der Endpunktfarbe im Farbverlauf |
| **Kurve** | Gamma-Anpassung für den Farbabfall von diesem Endpunkt |

**Verteilungsschieberegler** (S1, S2, S3) legen fest, wo die drei Mittelpunkte zwischen den Endpunkten entlang des Verlaufsstreifens liegen. Durch das Zurücksetzen werden die Mittelpunkte auf den gleichen Abstand zurückgesetzt.

Der Verlaufsvorschaustreifen oben im Verlaufseditor-Block zeigt das Ergebnis der aktuellen Endpunkt- und Verteilungseinstellungen.

## Palette andockbarDie andockbare **Palette** (**Panels > Palette**) ist ein einfacheres, leseorientiertes Panel zum Durchsuchen und Auswählen von Farben aus jeder Palette. Es zeigt die gleiche dreiteilige Ansicht (Palettenpigmente, gespeicherte Mischungen, verwendete Mischungen) ohne die Expander „Wertebänder“ und „Verläufe“.

Über ein Palettenauswahl-Dropdown-Menü oben können Sie zwischen allen verfügbaren Paletten wechseln. Klicken Sie auf einen beliebigen Eintrag, um ihn als Vordergrundfarbe festzulegen. Doppelklicken Sie, um den Farbnamen-Editor zu öffnen. Für beschreibbare Paletten sind in der Schaltflächenleiste die Aktionen „Farbe bearbeiten“, „Neue Farbe aus FG“ und „Farbe löschen“ verfügbar.

Die andockbare Palette ist für den schnellen Farbzugriff während des Malens gedacht, wenn der vollständige Paletteneditor zu viel Platz beanspruchen würde.

## Registerkarte „Paletten“.

Die **Registerkarte „Paletten“** (verfügbar als andockbare Registerkarte) zeigt die aktive Palette im Kompaktmodus. Es schließt die Pigmente aus, um sich auf gespeicherte Mischungen zu konzentrieren