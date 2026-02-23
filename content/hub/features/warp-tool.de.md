---
title: "Warp-Werkzeug"
type: docs
---
Das Warp-Werkzeug verschiebt, zieht und lässt Pixel frei über die Leinwand fließen. In Lumi geht es weiter als die meisten Implementierungen: Es kann eine ganze Ebenengruppe – egal wie viele verschachtelte Ebenen und Masken sie enthält – als einzelnes einheitliches Objekt verzerren, ohne dass die Struktur abgeflacht wird oder verloren geht.

## Übersicht

Wählen Sie eine Ebene aus und ziehen Sie darüber, um Pixel in eine beliebige Richtung zu verschieben. Die Verzerrung ist während der Arbeit nicht destruktiv: Sie können einzelne Striche rückgängig machen und wiederherstellen, die Pinselgröße oder das Verhalten zwischen den Strichen ändern und mit der Verfeinerung fortfahren, bis Sie einen Commit ausführen. Beim Festschreiben wird die akkumulierte Verschiebungskarte destruktiv auf die Pixeldaten der Ebene angewendet.

Wenn eine **Gruppenebene** ausgewählt ist, bearbeitet das Werkzeug die Gruppe als Ganzes. Sie sehen und interagieren mit einer Live-Vorschau der gesamten zusammengesetzten Gruppe. Beim Commit wird derselbe Warp präzise und unabhängig auf jede untergeordnete Ebene und Maske innerhalb der Gruppe angewendet, wobei die vollständige Ebenenstruktur erhalten bleibt.

## Gruppenverzerrung

Das Verzerren einer Gruppe ist die Hauptfähigkeit, die Lumis Verzerrungswerkzeug auszeichnet.

### Das Problem, das es löst

In den meisten Malprogrammen erfordert das Verzerren einer Illustration mit mehreren Ebenen entweder das Glätten der Gruppe zuerst (wodurch die Ebenenstruktur zerstört wird) oder das separate Verzerren jeder Ebene und den Versuch, sie nach Augenmaß anzupassen (mühsam und ungenau). Bei keinem der beiden Ansätze bleibt die ursprüngliche Struktur für eine weitere zerstörungsfreie Bearbeitung erhalten.

Lumi verformt die gesamte Gruppe als ein Element und verteilt dann genau dieselbe Transformation auf jede Ebene darin.

### Wie es funktioniert

Wenn Sie eine Gruppe auswählen und mit einem Warp-Strich beginnen, erstellt Lumi eine **schwebende Vorschauebene** aus der zusammengesetzten Projektion der Gruppe. Wenn die Gruppe über eine Maske verfügt, wird die Maske in die Vorschau integriert, sodass die Vorschau das endgültige Erscheinungsbild genau wiedergibt. Sie malen Ihre Warp-Striche direkt in dieser Vorschau – was Sie sehen, ist genau das, was Sie erhalten.

Beim Commit, Lumi:

1. Wendet die Verschiebung auf jede Basisebene innerhalb der Gruppe an (einschließlich tief verschachtelter Ebenen in Untergruppen) und erweitert die Leinwand jeder Ebene gerade so weit, dass der gesamte Warp-Bereich erfasst wird.
2. Wendet im selben Durchgang auf jede Maske innerhalb der Gruppe die gleiche Verschiebung an.
3. Nimmt die automatische Grenzenberechnung der Gruppe wieder auf, sodass die Größe der Gruppe an ihre neu verzerrten untergeordneten Elemente angepasst wird.
4. Beschneidet jede verzerrte Ebene wieder auf ihren tatsächlich gemalten Inhalt, um die Dateigröße kompakt zu halten.
5. Entfernt die Vorschauebene und generiert die Gruppenprojektion aus den aktualisierten untergeordneten Elementen neu.

All dies geschieht in einem einzigen Rückgängig-Schritt. Nach dem Festschreiben sieht die Gruppe genauso aus wie in der Vorschau, wobei alle Ebenen und Masken intakt sind.

### Masken

Die Option **Warp-Masken** (standardmäßig aktiviert) bewirkt, dass Masken auf jeder Ebene und Gruppe innerhalb des Warp-Ziels die identische Verschiebungstransformation erhalten. Ebenenmasken bewegen sich mit ihren Ebenen: Eine Maske, die den Umriss einer Figur beschnitten hat, schneidet nach dem Verzerren weiterhin denselben Umriss ab.

Wenn **Warp-Masken** deaktiviert ist, wird nur der Ebeneninhalt verschoben; Masken behalten ihre ursprüngliche Position.

## Werkzeugoptionen

### Verhalten

| Modus | Wirkung |
| :--- | :--- |
| **Verschieben** | Verschiebt Pixel in Richtung des Strichs. Der primäre Modus für die meisten Warping-Arbeiten. |
| **Wachsen** | Erweitert Pixel von der Pinselmitte nach außen. |
| **Schrumpfen** | Zieht Pixel nach innen zur Pinselmitte. |
| **Im Uhrzeigersinn wirbeln** | Dreht Pixel im Uhrzeigersinn um die Pinselmitte. |
| **Gegen den Uhrzeigersinn wirbeln** | Dreht Pixel gegen den Uhrzeigersinn um die Pinselmitte. |
| **Löschen** | Entfernt die Warp-Verschiebung und stellt die Pixel an ihren ursprünglichen Positionen wieder her. |
| **Glatt** | Streut Verschiebungen und mildert abrupte Übergänge zwischen verzerrten und unverzerrten Bereichen. |

### Pinselsteuerung

- **Größe**: Durchmesser des Warp-Pinsels in Pixel. Größere Bürsten verdrängen breitere Bereiche mit einem weicheren Abfall; Kleinere Bürsten ermöglichen eine präzise, ​​lokale Kontrolle.
- **Härte**: Abfall von der Mitte zum Rand. Hohe Härte erzeugt eine gleichmäßige Verschiebung über die gesamte Bürstenfläche; Eine geringe Härte konzentriert den Effekt im Zentrum.
- **Stärke**: Wie weit Pixel pro Strich verschoben werden. Eine geringere Festigkeit ermöglicht eine subtile, allmähliche Formgebung; Höhere Kraft erzeugt dramatische, schnelle Bewegungen.

### Taktzeitpunkt

- **Strich während der Bewegung** (nur Bewegungsmodus): Wendet Warp kontinuierlich an, während sich die Maus bewegt, und nicht nur bei einem Timer-Impuls. Verwenden Sie diese Option für fließende, pinselartige Striche, bei denen die Verschiebung dem Cursor direkt folgen soll.
- **Stroke Periodically**: Wendet Warp in einem festen Zeitintervall an, während die Maustaste gedrückt gehalten wird. Wird für die Modi „Wachstum“, „Schrumpfen“ und „Wirbel“ verwendet, bei denen eine kontinuierliche kreisförmige Anwendung beabsichtigt ist.
- **Rate**: Die Häufigkeit der periodischen Schlaganwendung.

### Qualität

- **Interpolation**: Die beim Festschreiben verwendete Stichprobenmethode. Linear ist für die meisten Arbeiten schnell und reibungslos; „Cubic“ und „Nohalo“ sorgen für eine höhere Wiedergabetreue bei feinen Details.
- **Hochwertige Vorschau**: Verwendet den Commit-Qualitäts-Sampler während der interaktiven Vorschau. Langsamer, aber die Vorschau entspricht genau dem Commit-Ergebnis.

### Gruppenoptionen

- **Warp-Bereich erweitern** (nur Gruppen-Warp): Die Anzahl der Pixel, die als transparenter Rand um die Gruppenvorschau auf allen Seiten hinzugefügt werden. Dies gibt verdrängten Inhalten Raum zum Einzug. Die Standardeinstellung von 256 Pixel ist für die meisten Arbeiten ausreichend; Reduzieren Sie den Wert für große Bilder, bei denen es auf den Speicher ankommt, oder erhöhen Sie ihn für sehr große Verschiebungsstriche.
- **Warp-Masken**: Legt fest, ob derselbe Warp auf Ebenen- und Gruppenmasken angewendet werden soll. Standardmäßig aktiviert.

## Rückgängig machen und Wiederherstellen

Jeder Strich ist ein einzelner Rückgängig-Schritt innerhalb der Warp-Sitzung. **Strg+Z** entfernt den letzten Strich und stellt die Verschiebungskarte in ihrem vorherigen Zustand wieder her. **Strg+Y** (oder **Strg+Umschalt+Z**) wendet es erneut an. Sie können den gesamten Schlaganfallverlauf noch einmal durchgehen, bevor Sie eine Entscheidung treffen.

Durch Drücken von **Escape** oder Wechseln der Werkzeuge werden alle nicht übernommenen Striche verworfen und die Ebene(n) in ihren ursprünglichen Zustand zurückversetzt. Es werden keine Änderungen geschrieben, bis Sie sie explizit festschreiben.

## Commitment

Klicken Sie auf die Schaltfläche **Commit** (oder drücken Sie **Enter**), um den angesammelten Warp destruktiv anzuwenden. Bei Gruppen-Warps löst dies die oben beschriebene vollständige mehrschichtige Anwendung aus. Der Rückgängig-Verlauf für den festgeschriebenen Warp ist dann ein einzelner Eintrag im Bild-Rückgängig-Stapel, der mit der Standardfunktion **Bearbeiten → Rückgängig** rückgängig gemacht werden kann.