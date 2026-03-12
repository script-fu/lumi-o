---
title: "Pinsel-Caching"
type: docs
---
Das Pinsel-Caching soll dafür sorgen, dass sich Ihre Lieblingspinsel so früh wie möglich schnell anfühlen. Anstatt denselben transformierten Pinselstempel immer wieder neu zu berechnen, kann Lumi einen Cache mit den Pinselformen speichern, die Sie tatsächlich verwenden, und diesen Cache später automatisch neu laden.

## Übersicht

Die Funktion basiert auf der Idee, dass viele ausdrucksstarke Pinsel beim Malen immer noch dieselben praktischen Kombinationen aus Größe, Winkel, Härte und Seitenverhältnis verwenden. Wenn diese Kombinationen wiederverwendet werden, kann Lumi den transformierten Pinselstempel direkt aus dem Cache bereitstellen, anstatt ihn neu zu erstellen.

Das Ergebnis ist:

- schnellerer Hub-Start, nachdem ein Cache gespeichert wurde
- Reibungslosere wiederholte Verwendung von Lieblingsvoreinstellungen
- weniger verschwendete Neuberechnungen während langer Malsitzungen
- Automatische Wiederherstellung gespeicherter Caches, wenn die Voreinstellung erneut verwendet wird

## Absicht

Das Pinsel-Caching ist für Pinsel gedacht, zu denen Sie häufig zurückkehren: grundlegende Malvorgaben, bevorzugte Freihandwerkzeuge, strukturierte Trockenpinsel und andere Pinsel, deren transformierte Stempel teuer genug sind, um bemerkt zu werden.

Das Ziel besteht nicht darin, jeden theoretischen Pinselzustand vorab zu backen. Das Ziel besteht darin, dass beim echten Malgebrauch zunächst die wertvollsten Zustände gefüllt werden und dann der gefüllte Cache gespeichert wird, damit der Pinsel bei der nächsten Verwendung bereits warm ist.

## Wie es funktioniert

Das Pinsel-Caching funktioniert zusammen mit der Pinselquantisierung.

Wenn die Quantisierung für ein Dynamik-Preset aktiviert ist, werden transformierungsbeeinflussende Ausgänge auf diskrete Schritte ausgerichtet. Dadurch verfügt Lumi über einen endlichen Satz wiederverwendbarer Pinselzustände. Während Sie malen:

1. Lumi prüft, ob der transformierte Stempel bereits im Cache vorhanden ist.
2. Ist dies der Fall, wird der Stempel sofort wiederverwendet.
3. Ist dies nicht der Fall, erstellt Lumi es einmal und speichert es.
4. Mit der Zeit füllt sich der Cache mit den Pinselzuständen, die Sie tatsächlich verwenden.

Wenn Sie diesen Cache speichern, kann Lumi ihn später automatisch laden, sodass der Pinsel näher an einem aufgewärmten Zustand startet, anstatt alles von Grund auf neu aufzubauen.

## Typischer Arbeitsablauf

1. Wählen Sie eine Pinselvoreinstellung, die Sie häufig verwenden.
2. Aktivieren Sie die Quantisierung für die Dynamik.
3. Paint normally for a while so the cache fills organically.
4. Öffnen Sie den **Tool Preset Editor** und überprüfen Sie den Abschnitt **Preset Cache**.
5. Sehen Sie sich die Live-Metriken an:
   - **Trefferquote**
   - **Abdeckung**
   - **Speicher**
6. Klicken Sie auf **Speichern**, wenn der Cache sinnvoll erscheint.
7. Bei späteren Sitzungen lädt Lumi den gespeicherten Cache automatisch, wenn die Voreinstellung aktiv wird.

Dadurch fühlt sich die Voreinstellung schneller an, insbesondere bei Pinseln mit teuren Transformationen oder großen Stempeln.

## Wo es zu finden ist

### Dynamik-Editor

Verwenden Sie den **Dynamik-Editor**, um die Quantisierung zu steuern:

- Quantisierung aktivieren
- Wählen Sie die globale Schrittzahl
- Überschreiben Sie optional die Schrittanzahl pro Ausgabeachse

Quantisierung macht den Cache praktisch, indem sie die kontinuierliche Variation in wiederverwendbare Behälter reduziert.

### Werkzeugvoreinstellungseditor

Verwenden Sie den **Tool Preset Editor**, um den Cache für die aktuelle Voreinstellung zu verwalten:

- **Speichern** – speichert den aktuellen In-Memory-Cache auf der Festplatte
- **Laden** – stellt einen zuvor gespeicherten Cache wieder her
- **Freier Speicher** – Geben Sie den In-Memory-Cache frei, ohne die gespeicherte Kopie zu löschen
- **Entfernen** – den gespeicherten Cache von der Festplatte löschen

Der **Preset Cache**-Expander zeigt auch Live-Trefferrate, Abdeckung und Speichernutzung an.

## Was zwischengespeichert wird

Das Pinsel-Caching zielt auf transformierte Pinselstempel ab: Die teuren gerasterten Ergebnisse nach Größe, Winkel, Härte, Seitenverhältnis und zugehörigen Transformationseingaben wurden behoben.

Es ist am nützlichsten, wenn:- Der Pinsel erfordert kostspielige Transformationsarbeiten
- Die gleiche Voreinstellung wird über viele Sitzungen hinweg verwendet
- Der Pinsel greift immer wieder auf ähnliche dynamische Zustände zurück
- Eine schnelle Reaktionsfähigkeit beim Start ist wichtig

Es ist weniger nützlich für Pinsel, deren Transformationszustand sich stark ändert und sich selten wiederholt.

## Automatisches Laden

Gespeicherte Caches sollen von Beginn einer Sitzung an helfen und nicht erst, wenn Sie bereits eine Weile gemalt haben.

Wenn für die aktive Voreinstellung ein gespeicherter Cache vorhanden ist, kann Lumi ihn automatisch laden, sodass Ihr Lieblingspinsel mit vielen bereits verfügbaren nützlichen Zuständen startet. Das verkürzt die Kaltstartzeit und bringt die Bürste sofort näher an die höchste Reaktionsgeschwindigkeit.

## Speichersicherheit

Das Brush-Caching soll die Geschwindigkeit verbessern, ohne die Maschine zu übernehmen.

Lumi verfolgt die Cache-Speichernutzung, macht sie in der Benutzeroberfläche verfügbar und wendet Laufzeitbegrenzungen bei Speicherauslastung an. Wenn das System nicht über genügend RAM verfügt, wird das Cache-Wachstum automatisch eingeschränkt.

## Beste Anwendungsfälle

Das Pinsel-Caching eignet sich besonders gut für:

- Lieblingsbürsten für den täglichen Gebrauch
- Strukturierte Pinsel, die im gesamten Gemälde verwendet werden
- große ausdrucksstarke Pinsel mit hohem Transformationsaufwand
- Pinselvoreinstellungen, die über wiederholte Illustrationsworkflows hinweg geteilt werden
- Voreinstellungen, bei denen Sie sich „bereit“ fühlen möchten, sobald Sie sie auswählen

## Kurz gesagt

Mit dem Pinsel-Caching lernt Lumi die Pinselzustände, die Sie tatsächlich verwenden, speichert sie und ruft sie später automatisch wieder auf. Es ist eine praktische Schnelligkeitsfunktion für Lieblingsvoreinstellungen: Malen Sie mit dem Pinsel, lassen Sie den Cache füllen, speichern Sie ihn und schon starten zukünftige Sitzungen schneller.