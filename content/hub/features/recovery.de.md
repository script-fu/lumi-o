---
title: "Dateiwiederherstellung"
type: docs
---
Lumi unterhält zwei unabhängige Wiederherstellungssysteme – automatische Hintergrundspeicherungen und manuelle inkrementelle Prüfpunkte – beide sind über einen einzigen Dialog zugänglich.

## Zugriff

**Datei** → **Bild wiederherstellen**

Das Dialogfeld wird mit den Wiederherstellungsstatus für die aktuell geöffnete Datei vorab gefüllt geöffnet. Verwenden Sie die Dateiauswahl oben, um zu einer anderen `.lum` Datei zu wechseln.

---

## Automatisch speichern

Lumi speichert während der Bearbeitung in regelmäßigen Abständen einen Hintergrund-Schnappschuss Ihrer Arbeit. Automatische Speicherungen werden in ein **separates Cache-Verzeichnis** geschrieben, sodass die funktionierende `.lum`-Datei unberührt bleibt:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

Die Pfadkodierung verwendet `~` als Trennzeichen, um ein eindeutiges Cache-Verzeichnis pro Datei zu erstellen. Dies bedeutet, dass automatische Speicherungen auch dann verfügbar sind, wenn die Projektdatei selbst verloren geht oder beschädigt ist.

- **Häufigkeit**: Konfigurierbar unter **Bearbeiten** → **Einstellungen** → **Leistung** → Intervall für automatische Speicherung.
- **Speicherort**: Auch unter Einstellungen → Leistung festgelegt.
- **Zweck**: Wiederherstellung nach einem Absturz. Auf der Registerkarte „Automatische Speicherung“ im Dialogfeld „Bild wiederherstellen“ werden die verfügbaren Status der automatischen Speicherung mit Zeitstempel angezeigt.

Wenn Sie eine Datei öffnen, die über neuere Autosave-Daten verfügt, benachrichtigt Sie Lumi zum Zeitpunkt des Öffnens.

---

## Inkrementelle Speicherungen

Beim inkrementellen Speichern handelt es sich um ein manuelles Prüfpunktsystem, das **in der Projektdatei** unter `recovery/` gespeichert wird. Die Struktur ist:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Eine neue `primary-NN.lum/` Baseline wird nach **Datei → Speichern** geschrieben. Durch nachfolgendes Drücken von Strg+S werden `delta-NNNN.lum/` Unterverzeichnisse erstellt, die nur die Puffer enthalten, die sich seit der letzten Baseline geändert haben. Autosave-Deltas und manuelle Save-Deltas verwenden separate Zähler, sodass sie den Verlauf des anderen nicht beeinträchtigen.

Inkrementelle Speicherungen sind **standardmäßig deaktiviert** und müssen pro Projekt aktiviert werden:

1. **Datei** → **Speichern unter** (Umschalt+Strg+S).
2. Aktivieren Sie im Dialogfeld „Speichern unter“ die Option **Inkrementelles Speichern** und legen Sie optional ein Limit für **Max. Speicherungen** fest.
3. Die Einstellung wird mit dem Projekt gespeichert und gilt für alle nachfolgenden Tastenkombinationen Strg+S.

Wenn Sie eine `.lum`-Datei öffnen, die über neuere inkrementelle Speicherungen als die primäre Speicherung verfügt, zeigt Lumi die Eingabeaufforderung **Inkrementelle Speicherung erkannt** an und bietet an, den neuesten Prüfpunkt zu laden.

---

## Dialogfeld „Bild wiederherstellen“.

Der Dialog verfügt über drei Registerkarten und zwei Aktionsschaltflächen.

### Registerkarte „Automatisch speichern“.

Listet alle verfügbaren Autosave-Status für die ausgewählte Datei mit Zeitstempeln und Miniaturansichten (sofern verfügbar) auf. Wählen Sie einen Status aus und klicken Sie auf **Wiederherstellen**, um ihn zu öffnen.

Verwenden Sie diese Registerkarte, um:
- Wiederherstellung nach einem Absturz.
- Kehren Sie aus derselben Sitzung zu einem früheren Zustand zurück.

### Inkrementelle Registerkarte

Listet alle in der Projektdatei gespeicherten Prüfpunktzustände auf. Jeder Eintrag zeigt den Checkpoint-Zeitstempel. Wählen Sie einen Prüfpunkt aus und klicken Sie auf **Wiederherstellen**, um ihn zu öffnen.

Verwenden Sie diese Registerkarte, um:
- Kehren Sie zu einem früheren Punkt in einer Sitzung zurück, ohne separate Dateien gespeichert zu haben.
- Durchsuchen Sie den Versionsverlauf eines Projekts.

### Neuester Tab

Die Standardregisterkarte, wenn das Dialogfeld geöffnet wird. Identifiziert automatisch den neuesten verfügbaren Wiederherstellungsstatus sowohl bei automatischen Speicherungen als auch bei inkrementellen Prüfpunkten und zeigt seinen Zeitstempel an. Klicken Sie auf **Wiederherstellen**, um es sofort zu laden, ohne einzelne Status durchsuchen zu müssen.

---

## Schaltflächen

| Knopf | Aktion |
|--------|--------|
| **Wiederherstellen** | Öffnet den ausgewählten Wiederherstellungsstatus als neues Bild. |
| **Schließen** | Schließt den Dialog ab, ohne ihn wiederherzustellen. |
| **Alte Zustände aufräumen…** | Öffnet eine Bereinigungsaufforderung (siehe unten). |

---

## Bereinigen Sie alte ZuständeDie Anhäufung von Wiederherstellungszuständen im Laufe der Zeit kann erheblichen Speicherplatz beanspruchen. Die Schaltfläche **Alte Zustände bereinigen…** (unten links im Dialogfeld) öffnet eine Bereinigungsaufforderung für die aktive Registerkarte (Autosave oder Inkrementell).

Die Eingabeaufforderung zeigt:
– Wie viele vollständige Speicherungen gibt es für die Datei?
– Der gesamte Speicherplatz, den sie belegen.
- Eine Drehschaltfläche **Neueste behalten**, um auszuwählen, wie viele Speicherungen beibehalten werden sollen.

Wenn Sie **Neueste beibehalten** auf `0` setzen, werden alle Wiederherstellungsstatus gelöscht. Mit der nächsten Strg+S-Taste nach einer vollständigen Bereinigung wird ein neuer Primärspeicher erstellt.

---

## Startwiederherstellung

Wenn Lumi beim Start erkennt, dass die zuletzt geöffnete Datei neuere automatische Speicherdaten enthält als die letzte vollständige Speicherung, wird vor dem Laden eine Wiederherstellungsaufforderung angezeigt. Sie können akzeptieren (die automatische Speicherung laden) oder ablehnen (die primäre Speicherung wie gewohnt öffnen).