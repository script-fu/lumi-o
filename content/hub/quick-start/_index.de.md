---
title: "Schnellstart"
type: docs
---
Lumi ist noch nicht veröffentlicht, es ist als Entwicklungsversion verfügbar.

Wenn Sie bereits Linux verwenden und Lumi schnell ausführen möchten, verwenden Sie das neueste **Entwicklungs-AppImage** von GitLab-Artefakten:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Laden Sie die neueste AppImage-Artefakt-ZIP-Datei herunter.
2. Ziehen Sie den Reißverschluss heraus.
3. Doppelklicken Sie auf die Datei `Lumi*.AppImage`, um sie auszuführen.

Das AppImage sollte bereits lauffähig sein. Ist dies nicht der Fall, aktivieren Sie **Ausführen der Datei als Programm zulassen** in den Berechtigungen der Datei oder verwenden Sie die unten stehende Terminalmethode.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Wacom-Setup unter Linux

Für digitales Malen in Lumi ist normalerweise ein einfaches **Lineardruck-Setup** am besten:

- Halten Sie die Druckkurve des Tablet-Treibers linear.
- Halten Sie die Druck-/Eingangskurven in Lumi linear.
- Gestalten Sie das Gefühl mit dem Pinsel selbst, da die Pinseldynamik nichtlinear sein kann.

Sie können die Linux-Treiberkurve überprüfen und zurücksetzen mit:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Praktische Tipps:

- Lumi blockiert derzeit die problematische Wacom-Pad-/Touchring-Eingabe, um X11-Störungen zu vermeiden. Ordnen Sie die Tablet-Tasten stattdessen der **relativen** Pinselgröße nach oben/unten zu.
- Wenn das Ziehen der Pinselgröße mit `Alt` nicht funktioniert, verwendet Ihr Desktop möglicherweise `Alt` zum Verschieben von Fenstern. Ändern Sie diese Fenstermanager-Verknüpfung in `Super` oder deaktivieren Sie sie.

Wenn Sie mit dem Quellcode arbeiten möchten, gehen Sie zu [Technical Guides](/hub/technical-guides/) und [Installation](/hub/technical-guides/Installation/).