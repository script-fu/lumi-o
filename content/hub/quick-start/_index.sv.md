---
title: "Snabbstart"
type: docs
---
Lumi är inte släppt än, den finns tillgänglig som en utvecklingsversion.

Om du redan är på Linux och vill köra Lumi snabbt, använd den senaste **utvecklingen AppImage** från GitLab-artefakter:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Ladda ner den senaste utvecklingen av AppImage artifact zip.
2. Dra ut blixtlåset.
3. Dubbelklicka på filen `Lumi*.AppImage` för att köra den.

AppImage bör redan vara körbar. Om det inte är det, aktivera **Tillåt körning av fil som program** i filens behörigheter, eller använd terminalmetoden nedan.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Wacom-installation på Linux

För digital målning i Lumi är en enkel **linjär tryckinställning** vanligtvis bäst:

- Håll surfplattans tryckkurva linjär.
- Håll tryck/ingångskurvor i Lumi linjära.
- Forma känslan med själva borsten, eftersom borstdynamiken kan vara icke-linjär.

Du kan kontrollera och återställa Linux-drivrutinskurvan med:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Praktiska tips:

- Lumi blockerar för närvarande den problematiska Wacom pad/touch-ring-ingången för att undvika X11-fel. Kartlägg surfplattans knappar till **relativ** penselstorlek upp/ner istället.
- Om det inte fungerar att dra i penselstorlek med `Alt`, kanske ditt skrivbord använder `Alt` för att flytta fönster. Ändra genvägen för fönsterhanteraren till `Super` eller inaktivera den.

If you want to work from source code, go to [Technical Guides](/hub/technical-guides/) and [Installation](/hub/technical-guides/Installation/).