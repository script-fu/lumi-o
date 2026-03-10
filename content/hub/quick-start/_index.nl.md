---
title: "Snel beginnen"
type: docs
---
Lumi is nog niet uitgebracht, het is beschikbaar als ontwikkelingsversie.

Als je al Linux gebruikt en Lumi snel wilt gebruiken, gebruik dan de nieuwste **ontwikkeling AppImage** van GitLab-artefacten:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Download de nieuwste AppImage-artefact-zip.
2. Pak de ritssluiting uit.
3. Dubbelklik op het bestand `Lumi*.AppImage` om het uit te voeren.

De AppImage zou al uitvoerbaar moeten zijn. Als dit niet het geval is, schakelt u **Uitvoeren van bestand als programma toestaan** in de bestandsrechten in, of gebruikt u de onderstaande terminalmethode.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Wacom-installatie op Linux

Voor digitaal schilderen in Lumi is een eenvoudige **lineaire drukinstelling** meestal het beste:

- Houd de drukcurve van de tabletdriver lineair.
- Houd de druk-/ingangscurven in Lumi lineair.
- Vorm het gevoel met de borstel zelf, omdat de penseeldynamiek niet-lineair kan zijn.

U kunt de Linux-stuurprogrammacurve controleren en opnieuw instellen met:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Praktische tips:

- Lumi blokkeert momenteel de problematische Wacom-pad/touch-ring-invoer om X11-storingen te voorkomen. Wijs tabletknoppen in plaats daarvan toe aan **relatieve** penseelgrootte omhoog/omlaag.
- Als het slepen op penseelgrootte met `Alt` niet werkt, gebruikt uw bureaublad mogelijk `Alt` om vensters te verplaatsen. Wijzig de snelkoppeling voor vensterbeheer in `Super` of schakel deze uit.

Als je vanuit de broncode wilt werken, ga dan naar [Technical Guides](/hub/technical-guides/) en [Installation](/hub/technical-guides/Installation/).