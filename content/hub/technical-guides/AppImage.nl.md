---
title: "AppAfbeelding"
type: docs
url: "hub/technical-guides/AppImage"
---
Een AppImage is een Linux-applicatiepakket met één bestand. U downloadt één bestand, markeert het als uitvoerbaar bestand en voert het uit zonder software voor het hele systeem te installeren.

Officiële AppImage-site: https://appimage.org/

De AppImage biedt een draagbare versie van Lumi die werkt zonder installatie of systeemaanpassing. Het is ideaal voor artiesten die de software onmiddellijk willen gebruiken zonder afhankelijkheden te beheren, broncode te compileren of een ontwikkelomgeving te configureren.

Als een op zichzelf staand uitvoerbaar bestand kan de AppImage overal op het systeem worden opgeslagen. Dit maakt het eenvoudig om nieuwe releases te testen, meerdere versies te behouden of de software tussen machines te verplaatsen.

Voor het ontwikkelingsproces van Lumi functioneert de AppImage als een draagbare testbuild die nauw aansluit bij de continue integratie-output. Dit maakt betrouwbaar testen in een consistente omgeving mogelijk, terwijl lokale bronbuilds gericht blijven op ontwikkelingswerk.

## Release versus ontwikkeling AppImage

- **Release AppImage**: nog niet beschikbaar (Lumi is nog niet uitgebracht).
- **Development AppImage (CI-artefact)**: automatisch gegenereerd op basis van doorlopende ontwikkelingsverplichtingen voor testen.

Deze handleiding behandelt hoofdzakelijk de workflow **ontwikkeling AppImage**.

Huidige artefactpagina:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Basisprincipes van het downloaden van CI AppImage

CI produceert artefact-zipbestanden (bijvoorbeeld `lumi-appimage*.zip`).

Basis handmatige stroom:

1. Download de nieuwste CI-artefact-zip.
2. Pak het uit.
3. Voer het meegeleverde `Lumi*.AppImage` bestand uit.

De onderstaande scripts zijn optionele helpers die deze stappen automatiseren.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Optionele helperscripts

- `lumi-appimage-unpack-zip.sh`
  - vindt de nieuwste `lumi-appimage*.zip` in `~/Downloads`
  - installeert AppImage op `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installeert bureaubladbronnen op `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - lanceert de AppImage in een terminal
  - maakt runtime-uitvoer mogelijk (`APPIMAGE_DEBUG=1`)

## Algemene opmerkingen

- Als u AppImage handmatig uitvoert (zonder helperscripts), maak het dan eerst uitvoerbaar:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` past uitvoerbare machtigingen al automatisch toe.

- Als Lumi al een andere build gebruikt, sluit deze dan voordat u AppImage start.