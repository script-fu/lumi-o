---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Een AppImage is een Linux-applicatiepakket in één bestand. Je downloadt één bestand, maakt het uitvoerbaar en start het zonder software systembreed te installeren.

Officiële AppImage-site: https://appimage.org/

De AppImage biedt een draagbare versie van Lumi die werkt zonder installatie of systeemaanpassingen. Ideaal voor artiesten die de software meteen willen gebruiken zonder dependencies te beheren, broncode te compileren of een ontwikkelomgeving te configureren.

Als zelfstandig uitvoerbaar bestand kan de AppImage overal op het systeem worden opgeslagen. Zo kun je eenvoudig nieuwe releases testen, meerdere versies bewaren of de software tussen machines verplaatsen.

In het ontwikkelproces van Lumi fungeert de AppImage als een draagbare testbuild die nauw aansluit op CI-output. Zo kun je betrouwbaar testen in een consistente omgeving, terwijl lokale source builds gericht blijven op ontwikkelwerk.

Opmerking: CI bouwt de AppImage met Lumi's geïntegreerde dependency-bronnen in de repo (BABL/GEGL/GTK3), zodat de dependency-stack consistent is met de lokale `lumi-build-script.sh`-workflow.

## Release versus development AppImage

- **Release AppImage**: nog niet beschikbaar (Lumi is nog niet uitgebracht).
- **Development AppImage (CI-artefact)**: automatisch gegenereerd vanuit lopende development commits voor testen.

Deze gids behandelt vooral de workflow voor de **development AppImage**.

Huidige artefactpagina:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Basis: CI AppImage downloaden

CI levert artefact-zipbestanden (bijvoorbeeld `lumi-appimage*.zip`).

Eenvoudige handmatige stappen:

1. Download de nieuwste CI-artefact-zip.
2. Pak het uit.
3. Start het meegeleverde `Lumi*.AppImage`-bestand.

De scripts hieronder zijn optionele helpers die deze stappen automatiseren.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Optionele helper-scripts

- `lumi-appimage-unpack-zip.sh`
  - zoekt de nieuwste `lumi-appimage*.zip` in `~/Downloads`
  - installeert de AppImage op `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installeert desktopresources op `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - start de AppImage in een terminal
  - schakelt runtime-output in (`APPIMAGE_DEBUG=1`)

## Algemene opmerkingen

- Als je AppImage handmatig start (zonder helper-scripts), maak het bestand eerst uitvoerbaar:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` stelt uitvoerbare rechten al automatisch in.

- Als Lumi al draait vanuit een andere build, sluit het af voordat je de AppImage start.
