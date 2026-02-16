---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
---
En AppImage är ett Linux-applikationspaket med en fil. Du laddar ner en fil, markerar den som körbar och kör den utan att installera programvara i hela systemet.

Officiell AppImage-webbplats: https://appimage.org/

AppImage tillhandahåller en bärbar version av Lumi som körs utan installation eller systemändringar. Den är idealisk för artister som vill använda programvaran omedelbart utan att hantera beroenden, kompilera källkod eller konfigurera en utvecklingsmiljö.

Som en fristående körbar fil kan AppImage lagras var som helst på systemet. Detta gör det enkelt att testa nya versioner, behålla flera versioner eller flytta programvaran mellan maskiner.

För Lumis utvecklingsprocess fungerar AppImage som en bärbar testbyggnad som nära matchar kontinuerliga integrationsutdata. Detta möjliggör tillförlitlig testning i en konsekvent miljö samtidigt som lokala källbyggen hålls fokuserade på utvecklingsarbete.

## Release vs Development AppImage

- **Släpp AppImage**: inte tillgänglig ännu (Lumi har inte släppts).
- **Utvecklingsappbild (CI-artefakt)**: genereras automatiskt från pågående utvecklingsbeslut för testning.

Den här guiden täcker huvudsakligen arbetsflödet för **utveckling av AppImage**.

Aktuell artefaktsida:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Grunderna för nedladdning av CI AppImage

CI producerar artefakt-zip-filer (till exempel `lumi-appimage*.zip`).

Grundläggande manuellt flöde:

1. Ladda ner den senaste CI artefakt zip.
2. Extrahera den.
3. Kör den medföljande filen `Lumi*.AppImage`.

Skripten nedan är valfria hjälpare som automatiserar dessa steg.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Valfria hjälpskript

- `lumi-appimage-unpack-zip.sh`
  - hittar den senaste `lumi-appimage*.zip` i `~/Downloads`
  - installerar AppImage till `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installerar skrivbordsresurser till `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - startar AppImage i en terminal
  - aktiverar runtime-utgång (`APPIMAGE_DEBUG=1`)

## Vanliga anteckningar

- Om du kör AppImage manuellt (utan hjälpskript), gör det först körbart:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` tillämpar redan körbara behörigheter automatiskt.

- Om Lumi redan körs från en annan version, stäng den innan du startar AppImage.