---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

En AppImage är ett Linux-applikationspaket i en enda fil. Du laddar ner en fil, markerar den som körbar och kör den utan att installera programvara systemomfattande.

Officiell AppImage-webbplats: https://appimage.org/

AppImage tillhandahåller en bärbar version av Lumi som körs utan installation eller systemändringar. Den är idealisk för artister som vill använda programvaran direkt utan att hantera beroenden, kompilera källkod eller konfigurera en utvecklingsmiljö.

Som fristående körbar fil kan AppImage lagras var som helst i systemet. Det gör det enkelt att testa nya versioner, behålla flera versioner eller flytta programvaran mellan datorer.

I Lumis utvecklingsprocess fungerar AppImage som en bärbar testbyggnad som nära matchar CI-utdata. Det möjliggör tillförlitlig testning i en konsekvent miljö samtidigt som lokala källbyggen hålls fokuserade på utvecklingsarbete.

Obs: CI bygger AppImage med Lumis integrerade beroendekällor i repot (BABL/GEGL/GTK3), så beroendestacken överensstämmer med det lokala arbetsflödet `lumi-build-script.sh`.

## Release vs. development AppImage

- **Release AppImage**: inte tillgänglig ännu (Lumi har inte släppts).
- **Development AppImage (CI-artefakt)**: genereras automatiskt från pågående utvecklingscommits för testning.

Den här guiden täcker främst arbetsflödet för **development AppImage**.

Aktuell artefaktsida:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Grunderna för nedladdning av CI AppImage

CI producerar artefakt-zipfiler (till exempel `lumi-appimage*.zip`).

Grundläggande manuellt flöde:

1. Ladda ner den senaste CI-artefakt-zipfilen.
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
  - aktiverar runtime-utdata (`APPIMAGE_DEBUG=1`)

## Allmänna anmärkningar

- Om du kör AppImage manuellt (utan hjälpskript), gör den körbar först:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` tillämpar redan körbara behörigheter automatiskt.

- Om Lumi redan körs från en annan build, stäng den innan du startar AppImage.
