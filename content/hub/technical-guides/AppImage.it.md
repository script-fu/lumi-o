---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Un AppImage è un pacchetto applicativo Linux in un singolo file. Scarichi un file, lo contrassegni come eseguibile e lo avvii senza installare software a livello di sistema.

Sito ufficiale AppImage: https://appimage.org/

L'AppImage fornisce una versione portatile di Lumi che funziona senza installazione né modifica del sistema. È ideale per gli artisti che vogliono usare subito il software senza gestire dipendenze, compilare codice sorgente o configurare un ambiente di sviluppo.

Essendo un eseguibile autonomo, l'AppImage può essere archiviato ovunque nel sistema. Ciò semplifica il test di nuove release, il mantenimento di più versioni o lo spostamento del software tra macchine.

Nel processo di sviluppo di Lumi, l'AppImage funge da build di test portatile che corrisponde da vicino all'output dell'integrazione continua. Ciò consente test affidabili in un ambiente coerente, mantenendo le build locali concentrate sul lavoro di sviluppo.

Nota: CI compila l'AppImage usando le sorgenti delle dipendenze integrate nel repository di Lumi (BABL/GEGL/GTK3), quindi lo stack delle dipendenze è coerente con il flusso di lavoro locale `lumi-build-script.sh`.

## AppImage release e AppImage di sviluppo

- **Release AppImage**: non ancora disponibile (Lumi non è stato rilasciato).
- **AppImage di sviluppo (artefatto CI)**: generato automaticamente dai commit di sviluppo in corso per i test.

Questa guida copre principalmente il flusso di lavoro dell'**AppImage di sviluppo**.

Pagina degli artefatti attuale:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Nozioni di base sul download dell'AppImage CI

CI produce file zip di artefatti (ad esempio `lumi-appimage*.zip`).

Flusso manuale di base:

1. Scarica l'ultimo zip di artefatti CI.
2. Estrailo.
3. Esegui il file `Lumi*.AppImage` incluso.

Gli script seguenti sono helper opzionali che automatizzano questi passaggi.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Script di supporto opzionali

- `lumi-appimage-unpack-zip.sh`
  - trova l'ultimo `lumi-appimage*.zip` in `~/Downloads`
  - installa l'AppImage in `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installa le risorse desktop in `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - avvia l'AppImage in un terminale
  - abilita l'output runtime (`APPIMAGE_DEBUG=1`)

## Note comuni

- Se esegui l'AppImage manualmente (senza script di supporto), rendilo prima eseguibile:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` applica già automaticamente i permessi di esecuzione.

- Se Lumi è già in esecuzione da un'altra build, chiudilo prima di avviare l'AppImage.
