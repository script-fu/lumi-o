---
title: "AppImage"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
Un AppImage è un pacchetto di applicazioni Linux a file singolo. Si scarica un file, lo si contrassegna come eseguibile e lo si esegue senza installare software a livello di sistema.

Sito ufficiale AppImage: https://appimage.org/

AppImage fornisce una versione portatile di Lumi che funziona senza installazione o modifica del sistema. È ideale per gli artisti che desiderano utilizzare immediatamente il software senza gestire dipendenze, compilare codice sorgente o configurare un ambiente di sviluppo.

Essendo un eseguibile autonomo, AppImage può essere archiviato ovunque nel sistema. Ciò semplifica il test di nuove versioni, il mantenimento di più versioni o lo spostamento del software tra computer.

Per il processo di sviluppo di Lumi, AppImage funziona come una build di test portatile che corrisponde strettamente all’output dell’integrazione continua. Ciò consente test affidabili in un ambiente coerente mantenendo le build di origine locali focalizzate sul lavoro di sviluppo.

## Rilascio e sviluppo dell'AppImage

- **Rilascio di AppImage**: non ancora disponibile (Lumi non è stato rilasciato).
- **Development AppImage (artefatto CI)**: generato automaticamente dagli impegni di sviluppo in corso per i test.

Questa guida copre principalmente il flusso di lavoro di **sviluppo di AppImage**.

Pagina dell'elemento corrente:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Nozioni di base sul download di CI AppImage

CI produce file zip degli artefatti (ad esempio `lumi-appimage*.zip`).

Flusso manuale di base:

1. Scarica l'ultimo file zip dell'artefatto CI.
2. Estrarlo.
3. Eseguire il file `Lumi*.AppImage` incluso.

Gli script seguenti sono aiutanti facoltativi che automatizzano questi passaggi.

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
  - installa AppImage su `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installa le risorse desktop su `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - avvia l'AppImage in un terminale
  - abilita l'output runtime (`APPIMAGE_DEBUG=1`)

## Note comuni

- Se esegui AppImage manualmente (senza script di supporto), rendilo prima eseguibile:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` applica già automaticamente le autorizzazioni eseguibili.

- Se Lumi è già in esecuzione da un'altra build, chiudila prima di avviare AppImage.