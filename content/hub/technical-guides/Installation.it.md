---
title: "Installazione"
type: docs
---
Hai bisogno di Git per il passaggio iniziale del clone di seguito. Se Git non è ancora installato, installalo prima (Debian/Ubuntu: `sudo apt install git`) o segui: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (prima configurazione)

Crea la directory per Lumi e usa Git per clonare il codice sorgente.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Installa le dipendenze (prima configurazione)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Costruisci Lumi (prima configurazione)

Prima build di configurazione completa (prima volta o dopo modifiche importanti):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Avvia Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opzionale: ricostruire/compilare

Ricostruzione normale dopo le modifiche al codice:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Percorso rapido di sola compilazione:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Costruisci un singolo componente integrato (sostituisci `babl` con `gegl` o `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Facoltativo: tipi di build

Usa `--type` quando necessario:

- `debug` – debug dei flussi di lavoro
- `debugoptimized` – impostazione predefinita bilanciata per lo sviluppo
- `release` – autonomia più veloce

Esempio:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```