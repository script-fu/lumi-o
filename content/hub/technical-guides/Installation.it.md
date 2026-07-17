---
title: "Installazione"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Ti serve Git per il passaggio iniziale di clonazione indicato di seguito. Se Git non è ancora installato, installalo prima (Debian/Ubuntu: `sudo apt install git`) oppure consulta: [Usare Git su Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clonare Lumi (prima configurazione)

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

## 2) Installare le dipendenze (prima configurazione)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Compilare Lumi (prima configurazione)

Prima build completa di configurazione (la prima volta o dopo modifiche importanti):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Avviare Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Facoltativo: ricompilare / compilare

Ricompilazione normale dopo modifiche al codice:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Percorso rapido di sola compilazione:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Compilare un singolo componente integrato (sostituisci `babl` con `gegl` o `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Facoltativo: tipi di build

Usa `--type` quando necessario:

- `debug` – flussi di lavoro di debug
- `debugoptimized` – impostazione predefinita bilanciata per lo sviluppo
- `release` – massime prestazioni in runtime

Esempio:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
