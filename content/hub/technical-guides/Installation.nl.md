---
title: "Installatie"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Je hebt Git nodig voor de eerste kloonstap hieronder. Als Git nog niet is geïnstalleerd, installeer het eerst (Debian/Ubuntu: `sudo apt install git`) of volg: [Git gebruiken op Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi klonen (eerste installatie)

Maak de map voor Lumi aan en gebruik Git om de broncode te klonen.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Afhankelijkheden installeren (eerste installatie)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Lumi bouwen (eerste installatie)

Eerste volledige setup-build (eerste keer of na grote wijzigingen):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Lumi starten

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Optioneel: opnieuw bouwen / compileren

Normale rebuild na codewijzigingen:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Snelle compile-only route:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Bouw één geïntegreerd onderdeel (vervang `babl` door `gegl` of `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Optioneel: buildtypes

Gebruik `--type` indien nodig:

- `debug` – voor debugworkflows
- `debugoptimized` – gebalanceerde standaard voor ontwikkeling
- `release` – snelste runtime

Voorbeeld:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
