---
title: "Installatie"
type: docs
---
Je hebt Git nodig voor de eerste kloonstap hieronder. Als Git nog niet is geïnstalleerd, installeer het dan eerst (Debian/Ubuntu: `sudo apt install git`) of volg: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Lumi klonen (eerste installatie)

Maak de map voor Lumi en gebruik Git om de broncode te klonen.

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

## 4) Start Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Optioneel: opnieuw opbouwen / compileren

Normaal opnieuw opbouwen na codewijzigingen:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Snel alleen-compileren pad:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Bouw één geïntegreerd onderdeel (vervang `babl` door `gegl` of `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Optioneel: buildtypen

Gebruik `--type` indien nodig:

- `debug` – foutopsporing in workflows
- `debugoptimized` – evenwichtige standaard voor ontwikkeling
- `release` – snelste looptijd

Voorbeeld:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```