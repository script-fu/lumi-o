---
title: "Installation"
type: docs
---
Du behöver Git för det första klonsteget nedan. Om Git inte är installerat ännu, installera det först (Debian/Ubuntu: `sudo apt install git`) eller följ: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (förstagångsinstallation)

Skapa katalogen för Lumi och använd Git för att klona källkoden.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Installera beroenden (förstagångsinstallation)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Bygg Lumi (förstagångsinstallation)

Första kompletta installationsbygget (första gången eller efter större ändringar):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Starta Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Valfritt: Bygg om/kompilera

Normal ombyggnad efter kodändringar:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Snabb kompileringsväg:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Bygg en enda integrerad komponent (ersätt `babl` med `gegl` eller `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Valfritt: Byggtyper

Använd `--type` vid behov:

- `debug` – felsökning av arbetsflöden
- `debugoptimized` – balanserad standard för utveckling
- `release` – snabbaste körtiden

Exempel:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```