---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Du behöver Git för det första klonsteget nedan. Om Git inte är installerat ännu, installera det först (Debian/Ubuntu: `sudo apt install git`) eller följ: [Använda Git på Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Klona Lumi (första installationen)

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

## 2) Installera beroenden (första installationen)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Bygg Lumi (första installationen)

Första fullständiga setup-bygget (första gången eller efter större ändringar):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Starta Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Valfritt: bygg om / kompilera

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

## Valfritt: byggtyper

Använd `--type` vid behov:

- `debug` – för felsökningsarbetsflöden
- `debugoptimized` – balanserad standard för utveckling
- `release` – snabbaste körtid

Exempel:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
