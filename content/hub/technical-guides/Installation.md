---
title: "Installation"
type: docs
url: "hub/technical-guides/Installation"
---

You need Git for the initial clone step below. If Git isn’t installed yet, install it first (Debian/Ubuntu: `sudo apt install git`) or follow: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clone Lumi (first-time setup)

Make the directory for Lumi and use Git to clone the source code.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Install Dependencies (first-time setup)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Build Lumi (first-time setup)

First full setup build (first time or after major changes):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Launch Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Optional: Rebuild / Compile

Normal rebuild after code changes:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Quick compile-only path:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Build a single integrated component (replace `babl` with `gegl` or `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Optional: Build Types

Use `--type` when needed:

- `debug` – debugging workflows
- `debugoptimized` – balanced default for development
- `release` – fastest runtime

Example:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
