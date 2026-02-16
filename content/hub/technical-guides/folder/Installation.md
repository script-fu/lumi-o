---
title: "Installation"
type: docs
url: "hub/technical-guides/folder/Installation"
---

This guide uses the current Lumi build scripts in:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Install Dependencies (first-time setup)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Build Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Launch Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Build Types

Use `--type` when needed:

- `debug` – debugging workflows
- `debugoptimized` – balanced default for development
- `release` – fastest runtime

Example:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
