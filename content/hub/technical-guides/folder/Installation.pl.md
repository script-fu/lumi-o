---
title: "Instalacja"
type: docs
url: "hub/technical-guides/folder/Installation"
---
W tym przewodniku wykorzystano aktualne skrypty kompilacji Lumi w:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Zainstaluj zależności (pierwsza konfiguracja)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Zbuduj Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Uruchom Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Typy kompilacji

W razie potrzeby użyj `--type`:

- `debug` – debugowanie przepływów pracy
- `debugoptimized` – zrównoważona wartość domyślna dla rozwoju
- `release` – najszybszy czas działania

Przykład:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```