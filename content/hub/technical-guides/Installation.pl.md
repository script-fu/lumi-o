---
title: "Instalacja"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Do wykonania poniższego początkowego klonowania potrzebujesz Gita. Jeśli Git nie jest jeszcze zainstalowany, zainstaluj go najpierw (Debian/Ubuntu: `sudo apt install git`) lub postępuj zgodnie z: [Git w systemie Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Sklonuj Lumi (pierwsza konfiguracja)

Utwórz katalog dla Lumi i użyj Gita, aby sklonować kod źródłowy.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Zainstaluj zależności (pierwsza konfiguracja)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Zbuduj Lumi (pierwsza konfiguracja)

Pierwsza pełna konfiguracja (pierwszy raz lub po większych zmianach):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Uruchom Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opcjonalnie: przebudowa / kompilacja

Normalna przebudowa po zmianach w kodzie:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Szybka ścieżka tylko do kompilacji:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Zbuduj pojedynczy zintegrowany komponent (zamień `babl` na `gegl` lub `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opcjonalnie: typy kompilacji

W razie potrzeby użyj `--type`:

- `debug` – do debugowania
- `debugoptimized` – zrównoważona wartość domyślna na czas rozwoju
- `release` – najszybszy czas wykonywania

Przykład:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
