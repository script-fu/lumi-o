---
title: "Instalacja"
type: docs
---
Do wykonania poniższego początkowego kroku klonowania potrzebujesz Gita. Jeśli Git nie jest jeszcze zainstalowany, zainstaluj go najpierw (Debian/Ubuntu: `sudo apt install git`) lub wykonaj polecenie: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Klonuj Lumi (pierwsza konfiguracja)

Utwórz katalog dla Lumi i użyj Gita do sklonowania kodu źródłowego.

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

## Opcjonalnie: Przebuduj/Skompiluj

Normalna przebudowa po zmianach kodu:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Ścieżka do szybkiej kompilacji:

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

- `debug` – debugowanie przepływów pracy
- `debugoptimized` – zrównoważona wartość domyślna dla rozwoju
- `release` – najszybszy czas działania

Przykład:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```