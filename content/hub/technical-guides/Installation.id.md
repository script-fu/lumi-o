---
title: "Instalasi"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
url: "hub/technical-guides/Installation"
translation_lock: true
---

Anda memerlukan Git untuk langkah kloning awal di bawah ini. Jika Git belum terinstal, instal terlebih dahulu (Debian/Ubuntu: `sudo apt install git`) atau ikuti: [Menggunakan Git di Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Klon Lumi (pengaturan pertama kali)

Buat direktori untuk Lumi dan gunakan Git untuk mengkloning kode sumber.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Klon via SSH (sesuai panduan Git di atas)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Atau klon via HTTPS (tanpa pengaturan kunci SSH)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev

```

## 2) Instal dependensi (pengaturan pertama kali)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Build Lumi (pengaturan pertama kali)

Build setup penuh pertama (pertama kali atau setelah perubahan besar):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Luncurkan Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opsional: Rebuild / kompilasi

Rebuild rutin setelah perubahan kode:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Jalur kompilasi cepat saja:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Build satu komponen terintegrasi (ganti `babl` dengan `gegl` atau `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opsional: Tipe build

Gunakan `--type` bila diperlukan:

- `debug` – alur kerja debugging
- `debugoptimized` – default seimbang untuk pengembangan
- `release` – waktu proses tercepat

Contoh:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
