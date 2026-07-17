---
title: "AppImage"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
url: "hub/technical-guides/AppImage"
translation_lock: true
---

AppImage adalah paket aplikasi Linux berupa satu file. Anda mengunduh satu file, menandainya sebagai executable, dan menjalankannya tanpa menginstal perangkat lunak di seluruh sistem.

Situs resmi AppImage: https://appimage.org/

AppImage menyediakan versi portabel Lumi yang berjalan tanpa instalasi atau modifikasi sistem. Ini ideal bagi seniman yang ingin segera menggunakan perangkat lunak tanpa mengelola dependensi, mengompilasi kode sumber, atau mengonfigurasi lingkungan pengembangan.

Sebagai executable mandiri, AppImage dapat disimpan di mana saja di sistem. Hal ini memudahkan pengujian rilis baru, menyimpan beberapa versi, atau memindahkan perangkat lunak antar mesin.

Dalam proses pengembangan Lumi, AppImage berfungsi sebagai build uji portabel yang selaras dengan keluaran integrasi berkelanjutan. Hal ini memungkinkan pengujian andal dalam lingkungan yang konsisten sambil menjaga build sumber lokal fokus pada pekerjaan pengembangan.

Catatan: CI membangun AppImage menggunakan sumber dependensi terintegrasi dalam repo Lumi (BABL/GEGL/GTK3), sehingga stack dependensi konsisten dengan alur kerja lokal `lumi-build-script.sh`.

## AppImage rilis vs AppImage pengembangan

- **AppImage rilis**: belum tersedia (Lumi belum dirilis).
- **AppImage pengembangan (artefak CI)**: dihasilkan otomatis dari commit pengembangan yang sedang berlangsung untuk pengujian.

Panduan ini terutama mencakup alur kerja **AppImage pengembangan**.

Halaman artefak saat ini:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Dasar-dasar pengunduhan AppImage CI

CI menghasilkan file zip artefak (misalnya `lumi-appimage*.zip`).

Alur manual dasar:

1. Unduh zip artefak CI terbaru.
2. Ekstrak.
3. Jalankan file `Lumi*.AppImage` yang disertakan.

Skrip di bawah ini adalah pembantu opsional yang mengotomatiskan langkah-langkah ini.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Ekstrak zip CI terbaru dari ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Luncurkan AppImage dengan keluaran terminal
bash lumi-appimage-launch.sh
```

## Skrip pembantu opsional

- `lumi-appimage-unpack-zip.sh`
  - menemukan `lumi-appimage*.zip` terbaru di `~/Downloads`
  - menginstal AppImage ke `~/AppImage/Lumi/Lumi_CI.AppImage`
  - memasang sumber daya desktop ke `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - meluncurkan AppImage di terminal
  - mengaktifkan keluaran runtime (`APPIMAGE_DEBUG=1`)

## Catatan umum

- Jika Anda menjalankan AppImage secara manual (tanpa skrip pembantu), jadikan AppImage executable terlebih dahulu:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` sudah menerapkan izin executable secara otomatis.

- Jika Lumi sudah berjalan dari build lain, tutup sebelum meluncurkan AppImage.
