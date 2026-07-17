---
title: "GitLab CI"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
url: "hub/technical-guides/GitLab-CI"
translation_lock: true
---

Integrasi berkelanjutan (CI) adalah cara untuk menguji, membangun, dan memvalidasi kode secara otomatis setiap kali ada perubahan.

**GitLab** menyediakan fitur CI/CD bawaan melalui file `.gitlab-ci.yml`. File ini, ditempatkan di root repositori, memberi tahu GitLab cara membangun dan menguji proyek. File tersebut mendefinisikan stage dan skrip yang dijalankan di lingkungan bersih setiap kali ada perubahan.

Dokumen ini menguraikan cara kerja pipeline GitLab CI/CD Lumi, termasuk peran file `.gitlab-ci.yml`, skrip shell, dan alat eksternal seperti Meson dan Ninja.

Untuk dokumentasi teknis terperinci tentang proses build CI Lumi, lihat [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) di repositori.

## Dasar-dasar CI/CD GitLab

CI dikendalikan oleh file bernama `.gitlab-ci.yml`. File ini mendefinisikan:

- **Stage**: Urutan kelompok job (misalnya, `build-this`, `build-that`, `package-up`)
- **Job**: Tugas individual yang dijalankan dalam setiap stage
- **Skrip**: Perintah shell yang dijalankan untuk setiap job
- **Runner**: Mesin yang digunakan GitLab untuk menjalankan tugas yang ditentukan dalam pipeline

Di Lumi, stage pipeline adalah:

- `dependencies`
- `build lumi`
- `appimage`

## Build berbasis container

Pipeline Lumi menggunakan containerisasi untuk build yang konsisten:

1. **Membuat build container**: Stage pertama menggunakan Buildah untuk membuat image Docker dengan semua dependensi
2. **Menggunakan container**: Stage berikutnya dijalankan di dalam container ini, memastikan lingkungan yang konsisten
3. **Build yang dapat direproduksi**: Isolasi container menjamin hasil yang sama di berbagai runner

Pendekatan ini memastikan build berjalan dengan cara yang sama di semua runner GitLab dan menyediakan lingkungan terkendali untuk proses build yang kompleks.

### Sumber dependensi terintegrasi

Image dependensi CI Lumi membangun stack bercabang dari **sumber terintegrasi dalam repo** (bukan klon eksternal):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Direktori ini disalin ke konteks build container dan dikompilasi ke dalam prefix dependensi (biasanya `/opt/lumi-deps`). Hal ini menjaga CI tetap dapat direproduksi dan memastikan build AppImage menggunakan sumber kebenaran yang sama dengan pengembangan lokal.

## Peran skrip shell

Job di `.gitlab-ci.yml` biasanya memanggil perintah shell secara langsung. Operasi kompleks sering dipindahkan ke skrip terpisah yang disimpan di repositori.

CI Lumi menggunakan skrip shell modular untuk mengatur logika build:

**Contoh pemanggilan skrip:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Manfaat pendekatan ini:**
- **YAML tetap bersih**: Menjaga file `.gitlab-ci.yml` fokus pada struktur job
- **Mudah dirawat**: Logika kompleks lebih mudah di-debug dan dimodifikasi dalam skrip shell
- **Dapat digunakan kembali**: Skrip dapat digunakan dalam konteks atau lingkungan berbeda
- **Modularitas**: Berbagai aspek build dapat dipisahkan menjadi skrip terfokus

Hal ini menjaga konfigurasi CI tetap bersih sambil memungkinkan proses build yang canggih.

## Integrasi dengan sistem build

Lumi menggunakan **Meson** dan **Ninja** untuk menyiapkan dan membangun kode.

Misalnya:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Di sini:

- `meson setup` menyiapkan direktori build dan menghasilkan `build.ninja`
- `ninja` menjalankan perintah build seperti yang ditentukan

## Struktur sistem build Meson

Sistem build **Meson** menggunakan file root `meson.build` yang ditempatkan di direktori root proyek. File ini mendefinisikan konfigurasi build tingkat atas dan titik masuk untuk proses build.

- Root `meson.build` biasanya terletak di direktori yang sama dengan `.gitlab-ci.yml`
- Dari situ, berkas turunan dibaca secara rekursif ke subdirektori, yang masing-masing mungkin memiliki file `meson.build` sendiri
- File subdirektori ini menentukan target, sumber, dependensi, dan instruksi build yang relevan dengan direktori tersebut

## Variabel lingkungan

Variabel kunci dalam pipeline Lumi meliputi:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variabel khusus job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Variabel ini mengontrol perilaku build dan memastikan konsistensi di berbagai stage dan runner.

## Contoh struktur

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

Dalam struktur ini:

- File root `meson.build` mengonfigurasi lingkungan build secara keseluruhan
- File subdirektori `meson.build` menangani detail kompilasi untuk komponen atau modul tertentu
- Tata letak hierarki ini menjaga logika build tetap modular dan mudah dirawat

## Artefak antar stage

Artefak adalah file yang dihasilkan job dan diperlukan pada stage berikutnya:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stage dan dependensi pipeline

Pipeline Lumi terdiri dari tiga stage utama:

1. **Dependensi**: Membuat lingkungan build dalam container dengan semua alat dan pustaka yang diperlukan
2. **Build Lumi**: Mengompilasi Lumi menggunakan Meson dan Ninja di lingkungan yang telah disiapkan
3. **AppImage**: Mengemas aplikasi yang dibangun ke dalam format AppImage yang dapat didistribusikan

**Dependensi stage:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Setiap stage berjalan hanya setelah dependensinya berhasil diselesaikan, memastikan urutan build yang tepat dan ketersediaan artefak.

## Nama job saat ini

`.gitlab-ci.yml` Lumi saat ini mendefinisikan nama job berikut:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Ringkasan

- `.gitlab-ci.yml` mendefinisikan struktur dan logika pipeline
- Job berisi perintah shell atau skrip eksternal
- Alat seperti Meson dan Ninja digunakan di dalam job sebagai bagian dari proses build

Lumi menggunakan GitLab CI untuk secara otomatis membangun AppImage untuk platform berbasis Debian. Alur ini membangun dependensi, mengompilasi Lumi, lalu mengemas AppImage.

Untuk detail tingkat sumber, gunakan:

- `.gitlab-ci.yml` di root repositori Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Untuk detail teknis komprehensif tentang proses build CI Lumi, termasuk pengaturan lingkungan, arsitektur skrip, dan pemecahan masalah, lihat [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
