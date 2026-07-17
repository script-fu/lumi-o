---
title: "Menginstal Debian"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
url: "hub/install-linux/Installing-Debian"
translation_lock: true
---

Dokumen ini menguraikan proses yang digunakan untuk menginstal Debian Stable sebagai sistem operasi pengembangan Lumi-o. Panduan ini juga dapat berguna bagi siapa pun yang menyiapkan lingkungan serupa.

Debian Stable dipilih karena Lumi bertujuan untuk dibangun secara andal di platform jangka panjang yang dapat diprediksi. Pengembangan GIMP menargetkan Debian Testing, sehingga Debian Stable menjadi basis sistem yang selaras.

Lumi berjalan paling baik di Debian dengan Cinnamon (X11), dan dikembangkan serta diuji di lingkungan tersebut. Cinnamon menyediakan alur kerja desktop yang familiar seperti Windows, sementara X11 memberikan lingkungan paling stabil untuk pengembangan Lumi.

Jika Anda beralih dari Windows, perubahan konseptual utamanya adalah sebagian besar instalasi dan konfigurasi perangkat lunak dilakukan melalui manajer paket dan perintah terminal sederhana, bukan melalui installer yang dapat diunduh.

## Untuk siapa panduan ini

Panduan ini mendokumentasikan pengaturan Debian Stable yang digunakan untuk pengembangan Lumi. Ini bukan tutorial instalasi Linux umum.

Panduan ini paling berguna untuk:

- seniman yang beralih dari Windows dan menginginkan pengaturan Linux yang dapat diprediksi
- pengembang yang membangun Lumi dari sumber
- pengguna yang lebih memilih mereproduksi lingkungan kerja yang dikenal daripada merancang konfigurasi sistem mereka sendiri

Keakraban dasar dengan partisi disk dan penggunaan baris perintah sederhana diasumsikan.

## Cadangkan data Anda

Sebelum menginstal Debian, buat cadangan lengkap direktori Home Anda di drive eksternal. Sertakan folder data tambahan yang ingin Anda pertahankan.

Catatan: Di Linux, `~` mewakili direktori Home Anda.

Jika Anda menggunakan repositori Git, push perubahan penting apa pun ke remote-nya agar dapat dipulihkan dengan mudah setelah instalasi. Langkah ini hanya relevan jika Anda sudah menggunakan Git.

## Buat partisi

Siapkan ruang di drive utama Anda untuk Debian. Banyak panduan dan alat tersedia untuk langkah ini, termasuk GParted. Tergantung pada pengaturan Anda, Anda dapat:

- mengecilkan partisi Windows yang ada untuk dual boot
- menggunakan kembali partisi Linux yang ada
- menyiapkan partisi Linux dan swap baru

Jika Anda tidak yakin, baca panduan khusus perangkat keras sebelum melakukan perubahan, karena langkah partisi sangat bervariasi antar sistem.


## Buat USB instalasi Debian

Dengan asumsi partisi target dan ruang swap sudah ada:

1. Unduh ISO Debian dari situs resmi: https://www.debian.org/
2. Di Windows, gunakan BalenaEtcher untuk menulis ISO ke drive USB.
3. Di Linux, gunakan alat baris perintah seperti `dd` untuk membuat USB yang dapat di-boot.

## Instal Debian

1. Masukkan drive USB.
2. Mulai ulang dan tekan tombol menu boot (biasanya `F2`, `F12`, `Esc`, atau `Del`) saat startup.
3. Pilih perangkat USB.
4. Pilih installer non-grafis.
5. Biarkan kata sandi root kosong ketika diminta agar installer memberikan akses sudo ke akun pengguna Anda.
6. Partisi secara manual:

   - Sistem file: ext4 (journaling)
   - Swap: partisi swap yang ada
   - Titik mount: `/`
   - Label: `linux`
   - Hostname: nama sistem ditampilkan sebagai `user@hostname`
   - Akun pengguna: nama lengkap Anda
   - Nama pengguna: nama login terminal

7. Installer Debian menyediakan pilihan lingkungan desktop pada tahap ini; pilih **Cinnamon** untuk pengaturan yang direkomendasikan Lumi.
8. Selesaikan instalasi dan reboot ke Debian Stable.

## Pengaturan sistem

### Penskalaan tampilan

Debian Stable saat ini menangani penskalaan fraksional secara tidak konsisten, terutama pada tampilan 4K. Daripada mengurangi resolusi tampilan, sesuaikan elemen antarmuka secara langsung.

Penyesuaian yang disarankan:

- Hindari penskalaan tampilan fraksional.
- Menu → Pemilihan Font → Pengaturan Font → Faktor Penskalaan Teks: `2.5`
- Font Desktop: `14`
- Panel → Sesuaikan → Tinggi Panel: `60`
- Tampilan Panel → Ukuran Ikon Simbol Zona Kanan: `48px`
- Mouse dan Touchpad → Penyesuaian Ukuran Pointer
- Desktop (klik kanan) → Kustomisasi → Ukuran ikon lebih besar

Penyesuaian Firefox:

- Bilah alamat → `about:config`
- Setel `layout.css.devPixelsPerPx` ke `1`

### Terminal

Konfigurasikan preferensi terminal:

1. Menu → Terminal → Edit → Preferensi
2. Teks → Ukuran awal: `140 columns`, `40 rows`
3. Teks → Font khusus: `Monospace 10`
4. Warna → Skema bawaan → Solarized Dark

## Pulihkan data

Pulihkan file cadangan ke direktori Home sesuai kebutuhan, misalnya:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Catatan: Folder yang dimulai dengan `.` adalah direktori konfigurasi tersembunyi di Linux.

## Opsional: Pengaturan Git

Hanya diperlukan jika Anda berencana membangun Lumi atau memulihkan repositori.

### Instal Git

```bash
sudo apt install git
```

Konfigurasikan identitas Anda:

```bash
git config --global --edit
```

#### Akses GitLab

Pulihkan akses repositori ke GitLab atau GitHub:

1. Ubah izin pada file kunci SSH: `chmod 600 ~/.ssh/id_rsa`
2. Tambahkan kunci ke instalasi Git baru: `ssh-add ~/.ssh/id_rsa`
3. Uji koneksi: `ssh -T git@ssh.gitlab.gnome.org` atau `ssh -T git@github.com`

Untuk setiap repositori, fetch remote dan reset cabang lokal agar cocok:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Jalankan `git status` untuk memastikan repositori bersih.

Anda kini memiliki OS baru dengan data dan repositori yang dipulihkan. Pengaturan ini mencerminkan lingkungan kerja yang dikenal dan digunakan untuk pengembangan Lumi, dan dapat disesuaikan dengan alur kerja individual sesuai kebutuhan.

## Bangun Lumi setelah pengaturan OS

Skrip build Lumi terletak di:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Instal dependensi sekali
sudo bash lumi-install-packages.sh

# Build setup penuh pertama
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Rebuild rutin setelah perubahan kode
bash lumi-build-script.sh --scope build --dir lumi-dev

# Jalur kompilasi cepat
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Luncurkan Lumi
bash lumi-launch-active.sh lumi-dev
```
