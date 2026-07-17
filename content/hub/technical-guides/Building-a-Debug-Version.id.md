---
title: "Membangun Versi Debug"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
url: "hub/technical-guides/Building-a-Debug-Version"
translation_lock: true
---

Panduan ini menjelaskan **alur kerja debug lokal** untuk Lumi menggunakan skrip di `build/lumi/scripts`.

Alur kerja dirancang untuk:

- menggunakan artefak build lokal (tanpa perlu mengunduh simbol),
- memverifikasi bahwa simbol debug benar-benar ada,
- meluncurkan GDB dengan mode simbol offline secara default.

## Prasyarat

- Linux berbasis Debian (baseline proyek: Debian 13)
- Pohon sumber Lumi sudah dikloning

## Penyiapan GDB sekali (opsional namun direkomendasikan)

Instal alat GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Penyiapan logging lokal opsional:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Catatan: Skrip debug lokal Lumi menonaktifkan `debuginfod` secara default agar resolusi simbol tetap lokal dan dapat direproduksi.

## Mulai cepat

Dari direktori skrip:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Build debug + peluncuran (default)

Gunakan ini untuk sesi debugging normal.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Perintah ini:

1. membangun Lumi dalam mode debug,
2. memverifikasi simbol debug,
3. meluncurkan Lumi di bawah GDB.

### Build debug saja (untuk sesi TTY/jarak jauh nanti)

Gunakan ini jika Anda ingin build sekarang dan meluncurkan/debug nanti.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Menggunakan TTY di Linux

TTY (konsol teks) sering kali merupakan cara paling andal untuk melakukan debug pada hard freeze.

- Beralih ke TTY dengan `Ctrl + Alt + F1` hingga `Ctrl + Alt + F6`
- Masuk dari prompt teks
- Kembali ke sesi grafis dengan `Ctrl + Alt + F7` (atau `F2` pada beberapa sistem)

Mengapa hal ini penting: jika sesi desktop macet, TTY sering masih merespons, sehingga Anda dapat melampirkan GDB, menangkap backtrace, dan memulihkan data crash yang berguna.

## Opsional: Debugging jarak jauh/TTY

Untuk hard freeze atau tampilan terkunci, gunakan `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Kemudian dari TTY (disarankan untuk skenario freeze) atau terminal lain:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Untuk peluncuran GDB lokal (jalur non-TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Catatan kinerja

Build debug memang lebih lambat. Setelah selesai debugging, beralih kembali ke build yang lebih cepat:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Reset release penuh untuk semua komponen utama
bash lumi-debug-reset-release.sh lumi-dev

# Varian lokal yang lebih cepat (opsional)
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
