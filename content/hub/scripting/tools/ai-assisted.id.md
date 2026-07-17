---
title: "Pengembangan dengan Bantuan AI"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
translation_lock: true
---
Alat AI modern dapat mempercepat pengembangan plug-in Lumi secara signifikan dengan bertindak sebagai mitra pengkodean kolaboratif.

## VS Code dalam Mode Agent

Menggunakan Visual Studio Code dengan asisten AI dalam **Mode Agen** (seperti mode Agen GitHub Copilot atau asisten lain yang mendukung alat) memungkinkan Anda melakukan tugas multi-langkah yang kompleks menggunakan bahasa alami.

Daripada hanya menyelesaikan satu baris kode, Agen dapat:
- Baca seluruh ruang kerja Anda untuk memahami konteks.
- Buat file dan direktori baru.
- Jalankan perintah terminal untuk menguji atau memvalidasi skrip.
- Cari pola yang ada di basis kode Anda.

## Akses Repositori

Bantuan AI paling efektif ketika Agen memiliki akses ke **lumi-dev** atau repositori proyek spesifik Anda. Dengan visibilitas ke dalam basis kode yang ada, Agen dapat:
- Gunakan **[Perpustakaan Utilitas]({{< ref "/hub/scripting/reference/utility-browser" >}})** sebagai referensi untuk fungsi pembantu.
- Ikuti pola yang ada untuk operasi GEGL dan manajemen lapisan.
- Gunakan kembali kode boilerplate dari plug-in yang sudah ada.

## Contoh Alur Kerja

Anda dapat langsung meminta Agen untuk membuat plug-in lengkap dengan menjelaskan hasil fungsional yang diinginkan:

> "Menggunakan utilitas Scheme yang tersedia dan contoh di ruang kerja, tulis plug-in baru yang membuat panduan horizontal 50% pada gambar aktif dan beri nama 'Panduan Pusat'."

Agen akan mencari cara membuat panduan, mengidentifikasi fungsi utilitas yang benar (seperti `lumi-image-add-hguide-percent` dari `common.scm`), dan menghasilkan file `.scm` lengkap dengan boilerplate registrasi yang benar.

## Praktik Terbaik

- **Bersikap Spesifik**: Jelaskan dengan tepat apa yang Anda ingin plug-in lakukan.
- **Utilitas Referensi**: Dorong Agen untuk melihat direktori `share/lumi/scripts/` untuk mencari pembantu tingkat tinggi.
- **Tinjau dan Uji**: Selalu uji plug-in yang dihasilkan oleh AI, seringkali ini merupakan proses yang berulang dan kreatif.