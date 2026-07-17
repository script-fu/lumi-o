---
title: "Utility Browser"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 99abaafdc68cf3433959e5db87130b22c51cfbd5a98697fa807732b9fdae9ff0
url: "hub/scripting/reference/utility-browser"
translation_lock: true
---
Utility Browser memungkinkan Anda menjelajahi stdlib utilitas Scheme bawaan yang dikirimkan bersama Lumi, tanpa harus keluar dari aplikasi atau menggali file sumber.

## Membuka Utility Browser

Buka **Bantuan → Pemrograman → Utility Browser**.

Jendela segera terbuka; tidak ada plug-in yang perlu dimuat terlebih dahulu.

## Apa yang Ditunjukkannya

Browser mencantumkan setiap prosedur, variabel, dan bentuk sintaksis yang diekspor oleh tujuh pustaka utilitas yang dimuat Lumi secara otomatis saat startup:

| Perpustakaan | Cakupannya |
|---|---|
| `common.scm` | Pembantu tujuan umum (string, nomor, daftar utilitas) |
| `files.scm` | Pembantu file dan jalur |
| `gegl.scm` | Buffer GEGL dan pembantu warna |
| `images.scm` | Pembantu tingkat gambar (`image-get-open-list`, dll.) |
| `layers.scm` | Lapisan dan pembantu yang dapat digambar |
| `parasites.scm` | Pembantu baca/tulis parasit |
| `paths.scm` | Pembantu jalur dan vektor |

Semua ini tersedia di plug-in Scheme apa pun atau di Konsol Scheme.

## Pencarian dan Penyaringan

- **Kotak pencarian**: memfilter berdasarkan nama saat Anda mengetik (pencocokan substring yang tidak peka huruf besar-kecil).
- **Filter baik**: persempit hasil ke `procedure`, `variable`, atau `syntax`.

Mengklik sebuah entri akan menampilkan dokumen lengkapnya dan perpustakaan asalnya.

## Stdlib sebagai Pembungkus

Pustaka utilitas adalah aplikasi praktis dari pola pembungkusan: setiap helper memberikan nama yang jelas untuk operasi tingkat rendah, menyembunyikan boilerplate, dan menyediakan satu tempat untuk memperbarui jika perintah yang mendasarinya berubah. Jika Anda ingin memahami pendekatan desain di baliknya, lihat tutorial **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})**.

## Hubungan dengan Procedure Browser

Utility Browser terpisah dari **Filter → Script-Fu → Konsol → Telusuri** (Procedure Browser). Procedure Browser mencantumkan prosedur yang terdaftar di PDB. Utility Browser mencantumkan definisi pembantu yang sengaja ada *di luar* PDB: definisi tersebut hanya untuk Scheme dan tidak mengikat C.