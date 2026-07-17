---
title: "Plugin Browser"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: ffbf087ea102e00b7057bf6bad9b6e2cb8f75ad05c7f26f0f2818d10f34392ce
url: "hub/scripting/reference/plugin-browser"
translation_lock: true
---
Browser Plug-In memungkinkan Anda menjelajahi sistem menu dan melihat di mana plug-in tertentu diinstal.

## Membuka Plugin Browser

Buka **Bantuan → Pemrograman → Browser Plug-In**.

## Apa yang Ditunjukkannya

Sementara Peramban Prosedur berfokus pada *fungsi* mentah dalam PDB, Peramban Plug-In adalah tampilan subset yang berfokus pada penemuan antarmuka pengguna. Ini secara khusus menyaring PDB untuk menampilkan "hal-hal yang tampak seperti plug-in yang diinstal pada menu."

Secara internal, ini menggunakan kueri yang hanya mengembalikan prosedur yang memiliki file terkait pada disk dan jalur menu terdaftar.

- **Pohon Menu**: Menampilkan representasi pohon dari struktur menu Lumi.
- **Lokasi Plug-In**: Membantu Anda menemukan lokasi plug-in yang baru dipasang di menu.
- **Metadata**: Menampilkan informasi tentang pembuat, versi, dan tanggal plug-in.

## Penggunaan

Gunakan Browser Plug-In ketika Anda mengetahui suatu fitur ada tetapi tidak dapat menemukannya di menu, atau ketika Anda merancang plug-in Anda sendiri dan ingin melihat di mana alat serupa berada.