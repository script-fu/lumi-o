---
title: "Peramban Prosedur"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: f2593585be79d09f94dee166e7003ceddc532b4d2f7c1060222fe5f5c758ef27
url: "hub/scripting/reference/procedure-browser"
translation_lock: true
---
Procedure Browser adalah alat referensi utama untuk menemukan ratusan fungsi yang tersedia di Procedure Databaseal (PDB) Lumi. Karena setiap alat, filter, dan skrip di Lumi harus terdaftar di PDB agar dapat dipanggil, browser ini secara efektif merupakan penjelajah PDB yang lengkap.

## Membuka Procedure Browser

Buka **Bantuan → Pemrograman → Procedure Browser**.

Anda juga dapat mengaksesnya dari Konsol Scheme melalui **Jelajahi**.

## Apa yang Ditunjukkannya

Procedure Browser dapat membuat daftar semua prosedur yang saat ini terdaftar di PDB, tanpa memandang asal prosedur tersebut. Defaultnya adalah mencari "internal", untuk menampilkan prosedur inti yang terdaftar secara internal.

- **Prosedur Internal**: Fungsi inti untuk manipulasi gambar, manajemen lapisan, dan kontrol alat.
- **Plug-in Eksternal**: Prosedur yang disediakan oleh plug-in C/C++ yang dikompilasi atau ekstensi persisten.

## Pencarian dan Penyaringan

- **Kotak pencarian**: Memfilter prosedur berdasarkan nama, deskripsi, atau penulis. Mengosongkan bidang pencarian menunjukkan semua prosedur yang tersedia.
- **Jenis Pencarian**: Dropdown pencarian memungkinkan Anda memfilter berdasarkan bidang tertentu. Jika Anda menyetelnya ke **menurut jenis** dan mencari "internal", daftar akan menyempit hingga hanya menampilkan prosedur inti yang terdaftar secara internal.
- **Tampilan Terperinci**: Mengklik prosedur akan menampilkan parameternya, nilai yang dikembalikan, penulis, tanggal, dan deskripsi fungsinya.

Hal ini penting untuk menemukan nama yang tepat dan tanda tangan argumen dari fungsi yang ingin Anda panggil dari skrip Anda.