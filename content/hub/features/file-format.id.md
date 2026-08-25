---
title: "Format File (.lum)"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
url: "hub/features/file-format"
translation_lock: true
---

Format `.lum` asli Lumi adalah direktori proyek, bukan satu berkas tertutup. Format ini dirancang untuk ilustrasi berlapis: pohon lapisan yang dalam, kanvas besar, mask, efek non-destruktif, dan titik pemeriksaan yang tidak harus menduplikasi seluruh lukisan.

Tugas format ini menjaga struktur kerja itu utuh — agar proyek dapat dibuka kembali apa adanya, diperiksa ketika ada yang salah, dan dipulihkan dari titik pemeriksaan terbaru tanpa memperlakukan karya sebagai satu gumpalan buram.

## Dipisahkan dengan sengaja

Proyek `.lum` adalah sebuah folder. Pohon lapisan dan properti gambar tersimpan dalam XML yang dapat dibaca. Setiap lapisan dan mask menyimpan buffer pikselnya sendiri, dinamai menurut karya seni, bukan menurut ID internal. Jalur vektor disimpan sebagai SVG biasa. Pengaturan filter yang berat berada di berkas tersendiri di samping gambar. Profil ICC disimpan sekali di akar proyek, sehingga snapshot pemulihan cukup merujuknya alih-alih menyalinnya.

Pemisahan itulah yang memungkinkan bagian lain format ini. Lapisan yang tidak berubah dapat dibiarkan di disk. Buffer yang rusak gagal sendiri, tanpa menyeret seluruh berkas. Piksel lapisan yang hilang menjadi lapisan kosong yang tetap punya nama, posisi, dan pengaturan blend; komposit grup yang hilang dibangun ulang dari lapisan di dalamnya. Proyek tetap menjadi peta cara lukisan itu disusun.

Palet pigmen tetap bersama alat warna Lumi. Proyek dapat mengingat palet mana yang terkait dengan gambar, tetapi pustaka palet itu sendiri berada di luar `.lum`.

## Status yang dapat diedit, bukan gambar datar

Berkas menyimpan lukisan yang sedang dikerjakan. Lapisan tetap lapisan, grup tetap grup, dan mask tetap mask — termasuk ofset, kunci, perilaku blend, dan tumpukan filter. Filter non-destruktif disimpan sebagai operasi dan parameter, bukan sebagai piksel yang sudah diterapkan. Lapisan yang hanya satu warna datar tidak perlu berkas piksel sama sekali.

Grup yang dilipat juga menyimpan tampilan kompositnya. Komposit tersimpan itulah yang muncul di kanvas saat grup ditutup, sehingga lapisan di dalamnya tidak perlu disusun ulang hanya untuk melihat gambar. Mode pemeriksaan yang hanya untuk tampilan tidak masuk ke cache itu: menampilkan mask atau alpha agar dapat disunting dipulihkan sebagai metadata, bukan dilekatkan ke dalam grup yang disimpan.

## Berkas besar dapat tetap sebagian di disk

Membuka `.lum` tidak harus memuat setiap piksel. Isi di dalam grup yang dilipat dapat tetap di disk sementara komposit tersimpan grup itu ditampilkan segera. Memperluas grup adalah saat lapisan, mask, dan grup bersarang itu masuk ke memori. Grup yang tetap tertutup tetap ringan.

Berkas juga mencatat grup mana yang benar-benar dipakai. Grup di jalur pemilihan aktif dapat dibuka kembali dalam keadaan diperluas; folder lain disimpan sebagai dilipat meski sempat terbuka di sesi terakhir. Itu mencegah berkas yang dalam agar tidak memuat setiap cabang yang tidak terpakai ke memori begitu dibuka.

Pengelompokan karena itu adalah pilihan kinerja sekaligus organisasi. Latar belakang besar, eksperimen yang diarsipkan, dan varian yang tidak dipakai dapat berada di grup tertutup tanpa menempati memori yang sama dengan lapisan yang sedang dilukis. Penyimpanan mengikuti aturan yang sama: buffer yang masih tersembunyi disalin atau dilewati sebagai berkas, tidak dimuat kembali ke memori hanya untuk ditulis lagi.

## Titik pemeriksaan yang hanya menulis yang berubah

Berkas → Simpan memperbarui proyek kerja. Penyimpanan bertahap dan penyimpanan otomatis menulis ke pohon pemulihan, dan hanya menulis data yang berubah — buffer lapisan yang berubah, bukan salinan kedua seluruh gambar. Setiap titik pemeriksaan tetap membawa deskripsi lengkap pohon lapisan, sehingga titik mana pun di jejak itu dapat dibuka dengan mengisi piksel yang tidak berubah dari titik pemeriksaan yang lebih lama dan, jika perlu, dari berkas kerja itu sendiri.

Penyimpanan otomatis memakai pola yang sama di cache terpisah, sehingga perlindungan otomatis tidak harus menulis ulang berkas di disk. Jika proyek dibuka saat ada titik pemeriksaan yang lebih baru daripada penyimpanan penuh terakhir, Lumi dapat menawarkannya alih-alih diam-diam membuang pekerjaan yang lebih baru. Gambar yang dipulihkan dibuka dengan nama berbeda agar penyimpanan cepat tidak menimpa aslinya.

## Format kerja

`.lum` dipakai untuk melanjutkan lukisan di Lumi. Format yang diratakan atau berorientasi kompatibilitas dipakai untuk penerbitan, pengiriman, dan aplikasi lain. Karena proyek adalah direktori berisi banyak berkas, proyek hendaknya diarsipkan jika perlu berpindah.

Berkas kerja tetap kaya dan dapat diedit. Ekspor adalah cara gambar jadi atau yang dibagikan meninggalkan struktur itu.
