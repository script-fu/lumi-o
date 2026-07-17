---
title: "Format File (.lum)"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
url: "hub/features/file-format"
translation_lock: true
---
Format file asli Lumi dibuat untuk proyek lukisan berlapis yang perlu tetap andal, dapat diperiksa, dan dapat dipulihkan seiring waktu. Format ini dirancang mengikuti realitas pekerjaan ilustrasi: banyak lapisan, kanvas besar, informasi warna tertanam, mask, efek, dan data pemulihan.

Alih-alih memperlakukan proyek sebagai satu blob tidak transparan, format ini menjaga struktur karya seni tetap terlihat oleh aplikasi. Hal ini memungkinkan Lumi menyimpan, memuat, dan memulihkan gambar besar dengan lebih cerdas sambil mempertahankan organisasi yang diandalkan seniman.

## Struktur proyek terbuka

Proyek Lumi memisahkan bagian karya seni: struktur gambar, konten lapisan, mask, data warna, metadata, dan informasi pemulihan—masing-masing punya peran jelas. Format ini lebih mudah dipahami dan lebih cocok untuk akses jangka panjang dibanding wadah monolitik tertutup.

Tujuannya bukan hanya menyimpan piksel, tetapi menyimpan status kerja ilustrasi. Lapisan tetap lapisan, mask tetap mask, dan berkas terus mencerminkan cara karya seni dibangun.

## Dirancang untuk lukisan besar

Gambar berlapis besar bisa cepat menjadi berat. Format Lumi mendukung alur kerja di mana tidak setiap bagian data gambar perlu dimuat ke memori sekaligus. Proyek tetap responsif dengan memuat bagian gambar yang benar-benar dibutuhkan untuk melihat, mengedit, mengomposisi, atau mengekspor.

Pendekatan ini membantu berkas kompleks terasa mudah dikelola, terutama saat karya seni berisi banyak lapisan tersembunyi, diarsipkan, eksperimental, atau dikelompokkan.

## Menyimpan tanpa mengganggu alur

Format file mendukung penyimpanan proyek normal dan snapshot pemulihan ringan. Seniman dapat melindungi pekerjaan secara rutin tanpa mengubah setiap titik pemeriksaan menjadi duplikat penuh seluruh gambar.

Karena informasi pemulihan bagian dari struktur proyek, Lumi dapat menyimpan riwayat berguna dekat karya seni sambil tetap memisahkan penyimpanan keamanan otomatis dari berkas kerja.

## Pertukaran dan ekspor

Format asli ditujukan untuk pekerjaan Lumi yang sedang berjalan, sedangkan format ekspor dipakai untuk berbagi hasil rata atau berfokus kompatibilitas. Dukungan impor membantu membawa karya seni yang ada ke lingkungan berlapis Lumi; dukungan ekspor memungkinkan karya jadi meninggalkan format proyek saat siap diterbitkan, dikirim, atau diproses lebih lanjut.

Perbedaan ini menjaga berkas kerja kaya dan dapat diedit sambil memungkinkan gambar akhir diproduksi dalam format eksternal umum.

## Keandalan jangka panjang

Singkatnya, format `.lum` adalah wadah praktis untuk pekerjaan melukis serius: cukup terbuka untuk diperiksa, cukup terstruktur untuk dipulihkan, dan cukup fleksibel menangani gambar berlapis kompleks secara efisien.
