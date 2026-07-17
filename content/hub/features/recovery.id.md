---
title: "Pemulihan Berkas"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
url: "hub/features/recovery"
translation_lock: true
---
Sistem pemulihan Lumi dirancang melindungi pekerjaan melukis dari crash, kesalahan, dan sesi terputus. Proyek mendapat jaring pengaman tanpa memaksa seniman terus-menerus menduplikasi berkas secara manual.

Pemulihan dibangun dari dua ide: perlindungan latar belakang otomatis dan titik pemeriksaan disengaja. Keduanya membantu melestarikan pekerjaan terbaru sambil tetap memungkinkan seniman kembali ke momen lebih awal dalam proyek.

![recover](/images/screens/recover.jpg)

## Perlindungan otomatis

Saat gambar sedang diedit, Lumi dapat menyimpan data pemulihan terpisah dari berkas kerja utama. Proyek tidak perlu ditulis ulang setiap kali snapshot keamanan dibuat.

Jika terjadi masalah, status pemulihan otomatis dapat memberikan versi karya seni yang mungkin lebih baru dari penyimpanan disengaja terakhir. Tujuannya sederhana: mengurangi pekerjaan yang hilang saat sesi berakhir tiba-tiba.

## Titik pemeriksaan disengaja

Beberapa momen dalam lukisan layak dilestarikan dengan sengaja: sebelum perubahan warna besar, setelah sketsa berhasil, sebelum keputusan flatten, atau saat mencoba arah berisiko.

Lumi mendukung titik pemeriksaan tingkat proyek untuk momen-momen ini. Lebih ringan daripada menyimpan salinan penuh terpisah untuk setiap eksperimen, namun tetap memberi seniman cara mundur ke titik penting dalam riwayat karya.

## Memulihkan dengan konteks

Status pemulihan disajikan sebagai versi karya seni, bukan berkas mentah yang harus dicari manual. Seniman dapat membandingkan penyimpanan otomatis terbaru dan titik pemeriksaan disengaja, lalu membuka status yang paling cocok dengan pekerjaan yang ingin dilanjutkan.

Gambar yang dipulihkan dibuka sebagai dokumen kerja, sehingga seniman dapat memeriksanya sebelum memutuskan cara menyimpan atau melanjutkan.

## Menjaga pemulihan praktis

Sistem pemulihan yang berguna juga harus tetap dapat dikelola. Lumi dirancang menjaga data pemulihan teratur dan memungkinkan status lama dihapus saat tidak diperlukan lagi.

Keamanan tidak menjadi berantakan. Pemulihan tetap aktif di latar belakang, sementara seniman tetap mengontrol seberapa banyak riwayat dipertahankan seiring waktu.

## Percaya diri saat bekerja

Tujuan pemulihan berkas bukan menggantikan penyimpanan, melainkan membuat pekerjaan kreatif kurang rapuh. Seniman dapat melukis, bereksperimen, dan mengambil risiko karena Lumi mempertahankan jalur kembali tambahan saat sesi, berkas, atau keputusan berjalan salah.
