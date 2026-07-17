---
title: "Struktur Data"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: 352594bbda9977488d773240c50663f63fd432a17483772a9cbf8d59dab378be
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/_index"
---
Dalam Scheme, **struktur data** adalah alat penting untuk mengatur, menyimpan, dan memanipulasi data. Mereka memungkinkan pengembang untuk membuat skrip yang efisien, mudah dibaca, dan digunakan kembali. Dengan memilih struktur data yang tepat untuk masalah tertentu, Anda dapat mengoptimalkan performa dan kejelasan kode Anda.

## Struktur Data Utama dalam Scheme

Scheme menyediakan beberapa struktur data yang kuat dan serbaguna, masing-masing cocok untuk tugas tertentu. Struktur data primer meliputi:

### Daftar

Daftar adalah kumpulan elemen yang dapat bertambah atau menyusut secara dinamis. Mereka ideal untuk data berurutan atau hierarki dan banyak digunakan dalam pemrograman fungsional.

Fitur utama:
- Berukuran dinamis.
- Elemen dapat berupa tipe campuran.
- Biasa digunakan untuk algoritma rekursif dan mewakili struktur mirip pohon.

Contoh penggunaan:
- Mengelola koleksi barang.
- Mewakili urutan atau hierarki.

---

### Vektor

Vektor adalah kumpulan elemen berukuran tetap, diindeks untuk akses cepat. Mereka paling cocok untuk skenario di mana kinerja dan akses posisi sangat penting.

Fitur utama:
- Memperbaiki ukuran saat pembuatan.
- Elemen diakses berdasarkan indeksnya.
- Lebih cepat dari daftar untuk operasi tertentu seperti akses acak.

Contoh penggunaan:
- Menyimpan konfigurasi atau data berukuran tetap.
- Pencarian dan pembaruan cepat berdasarkan posisi.

---

### Memilih Struktur Data yang Tepat

Keputusan untuk menggunakan **daftar** atau **vektor** bergantung pada kebutuhan spesifik skrip Anda. Berikut beberapa pedomannya:

| Fitur | Daftar | Vektor |
|----------|------------------------------|--------------------------------|
| **Fleksibilitas Ukuran** | Dinamis | Memperbaiki |
| **Kecepatan Akses** | Lebih lambat (akses berurutan) | Lebih cepat (akses terindeks) |
| **Kemudahan Modifikasi**| Lebih mudah | Lebih sulit (membutuhkan realokasi)|
| **Kasus Penggunaan** | Data dinamis, rekursi | Data statis, pencarian cepat |

---