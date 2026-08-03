---
title: "Perulangan"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: df3e2118b9a580de4eed6ac56d9717aa3cbf555ab66bb49fabb4164b2994af91
translation_lock: true
url: "hub/scripting/fundamentals/Iteration"
---
Iterasi adalah landasan pemrograman, memungkinkan skrip mengulangi tindakan dan memproses pengumpulan data secara efisien. Dalam Scheme, berdasarkan bahasa pemrograman Scheme, iterasi menyediakan alat untuk mengotomatiskan tugas yang berulang, memanipulasi struktur data, dan membuat pola eksekusi yang canggih.

### Peran Iterasi dalam Scheme

Iterasi memenuhi beberapa tujuan penting dalam skrip Anda:
- **Mengotomatiskan Pengulangan:** Memungkinkan Anda melakukan tindakan atau serangkaian tindakan yang sama beberapa kali tanpa menggandakan kode.
- **Meningkatkan Efisiensi:** Dengan memproses struktur data secara berulang, skrip dapat menangani operasi skala besar secara sistematis.
- **Perampingan Kode:** Iterasi menghilangkan redundansi, membuat kode lebih ringkas, mudah dibaca, dan dipelihara.

### Jenis Iterasi yang Tersedia

Scheme menawarkan beberapa konstruksi untuk iterasi, masing-masing disesuaikan dengan kebutuhan spesifik:
- **map:** Menerapkan fungsi ke setiap elemen daftar, mengembalikan daftar baru beserta hasilnya.
- **untuk-setiap:** Mirip dengan `map`, tetapi digunakan untuk menjalankan fungsi pada setiap elemen tanpa mengembalikan hasil.
- **do:** Konstruksi loop tujuan umum yang menangani berbagai macam proses berulang.
- **rekursi:** Teknik canggih yang memungkinkan fungsi memanggil dirinya sendiri untuk memecahkan masalah secara bertahap.

### Cara Kerja Iterasi

Iterasi biasanya melibatkan:
1. **Mendefinisikan Pengulangan:** Menentukan tindakan yang akan diulang dan data atau rentang yang akan diproses.
2. **Eksekusi Secara Berurutan:** Mengulangi tindakan untuk setiap elemen, langkah, atau kondisi hingga selesai.
3. **Mengembalikan Hasil (Opsional):** Bergantung pada konstruksinya, iterasi dapat menghasilkan hasil atau mengubah status.

Konstruksi ini memungkinkan Anda menulis skrip yang mudah beradaptasi, efisien, dan elegan yang dapat menangani tugas kompleks dengan mudah.