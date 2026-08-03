---
title: "Kondisional"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals"
---
Kondisional adalah bagian mendasar dari pemrograman, yang memungkinkan skrip membuat keputusan dan mengontrol alirannya berdasarkan kriteria tertentu. Dalam Scheme, kondisional memungkinkan Anda membuat skrip dinamis dan cerdas yang beradaptasi dengan perubahan input, lingkungan, atau tindakan pengguna.

### Peran Kondisional dalam Scheme

Kondisional memiliki beberapa tujuan utama dalam skrip Anda:
- **Logika Pengarahan:** Memungkinkan Anda menjalankan potongan kode berbeda bergantung pada apakah kondisi tertentu benar atau salah.
- **Meningkatkan Fleksibilitas:** Dengan merespons input atau status secara dinamis, kondisional membantu skrip Anda menangani berbagai skenario.
- **Menyederhanakan Kompleksitas:** Mereka memecah pengambilan keputusan menjadi struktur yang dapat dikelola, membuat kode lebih mudah dibaca, di-debug, dan dipelihara.

### Jenis Kondisional yang Tersedia

Scheme menyediakan beberapa konstruksi kondisional, masing-masing disesuaikan dengan kebutuhan logis yang berbeda:
- **`if`:** Untuk membuat keputusan biner sederhana, mengeksekusi satu blok kode jika kondisinya benar dan blok kode lainnya jika kondisinya salah.
- **`cond`:** Konstruksi multi-percabangan yang kuat untuk menangani berbagai kondisi dengan cara yang jelas dan terstruktur.
- **`and` / `or`:** Operator logika yang mengevaluasi kombinasi kondisi, memungkinkan pengambilan keputusan yang lebih kompleks.
- **`else`:** Sebuah cakupan semua yang mendefinisikan perilaku fallback ketika tidak ada kondisi yang ditentukan terpenuhi.

### Cara Kerja Kondisional

Kondisional biasanya melibatkan:
1. **Mengevaluasi Kondisi:** Ekspresi pengujian menentukan apakah suatu kondisi benar atau salah.
2. **Eksekusi Percabangan:** Berdasarkan evaluasi, skrip memilih blok kode mana yang akan dieksekusi.
3. **Mengembalikan Nilai (Opsional):** Dalam beberapa kasus, kondisional juga dapat menghasilkan nilai yang dapat digunakan oleh bagian skrip lainnya.