---
title: "Variabel dan Ruang Lingkup"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: 82a033dab5a3f8e3bacc73cde3d2f965fda6cd1b8957877e29da8cfcb547abdd
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope"
---
Dalam Scheme, pengelolaan variabel dan cakupannya adalah konsep inti untuk menulis skrip yang efisien dan mudah dipelihara. Variabel menyimpan nilai data yang dapat dimanipulasi oleh skrip Anda, sementara cakupan menentukan di mana variabel tersebut dapat diakses. Memahami cara mendefinisikan dan menggunakan variabel secara efektif memungkinkan Anda membuat kode yang terstruktur, dapat digunakan kembali, dan bebas kesalahan.

### Pengetikan Dinamis

Scheme diketik secara dinamis: Anda tidak mendeklarasikan tipe terlebih dahulu, dan variabel dapat menampung nilai-nilai yang berbeda seiring waktu.

```scheme
(define x 42)       ; x adalah angka
(set! x "hello")    ; sekarang x adalah string
```

### Peran Definisi dan Cakupan Variabel dalam Scheme

Mendefinisikan variabel dan mengelola cakupannya memiliki beberapa tujuan:
- **Pengorganisasian Data:** Variabel menyimpan informasi, membuat skrip Anda lebih mudah dibaca dan dikelola.
- **Meningkatkan Penggunaan Kembali:** Dengan menggunakan variabel cakupan, Anda dapat menggunakan kembali bagian kode tanpa konflik.
- **Enkapsulasi:** Cakupan yang dilokalkan mencegah interaksi yang tidak diinginkan antar variabel di berbagai bagian skrip Anda.
- **Logika Penyederhanaan:** Variabel sementara dalam cakupan terbatas mengurangi kompleksitas dalam penghitungan atau alur kerja yang lebih besar.

### Jenis Definisi dan Ruang Lingkup Variabel

Scheme menyediakan beberapa konstruksi untuk mendefinisikan dan melingkupi variabel:
- **`let`:** Membuat pengikatan lokal untuk variabel dalam blok kode tertentu.
- **`let*`:** Versi berurutan dari `let` di mana setiap pengikatan dapat bergantung pada pengikatan sebelumnya.
- **Dinamakan `let`:** Konstruksi yang kuat untuk mendefinisikan prosedur atau loop lokal rekursif.
- **`define`:** Membuat variabel atau fungsi global yang dapat diakses di seluruh skrip Anda.

### Cara Kerja Definisi dan Ruang Lingkup Variabel

Definisi dan ruang lingkup variabel biasanya melibatkan:
1. **Mendeklarasikan Variabel:** Memberikan nilai pada variabel dalam konteks tertentu.
2. **Limiting Scope:** Mengontrol tempat variabel dapat diakses (misalnya, dalam blok `let` atau secara global).
3. **Menggunakan Variabel:** Mengakses dan memodifikasi nilai variabel untuk melakukan penghitungan, logika, atau operasi prosedural.

### Contoh: Menggunakan `let` untuk Variabel Lokal

Konstruksi `let` memungkinkan Anda menentukan variabel sementara yang hanya tersedia dalam blok tertentu:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Contoh ini mendeklarasikan `x` dan `y` dengan nilai lokal dan menghitung jumlahnya.

### Contoh: Menggunakan `define` untuk Variabel Global

Konstruk `define` membuat variabel atau fungsi dengan cakupan global:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Skrip ini mendefinisikan konstanta global `pi` dan fungsi `circle-area` yang menggunakannya.

### Perbandingan Cakupan: Lokal vs. Global

| Fitur | Lingkup Lokal (`let`, `let*`) | Lingkup Global (`define`) |
|------------------|------------------------------------------|-----------------------------------------------|
| **Aksesibilitas** | Terbatas pada blok yang didefinisikan | Dapat diakses di seluruh skrip |
| **Enkapsulasi** | Mencegah interaksi yang tidak diinginkan | Mungkin bertentangan dengan variabel lain yang ditetapkan secara global |
| **Kasus Penggunaan** | Variabel sementara untuk tugas tertentu | Variabel atau fungsi bersama yang digunakan di seluruh |

### Ringkasan

- **Definisi dan cakupan variabel** merupakan dasar untuk mengatur dan mengelola data dalam skrip Scheme Anda.

- Gunakan **cakupan lokal** (`let`, `let*`, bernama `let`) untuk merangkum variabel sementara dan menghindari konflik.
- Gunakan **cakupan global** (`define`) untuk fungsi atau konstanta yang dapat digunakan kembali yang dibagikan di seluruh skrip Anda.
- Pemahaman yang jelas tentang konstruksi ini akan meningkatkan keterbacaan, pemeliharaan, dan keandalan kode Anda.