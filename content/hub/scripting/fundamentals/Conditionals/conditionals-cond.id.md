---
title: "kond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
Dalam Scheme, kondisional `cond` digunakan untuk memilih salah satu dari beberapa blok kode yang mungkin untuk dieksekusi, berdasarkan beberapa pengujian. Ibaratnya multi-cabang `if`, dimana setiap cabang diperiksa secara berurutan hingga ditemukan kecocokan.

### Sintaks

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Setiap tes dievaluasi sesuai urutan penulisannya.
- Saat pengujian bernilai benar (`#t`), **konsekuen** terkait akan dijalankan, dan ekspresi `cond` berhenti mengevaluasi pengujian lebih lanjut.
- Klausa `else` bersifat opsional dan berfungsi sebagai cadangan jika tidak ada pengujian yang bernilai benar.

### Cara Kerjanya

1. **Uji Setiap Kondisi**:
   - `cond` mengevaluasi tes sesuai urutannya.

2. **Jalankan Konsekuensi Pencocokan**:
   - Saat pengujian pertama yang bernilai true (`#t`) ditemukan, **konsekuensinya** akan dijalankan.
   - Jika tidak ada pengujian yang bernilai benar dan terdapat klausa `else`, **konsekuensi fallback** akan dieksekusi.

### Contoh

#### Contoh 1: Konsekuensi Ekspresi Tunggal

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- Tes pertama `(< 3 2)` bernilai salah (`#f`).
- Tes kedua `(= 3 3)` bernilai benar (`#t`), sehingga `"This will run"` dikembalikan.
- Klausa `else` tidak dijalankan karena sudah ditemukan kecocokan.

Hasil: **"Ini akan berjalan"**

#### Contoh 2: Beberapa Tindakan Menggunakan `begin`

Jika konsekuensi melibatkan beberapa tindakan, gunakan `begin` untuk mengelompokkannya:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- Tes pertama `(< 5 3)` bernilai salah (`#f`).
- Tes kedua `(> 5 3)` bernilai benar (`#t`):
  - Mencetak `"Condition met"`.
  - Kemudian menghitung `(* 5 5)` dan mengembalikan `25`.

Hasil: **Mencetak "Kondisi terpenuhi" dan mengembalikan 25.**

#### Contoh 3: Menggunakan Blok `let` dalam Konsekuensi

Saat Anda perlu memperkenalkan variabel lokal, gunakan blok `let`:

```scheme
(cond
  ;; Kasus 1: Jika 0 kurang dari -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Kasus 2: Jika 0 lebih besar dari -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Kasus default: Jika tidak ada kondisi di atas yang terpenuhi
  (else
    (let ((z 0))
      z)))
```

- Tes pertama `(< 0 -1)` salah.
- Tes kedua `(> 0 -1)` benar, jadi:
  - Blok `let` dijalankan, mengikat `y` ke `20`.
  - Mencetak `"Positive condition met"`.
  - Kemudian menghitung `(+ y y)` dan mengembalikan `40`.

Hasil: **Mencetak "Kondisi positif terpenuhi" dan menghasilkan 40.**

#### Contoh 4: Penggantian dengan `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Tak satu pun dari dua tes pertama yang bernilai benar.
- Klausa `else` dijalankan dan mengembalikan `"Fallback value"`.

Hasil: **"Nilai cadangan"**

### Ringkasan

- Gunakan `cond` untuk menangani berbagai kondisi secara jelas dan ringkas.
- Konsekuensi dapat berupa ekspresi tunggal atau tindakan berkelompok menggunakan `begin`.
- Gunakan `let` sebagai konsekuensi untuk mendeklarasikan variabel lokal untuk perhitungan.
- Selalu sertakan klausa `else` sebagai cadangan untuk menangani kasus yang tidak terduga.

Fleksibilitas ini menjadikan `cond` alat yang kuat dan mudah dibaca untuk menangani logika percabangan yang kompleks.