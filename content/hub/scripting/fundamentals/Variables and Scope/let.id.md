---
title: "membiarkan"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 6e768f3feb8a1873423841338e92494ebd2b4ac0af5b6e27253f3cf2c2ba455f
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/let"
---
Nama `let` digunakan karena mencerminkan asal matematisnya dalam memperkenalkan pengikatan sementara, seperti pada _"Let \( x = 2 \) dan \( y = 3 \)"_.

Pernyataan `let` dalam Scheme adalah **konstruksi pengikat** yang digunakan untuk mendefinisikan variabel dalam cakupan lokal. Ini memungkinkan Anda membuat pengikatan sementara untuk variabel dan kemudian mengeksekusi blok kode menggunakan pengikatan tersebut. Hal ini sangat berguna untuk menjaga kode tetap modular dan menghindari polusi variabel global.

Ada tiga bentuk utama `let` dalam Scheme:

- **`let`**: Izin standar untuk membuat pengikatan lokal sederhana.
- **`let*`** : Sequential let, dimana binding dapat bergantung pada hasil binding sebelumnya.
- **Dinamakan `let`**: Bentuk khusus `let` yang membuat loop rekursif atau prosedur bernama.

Dalam bentuknya yang paling sederhana, `let` membuat pengikatan variabel lokal dan mengevaluasi ekspresi dengan pengikatan tersebut.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Binding**: Daftar pasangan yang setiap pasangannya menetapkan `value` ke `variable`.
- **Ekspresi**: Isi `let`, yang dapat menggunakan variabel yang ditentukan secara lokal.

### Contoh

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Ini mendefinisikan dua variabel lokal, `x` (10) dan `y` (20).
- Kemudian menghitung `(+ x y)` menggunakan variabel-variabel ini.

**Hasil**: `30`

---

## Konstruksi `let*`

Konstruk `let*` mirip dengan `let`, namun binding dievaluasi **secara berurutan**. Ini berarti pengikatan selanjutnya dapat bergantung pada pengikatan sebelumnya.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Contoh

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- Pengikatan pertama menetapkan `10` ke `x`.
- Pengikatan kedua menghitung `y` sebagai `(+ x 5)`, menggunakan nilai `x`.
- Tubuh menghitung `(* x y)`.

**Hasil**: `150`

---

## Dinamakan `let`

Nama `let` adalah bentuk khusus dari `let` yang memberikan nama untuk blok `let` itu sendiri, mengubahnya menjadi prosedur rekursif. Ini berguna untuk membuat loop atau komputasi rekursif.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nama**: Blok `let` diberi nama, yang secara efektif mendefinisikan suatu fungsi.
- **Binding**: Nilai awal untuk variabel, mirip dengan standar `let`.
- **Body**: Ekspresi dapat memanggil nama `let` secara rekursif.

### Contoh: Perulangan dengan Nama `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- Fungsi `loop` dimulai dengan `n = 5` dan `result = 1`.
- Jika `n` adalah `0`, maka `result` akan dikembalikan.
- Jika tidak, ia akan memanggil dirinya sendiri secara rekursif dengan `n - 1` dan `result * n`.

**Hasil**: `120` (Faktorial 5)

---

## Tabel Ringkasan

| Membangun | Deskripsi | Kasus Penggunaan |
|------------|------------------------------------------|--------------------------------------------------------------------------|
| **`let`** | Mendefinisikan pengikatan lokal untuk variabel.    | Gunakan ketika semua pengikatan bersifat independen dan tidak bergantung satu sama lain.     |
| **`let*`** | Mendefinisikan pengikatan lokal berurutan.       | Gunakan ketika pengikatan selanjutnya bergantung pada hasil pengikatan sebelumnya.           |
| **Bernama `let`** | Mendefinisikan prosedur lokal rekursif. | Gunakan for loop, komputasi berulang, atau rekursi dalam konteks lokal. |

---

## Contoh

### Menggunakan `let` untuk Komputasi Lokal

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Hasil**: `13` (Menghitung `x² + y²`)

---

### Menggunakan `let*` untuk Ketergantungan Berurutan

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Hasil**: `8` (Menghitung `x³`)

---

### Menggunakan Named `let` untuk Komputasi Rekursif

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Hasil**: `120` (Faktorial 5)

---

Dengan menggunakan `let`, `let*`, dan diberi nama `let`, Scheme memungkinkan pemrograman modular, rekursif, dan sekuensial dengan aturan pelingkupan yang jelas.