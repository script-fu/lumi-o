---
title: "Dinamakan biarkan atau Definisi lokal"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bee02ac4fd1ab5ba61ffb50b49dbbba7fc473b141bd88a9cdf6d02aef3ca3a18
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/let vs define"
---
Keduanya **bernama `let`** dan **local `define`** merupakan alat canggih dalam Scheme untuk menyusun kode Anda, namun keduanya memiliki tujuan yang berbeda. Memahami kapan harus menggunakan masing-masing membantu dalam membuat skrip yang bersih, modular, dan efisien.

### Ikhtisar

- **Dinamakan `let`**: Konstruksi yang menggabungkan pengikatan variabel dan rekursi dalam cakupan lokal, biasanya digunakan untuk komputasi berulang atau rekursif.
- **Lokal `define`**: Cara untuk mendefinisikan fungsi atau variabel pembantu dalam cakupan fungsi terlampir, sehingga dapat digunakan kembali di berbagai bagian fungsi tersebut.

---

### Dinamakan `let`

#### Karakteristik:

1. Menggabungkan pengikatan variabel dan rekursi menjadi satu konstruksi.
2. Dicakup ke badan blok `let`.
3. Ideal untuk **rekursi lokal** atau proses berulang yang spesifik untuk satu tugas.

#### Sintaks

```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Contoh: Menjumlahkan Elemen Daftar

```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Hasil**: `10`

- **Cara Kerja**: Fungsi `loop` didefinisikan dalam `let`, memungkinkan panggilan rekursif dengan binding yang diperbarui.

---

### Lokal `define`

#### Karakteristik:

1. Memungkinkan pembuatan fungsi pembantu atau variabel yang dapat digunakan kembali dalam fungsi penutup.
2. Tercakup pada fungsi penutup tetapi terlihat seluruh tubuhnya.
3. Ideal untuk memodulasi kode dengan beberapa langkah atau logika yang dapat digunakan kembali.

#### Sintaks

```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Contoh: Memproses Banyak Nilai

```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Fungsi pembantu lokal
  (define (cube x) (* x x x))  ;; Fungsi pembantu lokal
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Hasil**: `41` (Menghitung \(2^2 + 3^3 + 4^2\))

- **Cara Kerja**: Fungsi pembantu `square` dan `cube` dapat digunakan kembali dalam fungsi `process-values`, sehingga memungkinkan logika modular.

---

### Perbedaan Utama

| **Aspek** | **Bernama `let`** | **Lokal `define`** |
|-------------------------------------------------------|--------------------------------------------------|------------------------------------------------|
| **Tujuan** | Menggabungkan rekursi dan iterasi dengan cara yang terlokalisasi. | Mendefinisikan fungsi atau variabel pembantu yang dapat digunakan kembali. |
| **Ruang Lingkup** | Terbatas pada badan blok `let`.           | Terlihat di seluruh fungsi penutup.      |
| **Dapat digunakan kembali** | Tidak dapat digunakan kembali di luar blok `let`.             | Dapat digunakan kembali beberapa kali dalam fungsi.    |
| **Kasus Penggunaan Terbaik** | Rekursi atau iterasi yang dilokalkan terkait dengan satu tugas. | Memodulasi kode dengan beberapa langkah yang dapat digunakan kembali. |
| **Sintaks** | Menggabungkan pengikatan dan rekursi dalam satu konstruksi.  | Mendefinisikan fungsi atau variabel secara eksplisit.      |

---

### Kapan Menggunakan Nama `let`

1. **Logika Sekali Pakai**: Ketika rekursi atau iterasi dikhususkan untuk satu komputasi.
2. **Enkapsulasi**: Untuk menghindari penambahan nama fungsi tambahan ke namespace fungsi penutup.
3. **Iterasi**: Saat mengelola variabel perantara dalam konstruksi perulangan.

**Contoh: Perhitungan Faktorial**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Hasil**: `120`

---

### Kapan Menggunakan Lokal `define`

1. **Pembantu yang Dapat Digunakan Kembali**: Saat logika perlu digunakan kembali di beberapa bagian fungsi.
2. **Desain Modular**: Untuk memecah komputasi kompleks menjadi subtugas yang lebih kecil dan diberi nama.
3. **Beberapa Langkah**: Ketika beberapa fungsi pembantu diperlukan untuk bagian komputasi yang berbeda.**Contoh: Memproses Input**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Hasil**: `(13 36)` (Menghitung \(2^2 + 3^2\) dan \(2^2 \cdot 3^2\))

---

### Menggabungkan Deklarasi dan Input dalam Nama `let`

Salah satu fitur paling canggih dari `let` adalah kemampuannya untuk menggabungkan **deklarasi variabel lokal** dan **parameter masukan** untuk rekursi ke dalam satu konstruksi. Hal ini menjadikan nama `let` ringkas dan ekspresif untuk tugas berulang atau rekursif.

#### Deklarasi Variabel Lokal

Dalam bernama `let`, pengikatan dalam tanda kurung bertindak sebagai **variabel lokal** yang diinisialisasi dengan nilai tertentu. Variabel-variabel ini tercakup dalam isi `let`.

```scheme
(let loop ((x 1)   ;; Mendeklarasikan x dengan nilai awal 1
           (y 2))  ;; Mendeklarasikan y dengan nilai awal 2
  (+ x y))         ;; Menggunakan x dan y di body
```

- **`x` dan `y`** adalah variabel lokal yang didefinisikan dan diinisialisasi sebagai bagian dari `let`.

---

#### Parameter Masukan untuk Rekursi

Variabel yang sama juga bertindak sebagai **parameter masukan** untuk panggilan rekursif ke `let` yang bernama. Saat bernama `let` memanggil dirinya sendiri, variabel tersebut akan diperbarui dengan nilai baru.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Panggilan rekursif dengan x dan y baru
```

- **Iterasi Pertama**: `x = 1`, `y = 2`
- **Iterasi Kedua**: `x = 2`, `y = 4`
- **Iterasi Ketiga**: `x = 3`, `y = 8`, dan seterusnya...

---

#### Setara Menggunakan Lokal `define`

Bernama `let` menyertakan inisialisasi variabel sebagai bagian dari sintaksisnya. Hal ini menghilangkan kebutuhan akan langkah terpisah untuk menyiapkan nilai awal. Dua contoh berikut ini setara:

##### Menggunakan Nama `let`

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Menggunakan Lokal `define`

```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Panggilan awal dengan x = 1, y = 2
```

Keduanya melakukan komputasi yang sama, namun bernama `let` menggabungkan deklarasi variabel dan penyiapan rekursi menjadi satu konstruksi ringkas.

---

#### Keuntungan Menggabungkan Deklarasi dan Input

1. **Ringkasan**: Dinamakan `let` mengurangi boilerplate dengan menggabungkan inisialisasi variabel dan rekursi ke dalam satu konstruksi.
2. **Kejelasan**: Memperjelas bahwa rekursi bersifat lokal pada `let` dan terkait dengan tugas tertentu.
3. **Enkapsulasi**: Logika rekursif tetap mandiri dan tidak mencemari namespace fungsi yang melingkupinya.

Sifat tujuan ganda dari `let`—sebagai deklarasi variabel dan mekanisme input rekursif—adalah yang menjadikannya fitur kuat dan unik dalam pemrograman Scheme.

### Ringkasan

- Gunakan **bernama `let`** untuk **rekursi terlokalisasi** atau **iterasi**, terutama ketika logika digabungkan secara erat ke satu tugas.
- Gunakan **local `define`** untuk **memodulasi kode** dengan fungsi atau variabel pembantu yang dapat digunakan kembali.

Dengan memahami perbedaannya, Anda dapat menulis program Scheme yang lebih ringkas, terorganisir, dan mudah dipelihara.