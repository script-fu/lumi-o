---
title: "Fungsi Lambda"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: bbc5db329f2db333e2133fe248611e30afe266325f05b4209ae197517d068186
translation_lock: true
url: "hub/scripting/fundamentals/Functions/lambda"
---
**Fungsi Lambda** dalam Scheme adalah fungsi anonim, artinya fungsi tersebut adalah fungsi tanpa nama. Fungsi-fungsi ini didefinisikan secara inline dan biasanya digunakan untuk operasi singkat dan satu kali saja. Konstruksi `lambda` adalah alat yang ampuh dalam pemrograman fungsional, memungkinkan Anda membuat logika yang ringkas dan fleksibel dengan cepat.

Fungsi Lambda sangat berguna ketika:

- Anda memerlukan fungsi kecil untuk tujuan sementara yang spesifik.
- Meneruskan fungsi sebagai argumen ke fungsi tingkat tinggi seperti `map`, `filter`, atau `fold`.
- Mengembalikan fungsi dari fungsi lainnya.

### Sintaks Fungsi Lambda

Fungsi Lambda dapat didefinisikan sendiri...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...atau segera dipanggil:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Parameter yang diterima fungsi.
- **`body-expression`:** Logika dijalankan ketika fungsi dipanggil.
- **Invokasi Langsung:** Bentuk kedua menunjukkan lambda yang segera dipanggil dengan argumen.

### Contoh Fungsi Lambda

#### Menggunakan Lambda untuk Perhitungan Sederhana

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Mengembalikan 8
```

Di sini:

- Fungsi lambda dibuat untuk menambahkan dua angka (`x` dan `y`).
- Fungsi ini segera dipanggil dengan argumen `3` dan `5`.

#### Fungsi Lambda Sebaris

Contoh berikut menunjukkan cara menggunakan `for-each` dengan fungsi bernama dan fungsi lambda:

**Menggunakan Fungsi Bernama:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Penjelasan**:
  - `print-item` adalah fungsi bernama yang mengubah angka menjadi string (`number->string`) dan mencetaknya menggunakan `lumi-message`.
  - `for-each` menerapkan `print-item` untuk setiap elemen dalam daftar `(1 2 3 4)`.

**Keluaran**: 1 2 3 4

**Menggunakan Fungsi Lambda:**

Logika yang sama dapat ditulis sejalan dengan fungsi lambda, sehingga tidak memerlukan fungsi bernama terpisah:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Penjelasan**:
  - `(lambda (x) (lumi-message (number->string x)))` mendefinisikan fungsi anonim.
  - Fungsi ini diterapkan ke setiap elemen daftar `(1 2 3 4)` oleh `for-each`.

**Keluaran**: 1 2 3 4

#### Lambda Berfungsi sebagai Argumen

Fungsi Lambda sering kali diteruskan langsung ke fungsi tingkat tinggi seperti `map` atau `filter`.

#### Mengkuadratkan Daftar Angka

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Mengembalikan (1 4 9 16)
```

- Fungsi `lambda` mengkuadratkan setiap elemen daftar.
- Fungsi `map` menerapkan `lambda` ke setiap elemen.

#### Lambda Berfungsi sebagai Nilai Pengembalian

Anda dapat mengembalikan fungsi lambda dari fungsi lain untuk menciptakan perilaku dinamis.

#### Menghasilkan Fungsi Penambah

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Mengembalikan 15
```

- `make-adder` menghasilkan fungsi lambda baru yang menambahkan nomor tertentu (`n`).
- Lambda yang dikembalikan disimpan di `add5`, yang menambahkan `5` ke inputnya.

#### Menggunakan Lambda dengan `let`

Lambda sering digunakan dengan `let` untuk membuat fungsi sementara dengan cakupan lokal.

#### Lambda Lokal untuk Tambahan

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Mengembalikan 7
```

- `let` mengikat fungsi lambda ke nama `add`.
- Lambda kemudian digunakan sebagai fungsi normal dalam lingkup `let`.

#### Menggabungkan Lambdas dengan Fungsi Tingkat Tinggi

Lambda sangat berguna ketika dikombinasikan dengan fungsi tingkat tinggi untuk melakukan transformasi data yang kompleks.

#### Memfilter Bilangan Genap

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Mengembalikan (2 4 6)
```

- `lambda` memeriksa apakah suatu bilangan genap.
- Fungsi `filter` menggunakan lambda untuk hanya menyimpan bilangan genap dari daftar.

### Manfaat Fungsi Lambda

- **Keringkasan:** Lambda mengurangi kode boilerplate dengan menghilangkan kebutuhan untuk mendefinisikan fungsi bernama terpisah.
- **Fleksibilitas:** Anda dapat menentukan dan menggunakannya di mana pun diperlukan, sehingga membuat kode lebih modular.
- **Peningkatan Keterbacaan:** Untuk tugas yang singkat dan spesifik, lambda memperjelas maksudnya tanpa mengacaukan kode dengan fungsi bernama tambahan.

### Kapan Menggunakan Fungsi Lambda

Gunakan fungsi lambda ketika:

- Logikanya singkat dan mandiri.
- Fungsi tersebut hanya diperlukan sementara atau dalam lingkup tertentu.
- Anda bekerja dengan fungsi tingkat tinggi seperti `map`, `filter`, atau `reduce`.

Hindari penggunaan lambda untuk logika multi-baris yang rumit, karena hal ini dapat mengurangi keterbacaan. Untuk operasi yang lebih luas, gunakan fungsi bernama saja.

### Kesimpulan

Fungsi Lambda dalam Scheme menyediakan cara yang ringkas dan canggih untuk mendefinisikan fungsi anonim untuk tugas tertentu. Fleksibilitas dan kemudahan penggunaannya menjadikannya alat penting bagi pemrogram Scheme mana pun. Memahami cara menggunakan `lambda` secara efektif akan membantu Anda menulis skrip yang lebih bersih, lebih modular, dan efisien.