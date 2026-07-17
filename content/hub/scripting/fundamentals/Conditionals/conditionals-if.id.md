---
title: "jika"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
translation_lock: true
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
Dalam bentuknya yang paling sederhana, kondisi `if` dalam Scheme mengevaluasi pengujian dan, berdasarkan hasilnya, mengeksekusi salah satu dari dua kemungkinan blok kode. Bentuk paling sederhana terlihat seperti ini:

```scheme
(if test-is-true
  do-this)
```

- Jika `test` bernilai true (`#t`), **blok kode pada konsekuensinya** akan dieksekusi. Blok tersebut dapat mengembalikan nilai atau melakukan tindakan lain, seperti menetapkan variabel atau mencetak keluaran.

### Contoh

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Dalam hal ini, `test` adalah `(< 0 1)` (memeriksa apakah 0 kurang dari 1).
- Karena pengujian bernilai benar (`#t`), blok kode `(lumi-message "True!")` dijalankan, yang mencetak `"True!"`.

### Menambahkan Cabang Alternatif: `if-else`

Saat menggunakan kondisional `if` dengan blok kode alternatif (kasus `else`), strukturnya terlihat seperti ini:

```scheme
(if test
  do-this
  else-do-this)
```

- Jika `test` bernilai true (`#t`), blok kode **konsekuen** akan dieksekusi.
- Jika `test` bernilai false (`#f`), blok kode **alternatif** akan dijalankan.

```scheme
(if test
  consequent
  alternative)
```

### Cara Kerjanya

1. **Uji Ekspresi**:
   - Ekspresi `test` dievaluasi terlebih dahulu.

2. **Hasil Berdasarkan Tes**:
   - Jika `test` bernilai benar (`#t`), **blok kode konsekuensi** akan dieksekusi.
   - Jika `test` bernilai false (`#f`), **blok kode alternatif** akan dijalankan.

Baik blok kode `consequent` dan `alternative` dapat melakukan operasi Scheme apa pun yang valid, termasuk mengembalikan nilai, memodifikasi variabel, atau menjalankan prosedur.

### Contoh

#### Contoh 1: Mengembalikan Nilai

```scheme
(if (< 0 1)
  1
  0)
```

- Di sini, `test` adalah `(< 0 1)` (memeriksa apakah 0 kurang dari 1).
- Karena pengujian bernilai benar (`#t`), blok **konsekuen** (`1`) dijalankan dan nilainya dikembalikan.

Hasil: **1**

#### Contoh 2: Mengevaluasi Blok awal

Jika Anda perlu melakukan beberapa tindakan ketika kondisinya benar atau salah, Anda dapat menggunakan `begin` atau `let` untuk mengelompokkannya.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Dalam contoh ini, `test` adalah `(= 0 1)` (memeriksa apakah 0 sama dengan 1).
- Karena pengujian bernilai false (`#f`), blok **alternatif** dijalankan:
  - Pertama, mencetak `"False condition met, calculating..."`.
  - Kemudian menghitung `(* 3 4)` dan mengembalikan `12`.

Hasil: **Mencetak "Kondisi salah terpenuhi, menghitung..." dan mengembalikan 12.**

#### Contoh 3: Mengevaluasi Pernyataan let

Menggunakan `let` memungkinkan kita mendeklarasikan variabel cakupan lokal di dalam blok kode.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Dalam contoh ini, `test` adalah `(= 1 1)` (memeriksa apakah 1 sama dengan 1).
- Karena pengujian bernilai true (`#t`), blok **consequent** dijalankan:
  - Pertama, mencetak `"True condition met, calculating..."`.
  - Kemudian menghitung `(* -1 10)` dan mengembalikan `-10`.

Hasil: **Mencetak "Kondisi sebenarnya terpenuhi, menghitung..." dan mengembalikan -10.**

### Ringkasan

- Kondisional `if` adalah alat yang ampuh dalam Scheme untuk mengevaluasi pengujian dan mengeksekusi blok kode yang sesuai.

- Dapat menangani ekspresi sederhana dan blok kode kompleks yang mengembalikan nilai, mengubah variabel, atau melakukan efek samping.
- Ingat: Jika tidak ada blok `else` yang eksplisit, `if` hanya mengevaluasi dan mengeksekusi **konsekuensi** jika pengujiannya benar. Jika tidak, ia akan mengevaluasi dan mengeksekusi **alternatif**.