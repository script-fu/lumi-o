---
title: "Daftar"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: caf60dbd4ddbab418dd6779d9efba0217982d37086ed8d485680b96142d5ef6f
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/lists"
---
Dalam Scheme, **daftar** adalah struktur data mendasar yang digunakan untuk mengelompokkan nilai. Daftar adalah kumpulan elemen yang diurutkan di mana setiap elemen dapat bertipe apa pun, termasuk daftar lainnya. Daftar banyak digunakan dalam Scheme untuk penyimpanan data dan struktur program.

### Contoh 1: Daftar Sederhana

```scheme
(list 1 2 3)
```

- Membuat daftar tiga elemen: `1`, `2`, dan `3`.

Hasil: **`(1 2 3)`**

---

#### Mengakses Elemen Daftar

Elemen dalam daftar diakses menggunakan prosedur `car` dan `cdr`:

- `car` mengambil elemen pertama dari daftar.
- `cdr` mengambil sisa daftar (semuanya kecuali elemen pertama).

#### Contoh

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Mengambil elemen pertama
(cdr my-list)  ; Mengambil sisa daftar
```

Hasil:

- `(car my-list)` kembali `1`
- `(cdr my-list)` kembali `(2 3)`

---

#### Rekursi Sederhana: Iterasi Melalui Daftar

Dengan memanggil `car` pada `cdr` daftar secara rekursif, Anda dapat memproses setiap elemen satu per satu hingga daftar tersebut dilintasi. Ini membentuk dasar dari banyak algoritma pemrosesan daftar.

#### Contoh: Mencetak Setiap Elemen Daftar

Berikut fungsi rekursif sederhana untuk mencetak setiap elemen dalam daftar:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Mencetak elemen pertama
      (print-elements (cdr lst)))))             ;; Memproses sisa daftar
```

- **Kasus Dasar:** Jika daftar kosong (`null? lst`), hentikan rekursi.
- **Kasus Rekursif:** Cetak elemen pertama (`car lst`), lalu panggil fungsi di daftar lainnya (`cdr lst`).

#### Contoh Penggunaan

```scheme
(print-elements (list 1 2 3))
```

Keluaran:

- `"1"`
- `"2"`
- `"3"`

Hasil: "selesai"

---

#### Cara Kerjanya

1. Fungsi ini mengambil elemen pertama dari daftar menggunakan `car` dan memprosesnya.
2. Kemudian memanggil dirinya sendiri dengan daftar lainnya (`cdr`).
3. Proses ini berulang hingga daftar kosong (`null? lst`).

---

### Contoh 2: Tipe Campuran

Daftar dapat menyertakan elemen dengan tipe berbeda, termasuk string, boolean, angka, daftar lain, atau bahkan hasil ekspresi:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Ini membuat daftar dengan:
  - Nomor (`42`)
  - Sebuah string (`"hello"`)
  - Boolean (`#t`)
  - Daftar lainnya (`(1 2)`)
  - Hasil ekspresi (`(+ 3 4)`, yang dievaluasi menjadi `7`)

Hasil: **`(42 "hello" #t (1 2) 7)`**

---

Contoh-contoh ini menunjukkan keserbagunaan daftar dalam Scheme, menjadikannya alat yang ampuh untuk mengatur dan memanipulasi data.

### Membuat Daftar

Prosedur `cons` digunakan untuk membuat daftar baru dengan menggabungkan elemen dengan daftar yang sudah ada.

```scheme
(cons new-element existing-list)
```

#### Contoh

```scheme
(cons 0 (list 1 2 3))
```

- Tambahkan `0` ke awal daftar `(1 2 3)`.

Hasil: **`(0 1 2 3)`**

---

### Memeriksa Daftar

Prosedur `list?` memeriksa apakah nilai yang diberikan merupakan daftar.

```scheme
(list? value)
```

#### Contoh: daftar?

```scheme
(list? (list 1 2 3))  ; Memeriksa apakah (list 1 2 3) adalah daftar
(list? 42)            ; Memeriksa apakah 42 adalah daftar
```

Hasil:

- `(list? (list 1 2 3))` mengembalikan `#t` (benar)
- `(list? 42)` mengembalikan `#f` (salah)

---

### Operasi pada Daftar

Scheme menyediakan beberapa prosedur bawaan untuk bekerja dengan daftar, termasuk:

- `length`: Mengembalikan jumlah elemen dalam daftar.
- `append`: Menggabungkan dua daftar atau lebih menjadi satu.
- `reverse`: Mengembalikan daftar baru dengan elemen dalam urutan terbalik.

```scheme
(length (list 1 2 3))          ; Mengembalikan 3
(append (list 1 2) (list 3 4)) ; Mengembalikan (1 2 3 4)
(reverse (list 1 2 3))         ; Mengembalikan (3 2 1)
```

Hasil:

- `(length (list 1 2 3))` kembali `3`
- `(append (list 1 2) (list 3 4))` kembali `(1 2 3 4)`
- `(reverse (list 1 2 3))` kembali `(3 2 1)`

#### Menggunakan `list-ref`

Prosedur `list-ref` mengambil elemen pada indeks daftar yang ditentukan (indeks berbasis nol).

```scheme
(list-ref lst index)
```

- **`lst`**: Daftar tempat mengambil elemen.
- **`index`**: Indeks berbasis nol yang menunjukkan elemen mana yang akan dikembalikan.

##### Contoh: daftar-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Mengambil elemen pada indeks 2
```

Hasil: `30`

---

### Daftar Bersarang

Daftar dalam Scheme dapat berisi daftar lain sebagai elemen, sehingga menciptakan struktur bersarang.

#### Contoh: Membuat Daftar Bersarang

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Membuat daftar tiga elemen, yang masing-masing merupakan daftar itu sendiri.

Hasil: **`((1 2) (3 4) (5))`**

---

#### Mengakses Data Bersarang

Untuk mengakses elemen dalam daftar bertingkat, Anda dapat menggunakan kombinasi `car` dan `cdr` untuk menavigasi struktur.

#### Contoh: Mengakses Elemen

```scheme
(car nested-list)              ; Mengambil elemen pertama: (1 2)
(car (car nested-list))        ; Mengambil elemen pertama subdaftar pertama: 1
(cdr (car nested-list))        ; Mengambil sisa subdaftar pertama: (2)
(car (cdr (car nested-list)))  ; Mengambil elemen kedua subdaftar pertama: 2
```

---

#### Penjelasan

1.**`car nested-list`**:
   - Mengambil elemen pertama `nested-list`, yaitu `(1 2)`.

2.**`car (car nested-list)`**:
   - Mengambil elemen pertama `(1 2)`, yaitu `1`.

3.**`cdr (car nested-list)`**:
   - Mengambil sisa `(1 2)`, yaitu `(2)`.

4.**`car (cdr (car nested-list))`**:
   - Mengambil elemen pertama `(2)`, yaitu `2`.

---

#### Contoh: Mengakses Elemen dari Sublist Lain

```scheme
(car (cdr nested-list))        ; Mengambil subdaftar kedua: (3 4)
(car (car (cdr nested-list)))  ; Mengambil elemen pertama subdaftar kedua: 3
```

---

Pendekatan ini memungkinkan Anda menavigasi dan mengakses elemen tertentu secara sistematis dalam daftar bertingkat, memberikan fleksibilitas yang kuat untuk bekerja dengan data hierarki.

### Ringkasan

- **Daftar** dalam Scheme adalah struktur data yang serbaguna dan penting.
- Gunakan `list` untuk membuat daftar, `car` dan `cdr` untuk mengakses elemen, dan `cons` untuk membuat daftar.
- Prosedur bawaan seperti `length`, `append`, `reverse`, dan `list-ref` membuat pengoperasian daftar menjadi mudah dan efisien.
- Daftar dapat disarangkan, memungkinkan struktur data yang kompleks untuk kasus penggunaan tingkat lanjut.