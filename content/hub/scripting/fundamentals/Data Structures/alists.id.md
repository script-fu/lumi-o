---
title: "Daftar Asosiasi (Alist)"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/alists"
---
**daftar asosiasi** (atau **alist**) adalah struktur data mendasar dalam Scheme yang digunakan untuk mewakili kumpulan pasangan nilai kunci. Ini diimplementasikan sebagai daftar pasangan, di mana setiap pasangan mengasosiasikan sebuah kunci (biasanya simbol) dengan sebuah nilai. Daftarnya sederhana, fleksibel, dan cocok untuk kumpulan data berukuran kecil hingga menengah.

### Struktur Daftar Asosiasi

Alist adalah daftar yang setiap elemennya merupakan **pasangan** (dibangun dengan `cons`). Setiap pasangan terdiri dari:

- **Kunci**: Elemen pertama (biasanya simbol).
- **Nilai**: Elemen kedua, yang dapat berupa tipe data apa pun.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Kunci**: `'name`, `'age`, `'city`
- **Nilai**: `"Alice"`, `30`, `"Paris"`
- **Struktur**: Daftar pasangan:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Membuat Daftar

Anda dapat membuat daftar dengan membuat pasangan secara manual atau membuatnya secara terprogram menggunakan `cons`.

#### Menggunakan Kutipan Tunggal (`'`)

Kutipan tunggal (`'`) adalah singkatan dari **quoting**, yang mencegah Scheme mengevaluasi ekspresi. Hal ini membuatnya ideal untuk membuat daftar statis di mana semua kunci dan nilai di-hardcode.

```scheme
;; Mendefinisikan alist secara manual
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Menambahkan pasangan baru secara programatis
(define updated-alist (cons '(country . "France") alist))
```

**Hasil**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Menggunakan kutip balik (`` ` ``) dan koma (`,`)

Operator kutipan balik (“` ` ``) mirip dengan kutipan tunggal, tetapi memungkinkan penyisipan ekspresi yang dievaluasi secara dinamis menggunakan koma (`,`). Ini berguna untuk membuat daftar tempat kunci atau nilai dihitung saat runtime.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Hasil**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Contoh Perbandingan

Daftar statis menggunakan `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Daftar dinamis menggunakan `` ` `` dan `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Mengakses Data di Alist

Untuk mengambil nilai dari alist, Anda dapat menggunakan fungsi `assoc`, yang mencari pasangan berdasarkan kuncinya.

```scheme
(assoc 'name alist)   ; Mengembalikan (name . "Alice")
(assoc 'country alist) ; Mengembalikan #f (kunci tidak ditemukan)
```

### Mengekstraksi Nilai

Setelah Anda mengambil pasangan menggunakan `assoc`, gunakan `cdr` untuk mengekstrak nilainya:

```scheme
(cdr (assoc 'name alist))   ; Mengembalikan "Alice"
```

### Ringkasan Fitur Utama

- **Kutipan Tunggal (`'`)**: Membuat daftar statis yang semua elemennya adalah data literal.
- **Kutipan balik (`` ` ``)**: Memungkinkan pembuatan alist dinamis dengan mencampur elemen statis dan ekspresi yang dievaluasi (menggunakan `,`).
- **Notasi Titik (`.`)**: Digunakan untuk membuat pasangan, mengaitkan kunci dengan nilai dalam daftar.