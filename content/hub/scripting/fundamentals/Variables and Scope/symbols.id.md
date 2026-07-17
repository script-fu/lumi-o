---
title: "Simbol"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 4153c94fca6fa6c5e1e98ac9449a9e7bd9cdc5b9e5dc4b96da5d8d1e8de3df43
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/symbols"
---
Simbol adalah salah satu tipe data inti dalam Scheme, yang mewakili pengidentifikasi unik dan tidak dapat diubah. Mereka terutama digunakan sebagai kunci, penanda, atau pengganti dalam program, menjadikannya penting untuk menulis kode yang bersih dan ekspresif.

Simbol dalam Scheme mirip dengan string tetapi berbeda karena simbolnya **unik** dan **atomik**. Ini berarti dua simbol dengan nama yang sama dijamin merupakan objek yang sama, memungkinkan pemeriksaan kesetaraan yang cepat dan penggunaan yang efisien dalam struktur data.

### Sintaks

Sebuah simbol ditulis sebagai rangkaian karakter:

- Diawali dengan huruf, diikuti huruf, angka, atau karakter khusus seperti `-`, `+`, atau `*`.
- Simbol peka huruf besar-kecil secara default.

Contoh:

```scheme
'hello       ; Simbol bernama `hello`
'foo-bar     ; Simbol bernama `foo-bar`
'*special*   ; Simbol bernama `*special*`
```

## Membuat Simbol

Simbol biasanya dibuat menggunakan operator **quote** (`'`), yang memberitahu Scheme untuk memperlakukan nama sebagai simbol daripada mengevaluasinya sebagai variabel atau fungsi.

### Contoh

```scheme
'my-symbol   ; Membuat simbol `my-symbol`
```

Anda juga dapat membuat simbol secara terprogram menggunakan prosedur `string->symbol`, yang mengubah string menjadi simbol.

```scheme
(string->symbol "dynamic-symbol")
```

**Hasil**: `'dynamic-symbol`


## Membandingkan Simbol

Karena simbol bersifat unik, Anda dapat membandingkannya secara efisien menggunakan `eq?`.

### Contoh

```scheme
(eq? 'apple 'apple)   ; #t (simbol yang sama)
(eq? 'apple 'orange)  ; #f (simbol berbeda)
```

Hal ini membuat simbol ideal untuk digunakan sebagai kunci dalam struktur data atau penanda dalam kode Anda.

## Menggunakan Simbol

Simbol sering digunakan dalam Scheme untuk:

1. **Kunci dalam Daftar Asosiasi:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Mengembalikan (name . "Alice")
```

2. **Pengidentifikasi dalam Kode:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Prosedur Bekerja dengan Simbol

Scheme menyediakan prosedur bawaan untuk bekerja dengan simbol:

| Prosedur | Deskripsi |
|----------------------------------||-----------------------------------------------------------------------------|
| **`symbol?`** | Memeriksa apakah suatu objek adalah simbol.                                            |
| **`eq?`** | Membandingkan dua simbol untuk identitas (perbandingan cepat).                       |
| **`symbol->string`** | Mengonversi simbol menjadi string (berguna untuk tampilan atau debugging).          |
| **`string->symbol`** | Mengonversi string menjadi simbol (berguna untuk pembuatan pengidentifikasi dinamis). |

### Contoh

```scheme
(symbol? 'example)            ; #t (benar: ini simbol)
(symbol->string 'example)     ; "contoh"
(string->symbol "new-symbol") ; 'simbol baru
```

## Ringkasan

Simbol adalah cara yang ringan dan efisien untuk mewakili pengidentifikasi, kunci, dan penanda dalam Scheme. Kekekalan dan pemeriksaan identitasnya yang cepat menjadikannya ideal untuk banyak tugas pemrograman. Memahami cara menggunakan simbol secara efektif akan meningkatkan kemampuan Anda menulis kode Scheme yang bersih dan ekspresif.