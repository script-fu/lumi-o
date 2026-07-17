---
title: "peta"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/map"
---
Fungsi `map` dalam Scheme digunakan untuk menerapkan prosedur ke setiap elemen daftar (atau beberapa daftar) dan **mengembalikan daftar baru** yang berisi hasilnya. Ini membuatnya ideal untuk mengubah data.

Bentuk paling sederhana dari `map` terlihat seperti ini:

```scheme
(map procedure list)
```

- **Prosedur**: Fungsi yang diterapkan ke setiap elemen daftar.
- **Daftar**: Daftar yang elemennya akan diubah.

---

### Contoh: Gandakan Setiap Elemen

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Di sini, fungsi `double` diterapkan ke setiap elemen daftar `(1 2 3 4)`.
- Hasilnya adalah daftar baru dengan setiap elemen digandakan.

**Keluaran**: `(2 4 6 8)`

---

### Cara Kerjanya

1. **Membuat Daftar Baru**:
   - `map` menerapkan prosedur yang disediakan untuk setiap elemen daftar dan mengumpulkan hasilnya ke dalam daftar baru.

2. **Mengubah Data**:
   - Ini terutama digunakan untuk transformasi data daripada melakukan efek samping.

---

#### Contoh: Menggunakan dengan Banyak Daftar

Jika beberapa daftar disediakan, `map` memproses elemen terkait dari setiap daftar.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- Fungsi `sum` menambahkan elemen terkait dari dua daftar dan mengembalikan hasilnya sebagai daftar baru.

**Keluaran**: `(5 7 9)`

---

### Ringkasan

- Fungsi `map` adalah alat yang ampuh untuk mengubah daftar dengan menerapkan prosedur ke setiap elemen.
- Berbeda dengan `for-each`, `map` **menghasilkan daftar baru** yang berisi hasil penerapan prosedur.
- Mendukung banyak daftar, memungkinkan operasi berdasarkan elemen di seluruh daftar tersebut.

Dengan menggunakan `map`, Anda dapat membuat versi data yang diubah secara efisien sambil menjaga daftar asli tidak berubah.