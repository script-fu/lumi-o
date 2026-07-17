---
title: "untuk masing-masing"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/for-each"
---
Fungsi `for-each` dalam Scheme digunakan untuk menerapkan prosedur ke setiap elemen daftar (atau beberapa daftar). Tidak seperti `map`, yang mengembalikan daftar baru beserta hasilnya, `for-each` digunakan untuk **efek samping**, seperti mencetak atau memperbarui variabel.

Bentuk paling sederhana dari `for-each` terlihat seperti ini:

```scheme
(for-each procedure list)
```

- **Prosedur**: Fungsi yang diterapkan ke setiap elemen daftar.
- **Daftar**: Daftar yang elemennya akan diproses.

---

### Contoh: Cetak Daftar

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Di sini, fungsi `print-item` diterapkan ke setiap elemen daftar `(1 2 3 4)`.
- Hal ini menyebabkan setiap nomor dicetak secara berurutan.

**Keluaran**: `1 2 3 4`

---

### Cara Kerjanya

1. **Iterasi pada Setiap Elemen**:
   - Prosedur yang disediakan dijalankan untuk setiap elemen dalam daftar, secara berurutan.

2. **Melakukan Efek Samping**:
   - Efek samping yang umum termasuk pencetakan, pencatatan, atau modifikasi variabel eksternal. Berbeda dengan `map`, `for-each` tidak mengembalikan daftar baru.

---

#### Contoh: Menggunakan dengan Banyak Daftar

Jika beberapa daftar disediakan, `for-each` memproses elemen terkait dari setiap daftar.

```scheme
(define (sum-and-print x y)
  (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- Fungsi `sum-and-print` menjumlahkan elemen terkait dari dua daftar dan mencetak hasilnya.

**Keluaran**: `5 7 9`

---

### Ringkasan

- Fungsi `for-each` berguna untuk melakukan efek samping pada setiap elemen daftar.
- Berbeda dengan `map`, `for-each` tidak membuat daftar baru—daftar ini hanya berfokus pada efek samping prosedur.
- Dapat menangani banyak daftar secara bersamaan, menerapkan prosedur ke elemen terkait.

Dengan menggunakan `for-each`, Anda dapat memproses daftar secara efektif ketika tujuannya adalah melakukan tindakan, bukan mengubah data.