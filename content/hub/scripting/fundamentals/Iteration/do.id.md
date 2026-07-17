---
title: "Mengerjakan"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/do"
---
Fungsi `do` dalam Scheme adalah mekanisme perulangan yang memungkinkan iterasi dengan kondisi inisialisasi, pembaruan, dan penghentian. Hal ini sangat berguna ketika Anda perlu melakukan serangkaian operasi beberapa kali atau hingga suatu kondisi terpenuhi.

Bentuk umum `do` adalah:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variabel**: Variabel loop.
- **Nilai awal**: Nilai awal setiap variabel loop.
- **Ekspresi pembaruan**: Ekspresi untuk memperbarui variabel loop di akhir setiap iterasi.
- **Kondisi penghentian**: Kondisi untuk menghentikan perulangan.
- **Ekspresi-hasil**: Nilai yang dikembalikan saat loop berakhir.
- **Body**: Kode yang akan dieksekusi di setiap iterasi.

---

### Contoh: Jumlahkan Angka dari 1 sampai 5

```scheme
(do ((i 1 (+ i 1))      ; Inisialisasi i ke 1, tambah 1
     (sum 0 (+ sum i))) ; Inisialisasi jumlah ke 0, tambahkan i ke jumlah
    ((> i 5) sum)       ; Berhenti ketika i > 5, kembalikan jumlah
  (lumi-message (number->string sum))) ; Mencetak jumlah di setiap langkah
```

- Variabel loop `i` dimulai dari 1 dan bertambah 1 di setiap iterasi.
- Variabel `sum` mengakumulasi jumlah `i`.
- Perulangan berakhir ketika `i > 5`, mengembalikan nilai akhir `sum`.

**Keluaran**: `15`

---

### Cara Kerjanya

1. **Inisialisasi**:
   - Setiap variabel loop diberi nilai awalnya.

2. **Pemeriksaan Pengakhiran**:
   - Pada awal setiap iterasi, kondisi terminasi diperiksa. Jika benar, perulangan berhenti dan ekspresi hasil dievaluasi.

3. **Iterasi**:
   - Jika kondisi penghentian salah, isi dieksekusi, dan variabel loop diperbarui menggunakan ekspresi pembaruan masing-masing.

---

### Ringkasan

- Konstruk `do` menyediakan cara yang fleksibel untuk mengimplementasikan loop dengan banyak variabel dan kondisi terminasi yang kompleks.
- Berguna untuk tugas yang memerlukan pembaruan status di seluruh iterasi.
- Kondisi terminasi menentukan kapan loop berakhir dan dapat mengembalikan hasil akhir.

Dengan menggunakan `do`, Anda dapat mengimplementasikan algoritma berulang dalam Scheme dengan kontrol yang tepat atas inisialisasi, pembaruan, dan penghentian. Hal ini menjadikan `do` kombinasi **mekanisme pengikatan tercakup** (seperti `let`) dan **struktur kontrol berulang**, yang memungkinkannya menangani perulangan dan keadaan sementara dengan cara yang bersih dan ringkas.