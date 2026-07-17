---
title: "Fungsi Variadik"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 514c81d0058b436609f97d1177e349ad6d7685ad6ccef15afaaa8ef9f137d852
translation_lock: true
url: "hub/scripting/fundamentals/Functions/variadic"
---
**Fungsi variadik** dalam Scheme adalah fungsi yang menerima sejumlah argumen yang bervariasi. Fungsi-fungsi ini sangat serbaguna dan memungkinkan Anda membuat kode yang fleksibel dan dapat digunakan kembali. Dalam pemrograman fungsional, fungsi variadik menyederhanakan operasi yang perlu memproses sejumlah masukan, seperti menjumlahkan daftar angka atau merangkai string.

Fungsi variadik sangat berguna ketika:

- Jumlah argumen tidak dapat ditentukan sebelumnya.
- Anda perlu menerapkan operasi yang sama ke daftar input dinamis.
- Menulis utilitas untuk agregasi atau transformasi data.

### Sintaks Fungsi Variadik

Fungsi variadik didefinisikan menggunakan simbol `.` sebelum nama parameter terakhir. Parameter terakhir ini mengumpulkan semua argumen yang tersisa ke dalam sebuah daftar.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Argumen tetap apa pun yang diperlukan dan diterima fungsi.
- **`variadic-parameter`:** Parameter khusus yang diawali dengan `.` yang mengumpulkan argumen tambahan sebagai daftar.
- **`body-expression`:** Logika dijalankan ketika fungsi dipanggil.

### Contoh Fungsi Variadik

#### Fungsi Variadik Dasar

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Penjelasan**:
  - `numbers` mengumpulkan semua argumen ke dalam daftar.
  - `apply` menerapkan fungsi `+` ke semua elemen daftar.

**Penggunaan**:
```scheme
(sum 1 2 3 4 5)  ; Mengembalikan 15
```

#### Fungsi Variadik dengan Parameter Tetap

Anda dapat menggabungkan parameter tetap dengan parameter variadik untuk membuat fungsi yang lebih fleksibel.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Penjelasan**:
  - `prefix` adalah argumen tetap.
  - `names` mengumpulkan argumen yang tersisa ke dalam daftar.
  - Setiap nama diawali dengan string yang diberikan menggunakan `map` dan `lambda`.

**Penggunaan**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Mengembalikan ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Menggabungkan Logika Tetap dan Variadik

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Penjelasan**:
  - `collection-name` adalah parameter tetap.
  - `items` mengumpulkan argumen tambahan ke dalam daftar.
  - Fungsi ini menggabungkan nama koleksi dan item menjadi satu string.

**Penggunaan**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Mengembalikan "Fruits: Apple, Banana, Cherry"
```

### Kasus Penggunaan Tingkat Lanjut

#### Memproses Input Sewenang-wenang

Fungsi variadik unggul dalam menangani data arbitrer. Berikut contoh penjumlahan bilangan positif saja:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Menyaring bilangan non-positif sebelum menjumlahkannya.

**Penggunaan**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Mengembalikan 18
```

#### Fungsi Variadik dengan Logika Rekursif

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Penjelasan**:
  - `first` menangani argumen pertama.
  - `rest` mengumpulkan argumen yang tersisa ke dalam daftar.
  - Secara rekursif menghitung nilai maksimum.

**Penggunaan**:
```scheme
(max-value 10 20 5 40 15)  ; Mengembalikan 40
```

### Manfaat Fungsi Variadik

- **Fleksibilitas:** Mereka menangani berbagai kasus masukan.
- **Keringkasan:** Mengurangi kebutuhan akan beberapa fungsi yang kelebihan beban.
- **Operasi Dinamis:** Aktifkan pemrosesan data waktu proses tanpa mengetahui jumlah argumen sebelumnya.

### Kapan Menggunakan Fungsi Variadik

Gunakan fungsi variadik ketika:

- Fungsi ini perlu memproses argumen dalam jumlah yang tidak diketahui.
- Satu operasi berlaku untuk semua input (misalnya, menjumlahkan, menggabungkan, atau memetakan).
- Menyederhanakan logika tingkat tinggi dengan argumen dinamis.

Hindari fungsi variadik ketika:

- Validasi input atau pengecekan tipe itu rumit.
- Argumen tetap cukup untuk logika yang diperlukan.
- Keterbacaan terganggu karena pengoperasian yang terlalu rumit.

### Kesimpulan

Fungsi variadik dalam Scheme menyediakan mekanisme yang kuat untuk menangani masukan dinamis. Dengan memahami sintaksis dan penggunaannya, Anda dapat membuat skrip yang fleksibel dan kuat yang beradaptasi dengan berbagai skenario. Dikombinasikan dengan fungsi tingkat tinggi, fungsi variadik membuat kode Anda lebih ringkas dan ekspresif.