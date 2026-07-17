---
title: "mendefinisikan"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: 1a62a7c4204b91c6988f897e400b577ef45a01ca57b06789e5f72591c5196144
translation_lock: true
url: "hub/scripting/fundamentals/Variables and Scope/define"
---
Pernyataan `define` dalam Scheme adalah konstruksi serbaguna yang digunakan untuk membuat pengikatan global atau lokal. Ini paling sering digunakan untuk mendefinisikan variabel dan fungsi, menjadikannya dapat digunakan kembali dan diakses di seluruh skrip atau dalam cakupan tertentu. Memahami `define` sangat penting untuk menulis program Scheme yang modular, dapat digunakan kembali, dan mudah dibaca.

### Tujuan `define`

Konstruksi `define` memiliki berbagai tujuan:
- **Mendefinisikan Variabel**: Memberikan nilai pada nama variabel, sehingga tersedia untuk digunakan nanti.
- **Mendefinisikan Fungsi**: Membuat prosedur yang dapat digunakan kembali yang merangkum logika tertentu.
- **Definisi Lokal**: Saat digunakan dalam suatu fungsi, `define` membuat pengikatan lokal yang tidak memengaruhi namespace global.

---

### Mendefinisikan Variabel dengan `define`

Kegunaan dasar `define` adalah untuk membuat variabel yang memiliki nilai konstan atau nilai yang dihitung.

#### Sintaks

```scheme
(define variable-name value)
```

#### Contoh: Mendefinisikan Konstanta

```scheme
(define pi 3.14159)
(* pi 2) ;; Menghitung 2π
```

**Hasil**: `6.28318`

---

### Mendefinisikan Fungsi dengan `define`

Anda dapat menggunakan `define` untuk membuat prosedur yang dapat digunakan kembali.

#### Sintaks

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Contoh: Mendefinisikan Fungsi Sederhana

```scheme
(define (square x)
  (* x x))
(square 4) ;; Menghitung 4²
```

**Hasil**: `16`

---

### Definisi Lokal dengan `define`

Saat digunakan di dalam suatu fungsi, `define` membuat pengikatan lokal yang hanya dapat diakses dalam fungsi yang melingkupinya. Hal ini menghindari polusi pada namespace global dan membantu mengatur kode Anda.

#### Contoh: Fungsi Pembantu Lokal

```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Fungsi pembantu lokal
  (define (cube x) (* x x x))  ;; Fungsi pembantu lokal
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Hasil**: `41` (Menghitung \(2^2 + 3^3 + 4^2\))

---

### Fitur Utama `define`

1. **Cakupan Global atau Lokal**:
   - Saat digunakan di tingkat atas, `define` membuat variabel atau fungsi global.
   - Saat digunakan di dalam fungsi lain, `define` membuat pengikatan lokal.

2. **Dapat digunakan kembali**:
   - Fungsi yang ditentukan dengan `define` dapat digunakan kembali beberapa kali dalam konteks berbeda.

3. **Peningkatan Keterbacaan**:
   - Memecah logika menjadi fungsi yang lebih kecil dan diberi nama yang baik akan meningkatkan kejelasan dan pemeliharaan kode Anda.

---

### Perbedaan Antara `define` dan `let`

| **Aspek** | **`define`** | **`let`** |
|----------|--------------------------------------------------|----------------------------------------|
| **Tujuan** | Membuat pengikatan global atau lokal untuk variabel atau fungsi. | Membuat pengikatan sementara dalam lingkup lokal. |
| **Ruang Lingkup** | Global ketika berada di level teratas; lokal ketika berada di dalam fungsi lain. | Selalu bersifat lokal pada blok `let`.       |
| **Dapat digunakan kembali** | Fungsi dan variabel dapat digunakan kembali di banyak tempat. | Variabel terikat sementara untuk satu blok. |
| **Sintaks** | Mendefinisikan variabel atau fungsi secara eksplisit.       | Menggabungkan pengikatan variabel dengan evaluasi ekspresi. |