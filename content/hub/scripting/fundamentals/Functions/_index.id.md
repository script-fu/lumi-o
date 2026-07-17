---
title: "Fungsi"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a1808e88698d7f38626bf136806af5388132ed2799927b899141c749dac679a3
translation_lock: true
url: "hub/scripting/fundamentals/Functions/_index"
---
Fungsi adalah konsep inti dalam Scheme, yang menyediakan sarana untuk merangkum logika, memungkinkan penggunaan kembali kode, dan menyusun skrip Anda secara efektif. Dengan fungsi, Anda dapat membuat skrip modular dan dapat dipelihara yang menangani berbagai tugas, mulai dari operasi dasar hingga alur kerja tingkat lanjut di Lumi.

Bagian ini berfungsi sebagai pengenalan fungsi dalam Scheme dan meletakkan dasar untuk memahami jenis, definisi, dan kegunaannya. Bagian selanjutnya akan mempelajari lebih dalam jenis fungsi tertentu dan kemampuan uniknya.

## Sintaks dan Ekspresi Minimal

Kode skema terbuat dari **ekspresi**. Sebuah ekspresi mengevaluasi suatu nilai. Sintaksnya seragam: tanda kurung membentuk panggilan, dengan nama operator atau fungsi terlebih dahulu.

```scheme
(+ 1 2)         ; Menjumlahkan 1 dan 2, hasilnya 3
(if #t 1 0)     ; Mengevaluasi ke 1 karena kondisinya benar
(list 1 2 3)    ; Membuat daftar: (1 2 3)
```

Karena semuanya merupakan ekspresi, aliran kontrol secara alami cocok dengan gaya yang sama seperti pemanggilan fungsi.

## Mengapa Fungsi Penting

Fungsi memainkan peran penting dalam Scheme karena beberapa alasan:

- **Kemampuan Penggunaan Kembali Kode:** Hindari pengulangan dengan merangkum logika ke dalam komponen yang dapat digunakan kembali.
- **Modularitas:** Bagi tugas-tugas kompleks menjadi bagian-bagian yang lebih kecil dan mudah dikelola.
- **Perilaku Dinamis:** Menerima parameter untuk menangani berbagai masukan atau beradaptasi dengan situasi berbeda.
- **Abstraksi Tinggi:** Sederhanakan logika dengan berfokus pada "apa" yang dilakukan suatu fungsi, bukan "bagaimana" fungsinya.

## Ikhtisar Tipe Fungsi

Scheme menawarkan berbagai konstruksi fungsi, masing-masing disesuaikan dengan kasus penggunaan tertentu:

1. **Fungsi Bernama**
   Ini adalah fungsi standar yang ditentukan dengan `define`. Mereka merupakan tulang punggung sebagian besar skrip.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Fungsi Anonim**
   Juga dikenal sebagai **fungsi lambda**, ini adalah fungsi tanpa nama yang didefinisikan secara inline untuk penggunaan satu kali saja.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Fungsi Tingkat Tinggi**
   Fungsi yang menggunakan fungsi lain sebagai argumen atau mengembalikan fungsi sebagai hasil, memungkinkan abstraksi yang kuat seperti pemetaan, pemfilteran, dan pengurangan.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Mengembalikan (1 4 9 16)
   ```

## Sintaks Umum untuk Fungsi

Fungsi dalam Scheme memiliki sintaks yang sederhana dan konsisten:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Nama fungsinya.
- **`parameter1, parameter2, ...`:** Argumen yang digunakan fungsi.
- **`body-expression`:** Logika dijalankan ketika fungsi dipanggil.

Contoh:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Mengembalikan 8
```

## Efek Samping dan Keadaan Global

Di Lumi, banyak prosedur berguna yang memiliki **efek samping**: prosedur tersebut memodifikasi gambar, mengubah sumber daya dapat digambar, menulis file, atau menampilkan keluaran.

- Pisahkan efek samping dalam prosedur kecil yang disebutkan dengan jelas.
- Hindari mengubah konteks global kecuali diperlukan.
- Saat Anda mengubah konteks (warna, kuas, dll), bungkus karya dengan `lumi-context-push` dan `lumi-context-pop` sehingga status pengguna dipulihkan.