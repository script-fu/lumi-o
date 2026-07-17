---
title: "Rekursi Sederhana"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/recursion"
---
Rekursi adalah konsep yang kuat dalam Scheme, di mana suatu fungsi memanggil dirinya sendiri untuk menyelesaikan sub-masalah yang lebih kecil dari masalah aslinya. Pola **rekursi sederhana** melibatkan kasus dasar untuk menghentikan rekursi dan kasus rekursif untuk mengurangi masalah.

Struktur umum fungsi rekursif terlihat seperti ini:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Kondisi Dasar**: Menghentikan rekursi.
- **Hasil Dasar**: Nilai yang dikembalikan ketika kondisi dasar terpenuhi.
- **Panggilan Rekursif**: Panggilan ke fungsi itu sendiri dengan argumen yang dimodifikasi yang mendekatkan komputasi ke kasus dasar.

---

### Contoh: Jumlah Bilangan (1 sampai n)

Fungsi rekursif sederhana untuk menghitung jumlah angka dari 1 sampai n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Kasus dasar: berhenti ketika n adalah 0
    0                          ; Hasil dasar: jumlahnya 0
    (+ n (sum-to-n (- n 1))))) ; Panggilan rekursif: jumlahkan n saat ini dengan hasil masalah yang lebih kecil
```

---

#### Cara Kerja : Meruntuhkan dan Merakit Kembali

Rekursi bekerja dengan memecah masalah awal menjadi bagian-bagian yang lebih kecil. Setiap panggilan ke fungsi menangani satu bagian dan meneruskan sisanya. Setelah kasus paling sederhana tercapai, hasilnya disusun kembali saat penghitungan selesai.

#### Jejak Langkah-demi-Langkah dari penjumlahan ke-n 3

1. **Panggilan Awal**: *jumlah-ke-n 3*
   → *(+ 3 (jumlah-ke-n 2))*

2. **Panggilan Kedua**: *jumlah-ke-n 2*
   → *(+ 2 (jumlah-ke-n 1))*

3. **Panggilan Ketiga**: *jumlah-ke-n 1*
   → *(+ 1 (jumlah ke-n 0))*

4. **Kasus Dasar**: *jumlah-ke-n 0*
   → *0*

---

#### Merakit Kembali Hasil Akhir

Setelah kasus paling sederhana diselesaikan, setiap lapisan komputasi selesai:

1. *jumlah-ke-n 0* menghasilkan *0*
2. *jumlah ke-n 1* menjadi *(+ 1 0) = 1*
3. *jumlah ke-n 2* menjadi *(+ 2 1) = 3*
4. *jumlah ke-n 3* menjadi *(+ 3 3) = 6*

---

### Contoh: Mencetak Setiap Elemen Daftar

Berikut fungsi rekursif sederhana untuk mencetak setiap elemen dalam daftar:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Mencetak elemen pertama
      (print-elements (cdr lst)))))             ; Memproses sisa daftar
```

- **Kasus Dasar:** Jika daftar kosong (*null? lst*), hentikan rekursi.
- **Kasus Rekursif:** Cetak elemen pertama (*car lst*), lalu panggil fungsi di daftar lainnya (*cdr lst*).

#### Contoh Penggunaan

```scheme
(print-elements (list 1 2 3))
```

Keluaran:

- *"1"*
- *"2"*
- *"3"*

Hasilnya: *"selesai"*

---

#### Cara Kerjanya

1. Fungsi ini mengambil elemen pertama dari daftar menggunakan *car* dan memprosesnya.
2. Ia kemudian memanggil dirinya sendiri dengan sisa daftar (*cdr*).
3. Proses ini berulang hingga daftar kosong (*null?lst*).

---

### Ringkasan

- Rekursi sederhana terdiri dari:
  1. **Kasus dasar**: Menghentikan rekursi.
  2. **Kasus rekursif**: Mengurangi masalah ke kasus dasar.
- Setiap panggilan rekursif memajukan komputasi menuju penyelesaian.
- Setelah kasus dasar tercapai, hasilnya digabungkan saat rekursi selesai.

Rekursi mencerminkan struktur masalah dan memberikan alur yang jelas dan logis. Selalu pastikan kasus dasar untuk menghindari rekursi tak terbatas.