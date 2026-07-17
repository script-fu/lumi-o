---
title: "vektor"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 23911f048f43dea4e07f47834a477d10f6eaebd9c9bd1b975db79ed1442deaaf
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/vectors"
---
Dalam Scheme, vektor adalah struktur data mendasar lainnya yang digunakan untuk mengelompokkan nilai. Tidak seperti daftar, vektor berukuran tetap, kumpulan elemen yang diindeks, menyediakan akses acak dan pembaruan yang lebih cepat. Setiap elemen dalam suatu vektor dapat bertipe apa saja, termasuk vektor lainnya. Vektor direpresentasikan menggunakan # diikuti dengan tanda kurung. `#(1 2 3)`

Meskipun vektor dan daftar mungkin tampak serupa, keduanya memiliki tujuan berbeda dalam pemrograman Scheme:

- Daftar lebih umum digunakan untuk operasi rekursif dan struktur dinamis, karena penerapan node tertautnya memungkinkan manipulasi awal dan traversal secara efisien melalui dekomposisi rekursif.

- Vektor, di sisi lain, dioptimalkan untuk skenario yang memerlukan akses acak ke elemen atau pembaruan pada indeks tertentu, sehingga lebih cocok untuk kasus penggunaan seperti tabel pencarian, konfigurasi ukuran tetap, atau operasi indeks yang kritis terhadap kinerja.

Intinya, daftar adalah pilihan alami untuk algoritma rekursif dan data berukuran dinamis, sementara vektor sangat cocok ketika pola akses berukuran tetap atau terindeks adalah yang terpenting.

### Vektor Sederhana

```scheme
(vector 1 2 3)
```

- Membuat vektor tiga elemen: `1`, `2`, dan `3`.

Hasil: **`#(1 2 3)`**

#### Mengakses Elemen Vektor

Elemen dalam vektor diakses menggunakan prosedur `vector-ref`, yang mengambil elemen pada indeks tertentu (mulai dari `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Mengambil elemen pada indeks 0
(vector-ref my-vector 1)  ; Mengambil elemen pada indeks 1
```

#### Iterasi: Memproses Setiap Elemen dalam Vektor

Anda dapat melakukan iterasi melalui vektor menggunakan loop atau rekursi. Scheme menyediakan `vector-length` untuk menentukan ukuran vektor. Berikut ini loop sederhana untuk mencetak setiap elemen dalam vektor:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Mencetak elemen
        (loop (+ i 1)))                                    ; Memproses indeks berikutnya
      (lumi-message "done"))))                             ; Akhiri loop
```

- **Kasus Dasar:** Jika indeks `i` mencapai panjang vektor, hentikan perulangan.
- **Kasus Rekursif:** Cetak elemen pada indeks `i`, lalu tambahkan `i`.

#### Contoh Penggunaan

```scheme
(print-elements (vector 1 2 3))
```

Hasil:

- `"1"`
- `"2"`
- `"3"`

Hasil: "selesai"

### Vektor Campuran

Vektor dapat mencakup elemen dengan tipe berbeda, termasuk string, boolean, angka, vektor lain, atau bahkan hasil ekspresi:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Ini menciptakan vektor dengan:
  - Nomor (`42`)
  - Sebuah string (`"hello"`)
  - Boolean (`#t`)
  - Vektor lain (`#(1 2)`)
  - Hasil ekspresi (`(+ 3 4)`, yang bernilai `7`)

Hasil: **`#(42 "hello" #t #(1 2) 7)`**

### Membangun Vektor

Vektor dibuat menggunakan `vector`, atau dengan menggunakan `make-vector` untuk membuat vektor berukuran tetap dengan nilai awal.

```scheme
(make-vector 5 0)
```

Membuat vektor berukuran `5` dengan semua elemen diinisialisasi ke `0`.

Hasil: `#(0 0 0 0 0)`

### Memperbarui Vektor

Prosedur `vector-set!` memperbarui elemen dalam vektor pada indeks tertentu.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Mengatur elemen kedua ke 42
my-vector
```

Hasil: `#(1 42 3)`

### Memeriksa Vektor

Prosedur `vector?` memeriksa apakah nilai yang diberikan adalah vektor.

```scheme
(vector? (vector 1 2 3))  ; Memeriksa apakah #(1 2 3) adalah vektor
(vector? 42)              ; Memeriksa apakah 42 adalah vektor
```

Hasil:

- `(vector? (vector 1 2 3))` mengembalikan `#t` (benar)
- `(vector? 42)` mengembalikan `#f` (salah)

### Vektor dan Perilaku Referensi Lewat

Dalam Scheme, vektor dapat berubah dan diteruskan dengan referensi. Ini berarti ketika Anda meneruskan vektor ke suatu fungsi, fungsi tersebut dapat mengubah vektor aslinya secara langsung. Setiap perubahan yang dilakukan pada vektor di dalam fungsi juga akan tercermin di luar fungsi. Perilaku ini berguna untuk berbagi dan memperbarui data secara efisien di berbagai fungsi, namun juga memerlukan kehati-hatian untuk menghindari efek samping yang tidak diinginkan.

#### Contoh: Memodifikasi Vektor dalam suatu Fungsi

Berikut ini contoh yang menunjukkan bagaimana vektor diteruskan dengan referensi dan dimodifikasi:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Memperbarui vektor pada indeks yang ditentukan

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Mengubah elemen kedua menjadi 99
my-vector                              ; Vektor asli sekarang diperbarui
```

Hasil: `#(10 99 30)`

#### Penjelasan Langkah demi Langkah

1. **Buat Vektor:** `my-vector` diinisialisasi dengan nilai `10`, `20`, dan `30`.
2. **Meneruskan ke Fungsi:** `my-vector` diteruskan ke `modify-vector` bersama dengan indeks dan nilai baru yang akan diperbarui.
3. **Modifikasi dalam Fungsi:** Prosedur `vector-set!` memperbarui nilai pada indeks yang ditentukan langsung dalam vektor asli.
4. **Mencerminkan Perubahan:** Karena vektor dilewatkan melalui referensi, perubahan yang dilakukan dalam fungsi akan tercermin dalam vektor aslinya.

#### Implikasi dari Pass-by-Reference

- **Kinerja:** Melewati vektor dengan referensi adalah hal yang efisien karena menghindari penyalinan struktur besar.
- **Efek Samping:** Berhati-hatilah saat berbagi vektor di seluruh fungsi untuk menghindari modifikasi yang tidak diinginkan pada data bersama.

### Operasi pada Vektor

Scheme menyediakan beberapa prosedur bawaan untuk bekerja dengan vektor, termasuk:

- `vector-length`: Mengembalikan jumlah elemen dalam vektor.
- `vector->list`: Mengubah vektor menjadi daftar.
- `list->vector`: Mengubah daftar menjadi vektor.

```scheme
(vector-length (vector 1 2 3))         ; Mengembalikan 3
(vector->list (vector 1 2 3))          ; Mengonversi vektor ke daftar: (1 2 3)
(list->vector (list 1 2 3))            ; Mengonversi daftar ke vektor: #(1 2 3)
```

Hasil:

- `(vector-length (vector 1 2 3))` kembali `3`
- `(vector->list (vector 1 2 3))` kembali `(1 2 3)`
- `(list->vector (list 1 2 3))` kembali `#(1 2 3)`

### Vektor Bersarang

Vektor dalam Scheme dapat berisi vektor lain sebagai elemen, sehingga menciptakan struktur bersarang.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Membuat vektor dari tiga elemen, yang masing-masing elemen itu sendiri merupakan vektor.

Hasil: **`#(#(1 2) #(3 4) #(5))`**

#### Mengakses Data Bersarang

Untuk mengakses elemen dalam vektor bertumpuk, gunakan `vector-ref` beberapa kali untuk menavigasi struktur.

#### Contoh: Mengakses Elemen

```scheme
(vector-ref nested-vector 0)              ; Mengambil elemen pertama: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Mengambil elemen kedua vektor pertama: 2
```

### Ringkasan

- **Vektor** dalam Scheme adalah struktur data terindeks dan berukuran tetap.
- Gunakan `vector` untuk membuat vektor, `vector-ref` untuk mengakses elemen, dan `vector-set!` untuk memperbarui elemen.
- Prosedur bawaan seperti `vector-length`, `vector->list`, dan `list->vector` memungkinkan pengoperasian yang fleksibel.
- Vektor bersarang memungkinkan struktur data yang kompleks dan hierarkis.