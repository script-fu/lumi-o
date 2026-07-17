---
title: "Plug-in Filter"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: db9cfb794dad80ce918a3eca7b47d02b23dbbba960a26765c04d95d459d8ec6b
translation_lock: true
url: "hub/scripting/tutorials/Filter Plug-in/filter-plug-ins"
---
Kami menggunakan plug-in _procedure_ untuk tutorial [Langkah Pertama](../../first-step/). Jenis plug-in tersebut berfungsi tanpa memerlukan gambar atau sumber daya dapat digambar sebagai masukan. Biasanya, Anda menggunakan plug-in untuk mengubah gambar dan sumber daya dapat digambarnya. Plug-in seperti ini disebut _filter_ plug-in.

### Apa itu Sumber Daya Dapat Digambar?

**Dapat digambar** di Lumi mengacu pada elemen gambar yang dapat digambar, seperti lapisan atau saluran. Plug-in filter biasanya beroperasi pada elemen ini.

### Contoh Plug-in Filter Sederhana

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Menggunakan pernyataan let untuk mendefinisikan variabel pesan dan kode inti
  (let ((message "hello, world"))
    ;; Menampilkan pesan di konsol error Lumi
    (lumi-message message)
    ;; Membalik warna drawable pertama yang dipilih
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Mendaftarkan plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Nama prosedur utama
  "Simple Filter Plug-in Demo"             ;; Nama seperti yang muncul di menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Deskripsi tooltip
  "Author Name"                            ;; Berikan kredit untuk diri sendiri
  "License"                                ;; Lisensi
  "Date written"                           ;; Tanggal ditulis
  "*"                                      ;; Menunjukkan plug-in ini memerlukan gambar
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Memerlukan satu atau lebih drawable yang dipilih

;; Menentukan lokasi menu untuk plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Salin teks dan simpan sebagai `simple-filter-plug-in.scm` dalam folder bernama `simple-filter-plug-in` di dalam salah satu folder plug-in Lumi. Folder plug-in Lumi adalah folder _any_ yang tercantum di bawah:
 **Lumi > Sunting > Preferensi > Folder > Plug-in**

Di Linux, klik kanan file `simple-filter-plug-in.scm`, buka **Properti > Izin**, dan centang **Izinkan mengeksekusi file sebagai program**. Setelah file berada di tempat yang benar, dapat dieksekusi dan bebas dari kesalahan sintaksis, ketika Lumi dimulai ulang, file tersebut akan muncul di bilah header menu atas, di dalam menu bernama **Plug-in**.

### Menjalankan Plug-in

1. Buka gambar (plug-in filter ini memerlukan gambar agar berfungsi).
2. Buka **Tools > Debug > konsol pesan** untuk melihat pesan.
3. Pilih **Demo Plug-in Filter Sederhana** dari menu **Plug-in**.
4. Salah satu lapisan yang dipilih akan memiliki warna yang terbalik dan sebuah pesan akan dicetak ke konsol kesalahan.

### Mengedit Ekstensi

Anda dapat menyesuaikan plug-in dengan mengedit file `.scm`-nya. Misalnya, untuk mengubah pesan yang ditampilkan:

1. Buka file dan cari baris yang mendefinisikan `message`.
2. Ganti `"hello, world"` dengan teks khusus Anda.
3. Simpan filenya.

Di Lumi versi 3, plug-in tidak perlu disegarkan agar perubahan yang disimpan dapat diterapkan. Cukup jalankan kembali plug-in untuk melihat pesan yang diperbarui.

### Pemeriksaan Plug-in

#### Jalur Shebang

Baris pertama memastikan skrip berfungsi sebagai plug-in di Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Definisi Prosedur

Prosedur ini menerima dua argumen: gambar aktif dan sumber daya dapat digambar yang dipilih.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Logika Inti

Pernyataan `let` mendefinisikan variabel dan melakukan operasi pada sumber daya dapat digambar.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Menampilkan pesan di konsol error Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Membalik warna drawable pertama yang dipilih
```

### Registrasi Ekstensi

Plug-in ini terdaftar pada Lumi sebagai plug-in filter:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Mendaftarkan prosedur utama
  "Simple Filter Plug-in Demo"             ;; Nama seperti yang muncul di menu Lumi
  "Tests a basic Scheme filter plug-in"    ;; Deskripsi tooltip
  "Author Name"                            ;; Nama penulis
  "License"                                ;; Jenis lisensi
  "Date written"                           ;; Tanggal ditulis
  "*"                                      ;; Menunjukkan plug-in memerlukan gambar
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Memerlukan satu atau lebih drawable yang dipilih
```

#### Menu Registrasi

Baris ini menentukan lokasi menu untuk plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Pemecahan masalah

Jika plug-in tidak muncul, periksa lokasi, nama, dan properti yang dapat dieksekusi.

Lokasinya harus berada di jalur pencarian plug-in.
Nama file harus sesuai dengan nama folder yang memuatnya.
File harus ditetapkan sebagai file yang dapat dieksekusi.


**Konsol pesan** adalah alat yang berharga untuk memecahkan masalah plug-in khusus. Jika plug-in Anda tidak berfungsi seperti yang diharapkan, periksa di sini untuk pesan kesalahan atau log. Jendela **Terminal** juga dapat memberikan informasi debug dan melaporkan masalah pemuatan.