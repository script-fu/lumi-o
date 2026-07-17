---
title: "Pemfaktoran ulang"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_source_sha256: 730a20920b8e93d463bfb01f5d729e5ea84a548cc4b846e6e888ee751d095cf1
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactoring"
---
Setelah suatu fungsi berfungsi, Anda dapat mengambil langkah mundur dan memikirkan cara terbaik untuk menyusun kode Anda. Tujuannya adalah membuat plug-in Anda sejelas, mudah dipahami, dan dapat dipelihara. Proses memperbaiki dan menyempurnakan struktur kode yang ada tanpa mengubah perilakunya dikenal sebagai pemfaktoran ulang.

Inilah fungsi awalnya lagi:

```scheme
(define (scheme-hello-world)
  ;; Mengatur handler pesan untuk menampilkan pesan di kotak dialog GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Mengatur handler pesan untuk menampilkan pesan di Konsol Error
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Mengirim pesan ke terminal, jendela OS yang meluncurkan Lumi
  (display "Hello world!\n"))
```

Nama fungsi adalah nama fungsi, dan parameter adalah apa yang diterima fungsi sebagai masukan. Badan adalah blok kode yang dijalankan saat fungsi dipanggil.

Bentuk abstrak:

```scheme
(define (function-name parameter)
  body)
```

### Pengulangan Kode

Hapus pengulangan sejak dini. `(lumi-message "Hello world!\n")` diulang dua kali, dan string pesan diulang tiga kali. Variabel memecahkan string yang berulang.

### Variabel

Dalam Scheme, variabel memiliki "ruang lingkup", yang diketahui, dan cakupan tersebut diatur menggunakan pernyataan `let`. Variabel terikat pada nilai di bagian pengikatan, dan variabel memiliki cakupan di badan let. Variabel hanya diketahui di dalam blok let dan tidak dapat diakses di luar blok tersebut.

```scheme
(let ((variable value))
  body)
```

Memperkenalkan variabel yang disebut "pesan":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Mengatur handler pesan untuk menampilkan pesan di kotak dialog GUI
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Mengatur handler pesan untuk menampilkan pesan di Konsol Error
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Mengirim pesan ke terminal, jendela OS yang meluncurkan Lumi
    (display message)))
```

Dalam contoh Anda, Anda telah menggunakan variabel bernama "pesan" yang terikat pada string "Hello World\n". Hal ini memungkinkan Anda mengubah isi pesan satu kali, bukan tiga kali, sehingga mengurangi kemungkinan kesalahan dan membuat kode lebih fleksibel.

### Fungsi Ekstraksi

Dalam pemrograman fungsional, memfaktorkan ulang kode untuk mengekstrak logika yang dapat digunakan kembali menjadi fungsi terpisah adalah praktik umum. Dengan melakukan ini, **fungsi utama** menjadi lebih sederhana dan lebih fokus pada tujuan tingkat tingginya, sedangkan **fungsi yang diekstraksi** tampak lebih kompleks karena menangani logika mendetail. Hal ini disengaja dan sejalan dengan prinsip inti pemrograman fungsional, seperti modularitas, pemisahan perhatian, dan keterbacaan. Ini yang difaktorkan ulang
Halo Dunia! setelah ekstraksi.

Mengekstraksi logika:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; Fungsi utama
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Fungsi untuk menangani output pesan ke berbagai tujuan
(define (send-message message output)
  (cond
    ;; Mengirim ke Konsol Error
    ((eq? output 'error-console)
       ;; Mengatur handler ke Konsol Error
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Mengirim ke kotak dialog GUI
    ((eq? output 'gui)
       ;; Mengatur handler ke dialog GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Mengirim ke jendela terminal
    ((eq? output 'terminal)
       ;; Output terminal ditangani dengan display
       (display message)))

  ;; Mengembalikan handler pesan default ke Konsol Error
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Simbol

Pada contoh di atas, tipe data yang disebut simbol digunakan, seperti 'gui. Simbol diteruskan sebagai parameter ke fungsi kirim pesan dan dapat digunakan untuk membuat keputusan bersyarat sederhana. Seperti kunci simbolik, mereka adalah pengidentifikasi unik. Untuk informasi lebih lanjut tentang simbol, kunjungi [halaman ini.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Menyederhanakan Fungsi Utama

Dalam fungsi asli (scheme-hello-world), semua logika untuk mengirim pesan ke output yang berbeda (GUI, konsol kesalahan, Terminal) dicampur ke dalam fungsi utama. Setelah pemfaktoran ulang, fungsi utama hanya berfokus pada **apa yang perlu dilakukan**, mengirimkan pesan ke tujuan yang berbeda.

Fungsi main yang difaktorkan ulang lebih sederhana:

- Ini dengan jelas menyatakan tujuannya: mengirim pesan yang sama ke beberapa output.
- Ini menghindari mengacaukan logika utama dengan kode berulang seperti mengatur penangan pesan untuk keluaran yang berbeda.
- Lebih mudah dibaca dan dipahami secara sekilas.

### Kompleksitas Fungsi yang Diekstraksi

Sebaliknya, fungsi **(kirim pesan)** adalah tempat logika detailnya berada. Sekarang menangani variasi perilaku untuk setiap output (GUI, konsol kesalahan, Terminal). Fungsinya sedikit lebih kompleks dibandingkan sebelumnya, namun kini **terpusat** dan **terisolasi**.

## Mengaitkannya dengan Pemrograman Fungsional

Dalam pemrograman fungsional, fungsi dipandang sebagai **warga kelas satu**, artinya fungsi tersebut dapat digunakan kembali, diedarkan, dan digabungkan untuk membentuk perilaku yang lebih kompleks. Tujuannya adalah untuk:- **Pecah masalah** menjadi bagian-bagian yang lebih kecil dan independen.
- **Pisahkan kompleksitas** ke dalam fungsi yang lebih kecil yang menangani tugas tertentu, seperti `send-message`.
- **Buat fungsi tingkat yang lebih tinggi tetap sederhana** sehingga dapat fokus mengatur aliran data dan tindakan, tanpa perlu mengetahui detail cara menyelesaikan setiap tugas.
- **Pemisahan masalah**: Fungsi ini menangani cara pesan dikirim berdasarkan jenis keluaran, yang mengisolasi logika ini dari fungsi utama.
- **Modularitas**: Dengan menangani semua logika pengiriman pesan di satu tempat, Anda dapat dengan mudah melakukan perubahan (seperti menambahkan opsi keluaran baru) tanpa mengubah fungsi utama.
- **Dapat digunakan kembali**: Fungsi `send-message` dapat digunakan kembali, artinya jika Anda perlu mengirim pesan ke beberapa output di tempat lain dalam kode Anda, Anda cukup memanggil fungsi ini daripada menulis ulang logika serupa.

Dengan melakukan pemfaktoran ulang, fungsi utama dalam contoh ini menjadi pernyataan **deklaratif** tentang apa yang terjadi ("mengirim pesan ke tiga tempat"), sedangkan kompleksitas cara mengirim pesan tersebut diabstraksikan ke dalam fungsi `send-message`.