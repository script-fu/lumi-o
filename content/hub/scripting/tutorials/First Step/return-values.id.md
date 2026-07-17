---
title: "Nilai Pengembalian"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
translation_lock: true
url: "hub/scripting/tutorials/First Step/return-values"
---
Nilai kembalian penting karena memungkinkan Anda mengontrol aliran tanpa status tambahan. Dalam Scheme, ekspresi yang terakhir dievaluasi menjadi nilai kembalian.

Halaman ini menggunakan bantuan validasi dari contoh perpesanan untuk menunjukkan bagaimana nilai pengembalian eksplisit membuat kode lebih mudah untuk dibuat.

### Apa yang dimaksud dengan Nilai Pengembalian?

Dalam Scheme, nilai kembalian suatu fungsi ditentukan oleh ekspresi terakhir yang dievaluasi oleh fungsi tersebut. Ini berarti apa pun yang dievaluasi oleh baris kode terakhir dalam fungsi akan dikembalikan sebagai hasil dari fungsi tersebut. Jika tidak ada nilai yang dikembalikan secara eksplisit, fungsi akan mengembalikan `#f` (salah) atau `undefined`.

Mari Anda lihat kembali fungsi validasi, (apakah-valid-string?)

```scheme
;; Tujuan: Memvalidasi bahwa pesan bukan string kosong
(define (is-valid-string? message)
  ;; Memeriksa apakah pesan adalah string tidak kosong
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

Dalam fungsi ini, jika pesan tidak valid, kesalahan akan terjadi. Namun, jika pesannya valid, tidak ada nilai pengembalian eksplisit yang diberikan, dan fungsi mengembalikan `#f` secara default.

### Menjadikan Nilai Pengembalian Eksplisit

Anda dapat memperbaikinya dengan membuat nilai kembalian lebih eksplisit. Misalnya, Anda dapat mengembalikan `#t` (true) jika pesannya valid:

```scheme
;; Tujuan: Memvalidasi bahwa pesan dikirim ke tujuan output yang valid
(define (is-valid-output-display? output)
  ;; Memeriksa apakah output termasuk tujuan tampilan yang diharapkan
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

Dalam versi ini, fungsi tersebut akan mengembalikan `#t` ketika pesannya valid, sehingga memberikan hasil yang jelas. Hal ini memungkinkan fungsi untuk digunakan lebih fleksibel dalam konteks lain yang memerlukan hasil boolean.

### Menggunakan Nilai Pengembalian Secara Efektif

Dengan memutuskan apa yang dikembalikan oleh fungsi Anda, Anda dapat membuatnya lebih mudah diprediksi dan berguna. Mengembalikan nilai seperti `#t`, `#f`, atau hasil spesifik memberi Anda kontrol lebih besar atas cara fungsi berinteraksi dengan kode lainnya. Misalnya, Anda dapat menggunakan nilai kembalian untuk membuat keputusan lebih lanjut dalam fungsi pemanggil atau meneruskannya sebagai argumen ke fungsi lain.

Berikut adalah contoh sederhana penggunaan nilai kembalian untuk mengontrol aliran logika:

```scheme
;; Tujuan: Mengirim pesan ke tujuan output yang sesuai
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

Dalam hal ini, (send-message) bergantung pada nilai kembalian (is-valid-output-display?) untuk memutuskan apakah akan melanjutkan.
Pernyataan kondisional `cond` akan dilewati jika pengujian pertama gagal. Juga, perhatikan cara membacanya dengan cara yang cukup alami, apakah tampilan keluaran valid?

## Jika Pernyataan Logika dalam Scheme

Sebelum contoh perpustakaan yang difaktorkan ulang, berikut adalah tinjauan singkat tentang logika kondisional. Scheme menggunakan `if` untuk memilih di antara dua jalur.

Berikut bentuk sederhana pernyataan `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Struktur ini memeriksa kondisi, dan jika kondisinya benar, maka ia akan mengeksekusi tindakan pertama. Jika kondisinya salah, tindakan kedua akan dijalankan.

Jika Anda perlu melakukan beberapa tindakan ketika kondisinya benar atau salah, Anda dapat menggunakan `begin` untuk mengelompokkannya menjadi satu:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Hal ini memungkinkan Anda menangani situasi yang lebih kompleks, di mana beberapa ekspresi atau pernyataan perlu dijalankan bergantung pada hasil pengujian kondisional.

Oke, berikut adalah kode pustaka dengan nilai kembalian yang disematkan dan digunakan untuk mengontrol proses eksekusi.

### Difaktorkan Ulang dengan Nilai Pengembalian

```scheme
;; Tujuan: Mengirim pesan ke bilah status, mengembalikan #t jika berhasil
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Tujuan: Mengirim pesan ke kotak dialog, mengembalikan #t jika berhasil
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Tujuan: Mengirim pesan ke konsol error, mengembalikan #t jika berhasil
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Tujuan: Mengirim pesan ke terminal, mengembalikan #t jika berhasil
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Tujuan: Mengirim pesan ke output yang sesuai, mengembalikan #t jika berhasil
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Tujuan: Memvalidasi bahwa pesan bukan string kosong, mengembalikan #t jika valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Tujuan: Memvalidasi bahwa output adalah tujuan yang valid, mengembalikan #t jika valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Kesimpulan

Nilai yang dikembalikan adalah bagian mendasar untuk membuat fungsi fleksibel dan dapat digunakan kembali. Dengan hati-hati memutuskan apa yang harus dikembalikan oleh setiap fungsi, Anda dapat memastikan fungsi-fungsi Anda berinteraksi dengan baik satu sama lain dan memberikan informasi berguna ke seluruh kode. Baik itu pengembalian `#t` atau `#f`, atau sesuatu yang lebih spesifik, nilai pengembalian memberi Anda cara untuk mengontrol alur program dan menangani berbagai hasil.