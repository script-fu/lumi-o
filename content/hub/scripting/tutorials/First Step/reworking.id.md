---
title: "Pengerjaan ulang"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: af1b2f3233ef50936b24aa195d3a7da50529a4fff3109b087be2f861e15496d1
translation_lock: true
url: "hub/scripting/tutorials/First Step/reworking"
---
Langkah ini memperbaiki perilaku halus dalam contoh perpesanan.

Kami meneruskan string "Hello World\n" sebagai pesan. "\n" adalah jenis karakter khusus, karakter "pelarian". Ini memberitahu pencetakan keluaran untuk memulai baris baru. Dalam Scheme, ini juga akan memaksa pesan yang dikirim ke Status Bar muncul sebagai kotak GUI.

Pembantu `send-to-gui` mengirim pesan ke kotak dialog Lumi.

Perbarui konten dan tujuan pesan sehingga contoh berperilaku konsisten.

Menghapus karakter escape dan memperluas fungsinya:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Ganti angka ajaib dengan konstanta yang disediakan oleh Lumi (misalnya, `MESSAGE-BOX` dan `ERROR-CONSOLE`).

Kemudian bagi validasi menjadi dua fungsi sehingga dapat digunakan kembali dari beberapa situs panggilan.

- (apakah-string-valid?) Untuk memeriksa apakah string adalah string dan bukan string kosong, dalam fungsi kirim-ke*.
- (apakah-valid-output-display?) Untuk memeriksa validitas tujuan output tertentu, dalam fungsi kirim-pesan.

Mengolah ulang perpustakaan:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Menambahkan baris baru agar pesan muncul dalam kotak
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Tujuan: Mengirim pesan ke tujuan output yang sesuai
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Tujuan: Memvalidasi bahwa pesan bukan string kosong
(define (is-valid-string? message)
  ;; Memeriksa apakah pesan adalah string tidak kosong
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Tujuan: Memvalidasi bahwa pesan dikirim ke tujuan output yang valid
(define (is-valid-output-display? output)
  ;; Memeriksa apakah output termasuk tujuan tampilan yang diharapkan
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Kesimpulan

Dengan mengolah kembali perpustakaan perpesanan Anda, Anda menjadikannya lebih kuat dan dapat diandalkan. Kami memperbaiki masalah tersembunyi pada karakter baris baru, memperkenalkan konstanta untuk kejelasan yang lebih baik, dan memperluas fungsionalitas dengan menambahkan dukungan untuk bilah status dan keluaran kotak dialog. Selain itu, memisahkan logika validasi menjadi fungsi-fungsi yang lebih kecil dan terfokus memastikan bahwa kode Anda lebih mudah dipelihara dan diperluas di masa depan.

Pengerjaan ulang ini menunjukkan bagaimana perubahan kecil dapat meningkatkan keseluruhan struktur dan fungsionalitas perpustakaan Anda, membuka jalan bagi lebih banyak fleksibilitas dan penggunaan kembali seiring pertumbuhan proyek Anda.