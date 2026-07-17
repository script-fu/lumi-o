---
title: "Pikiran Terakhir"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
translation_lock: true
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
Anda sekarang memiliki plug-in prosedur kerja dan perpustakaan pembantu kecil. Seri ini memperkenalkan pola inti yang akan Anda gunakan di sebagian besar skrip Lumi:

- Fungsi: Blok penyusun plug-in Anda.
- Refactoring: Memperbaiki struktur kode sambil mempertahankan fungsionalitas.
- Pustaka Kode: Memusatkan fungsi yang dapat digunakan kembali untuk menjaga kode Anda tetap bersih dan modular.
- Teknik Validasi: Memastikan bahwa masukan valid sebelum menjalankan logika inti Anda.

Anda juga melihat dasar-dasar penggunaan Git untuk melacak perubahan dan menjaga struktur proyek tetap bersih. Alur kerja tersebut membuatnya lebih mudah untuk melakukan iterasi tanpa kehilangan versi yang berfungsi.

Inilah versi terakhir dari kode plug-in utama Anda:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Kode perpustakaan:

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

Dengan memfaktorkan ulang bantuan perpesanan ke dalam pustaka kecil, plug-in tetap fokus pada maksud dan pustaka berisi detail penerapan. Validasi dan perutean pesan yang konsisten membuat kegagalan dapat diprediksi.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Langkah selanjutnya:

- Pindahkan pembantu yang dapat digunakan kembali ke file perpustakaan khusus.
- Jaga agar plug-in tetap kecil dan beri nama prosedur untuk apa yang mereka lakukan.
- Tambahkan validasi pada batas (input, jalur file, opsi menu).

Simpan hasil akhir sebagai dua file di repo plug-in Anda:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`