---
title: "Memuat"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
Segera setelah fungsi pembantu berkembang, pindahkan ke file perpustakaan kecil. Hal ini membuat plug-in tetap fokus dan membuat helper dapat digunakan kembali di beberapa plug-in.

### Membuat Fungsi Perpustakaan

Anda dapat mengambil fungsi kirim pesan dan membuat file baru dengan itu sebagai isinya. Simpan file ke folder repo Anda, bukan bagian plug-in, mungkin di dekat tingkat atas;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Ini adalah direktori utama untuk menyimpan kode Scheme Anda.
  - **perpustakaan/**: Di sinilah fungsi bersama seperti `send-message.scm` aktif.
  - **plug-ins/**: Di sinilah masing-masing plug-in Anda disimpan.
    - **hello-world/**: Folder untuk plug-in "Hello World!" tertentu.
      - **hello-world.scm**: Berkas skrip plug-in.

Contoh fungsi perpustakaan send-message.scm

```scheme
;; Fungsi untuk menangani output pesan ke berbagai tujuan
(define (send-message message output)
  (cond
    ;; Mengirim ke konsol Message
    ((eq? output 'error-console)
       ;; Mengatur handler ke konsol Message
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

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))
```

### Muat Fungsi Perpustakaan

Anda dapat memuat fungsi perpustakaan tersebut dengan perintah Scheme `load`;

Memuat file perpustakaan:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plug-ins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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

Hei! Kami sekarang memiliki sesuatu yang lebih sederhana dan lebih pendek untuk dibaca, yang menjelaskan dirinya sendiri tanpa komentar. Ini adalah kesimpulan yang memuaskan dari refactoring.