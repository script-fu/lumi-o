---
title: "Refaktorisasi Lagi"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: 6fd2dd04a60013a83905022f3a5fd57ae427d5c84df7ac2223dac7fcb1b77587
translation_lock: true
url: "hub/scripting/tutorials/First Step/refactor-again"
---
Seiring berkembangnya perpustakaan pembantu, semakin sulit untuk diikuti secara sekilas. Refactor lagi untuk menjaga setiap fungsi tetap kecil dan memiliki tujuan tunggal.

### Menguraikan Kompleksitas

Untuk membuat fungsi lebih mudah diikuti dan dipelihara, bagi menjadi fungsi-fungsi yang lebih kecil dan terfokus. Mulailah dengan memisahkan validasi dari perutean pesan.

### Buat Fungsi Validasi

Anda dapat mengambil bagian dari fungsi yang memvalidasi argumen `message` dan `output` dan memindahkannya ke fungsi terpisah. Dengan cara ini, fungsi inti `send-message` tidak perlu mengkhawatirkan validasi, sehingga lebih mudah untuk diikuti.

```scheme
(define (validate-message message output)
  ;; Memeriksa apakah pesan adalah string tidak kosong
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Memeriksa apakah output termasuk tujuan yang diharapkan
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Menyederhanakan Pengiriman Pesan

Sekarang validasi telah dipindahkan ke fungsi terpisah, fungsi `send-message` dapat fokus pada pengiriman pesan saja. Ini akan jauh lebih sederhana, karena hanya menangani tugas khusus mengarahkan pesan ke tujuan yang benar.

```scheme
(define (send-message message output)
  ;; Memanggil fungsi validasi sebelum melanjutkan
  (validate-message message output)

  (cond
    ;; Mengirim ke konsol Message
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Mengirim ke kotak dialog GUI
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Mengirim ke jendela terminal
    ((eq? output 'terminal)
       (display message)))

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))
```

### Menguraikan Lebih Lanjut: Pisahkan Setiap Pengendali Output

Setiap jenis keluaran pesan (GUI, Konsol pesan, Terminal) dapat dipindahkan ke fungsinya masing-masing. Hal ini memungkinkan pengujian, modifikasi, dan potensi perluasan yang lebih mudah di masa mendatang.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Mengirim ke output yang sesuai
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))
```

### Menggunakan Kembali Validasi di Setiap Fungsi Pengiriman

Karena validasi adalah bagian penting untuk memastikan bahwa pesan dan keluaran sudah benar, masuk akal jika setiap fungsi `send-*` melakukan validasinya sendiri. Hal ini memastikan bahwa apa pun keluaran yang dipanggil, Anda selalu memeriksa masukannya terlebih dahulu.

```scheme
(define (send-to-gui message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'terminal)
  (display message))
```

Lihat bahwa Anda telah menghapus validasi dari fungsi kirim-pesan dan mengalihkan tanggung jawab ke masing-masing fungsi keluaran. Perubahan ini memastikan bahwa setiap tujuan (GUI, Konsol pesan, Terminal) menangani validasinya sendiri, menyederhanakan fungsi kirim pesan dan menjaga logika validasi lebih dekat ke tempat yang diperlukan.

Pendekatan ini dapat menyederhanakan fungsi kirim-pesan, menjadikannya _dispatcher_, sekaligus memastikan bahwa setiap fungsi kirim-ke-* memvalidasi pesan dengan benar sebelum diproses.

Dengan memindahkan validasi ke setiap fungsi kirim-ke-*, Anda menjadikannya dapat digunakan kembali sebagai fungsi mandiri. Ini berarti Anda dapat memanggil fungsi kirim-ke-gui, kirim-ke-kesalahan-konsol, atau kirim-ke-terminal secara langsung tanpa bergantung pada fungsi operator kirim-pesan. Masing-masing fungsi ini sekarang sepenuhnya menangani logikanya sendiri dan dapat digunakan secara independen di bagian lain kode atau di plug-in lain, menjadikan kode Anda lebih modular dan fleksibel.

## Manfaat Pemfaktoran Ulang

- **Hapus Pemisahan Kekhawatiran**: Setiap fungsi kini hanya menangani satu tanggung jawab, sehingga membuat kode lebih mudah dipahami.
- **Ekstensibilitas**: Menambahkan jenis keluaran baru sangatlah mudah. Anda cukup mendefinisikan fungsi baru seperti `send-to-file` atau `send-to-logger`, lalu menambahkan huruf besar/kecil dalam pernyataan `cond`.
- **Dapat digunakan kembali**: Masing-masing fungsi penanganan keluaran ini dapat digunakan kembali di tempat lain dalam proyek Anda atau dibagikan ke beberapa plug-in.
- **Konsistensi**: Dengan menggunakan kembali fungsi validasi di setiap fungsi `send-to-*`, Anda memastikan bahwa semua output divalidasi dengan benar, sehingga membuat kode lebih kuat.

Versi perpustakaan yang difaktorkan ulang:

```scheme
;; Tujuan: Mengirim pesan ke kotak dialog GUI
(define (send-to-gui message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Tujuan: Mengirim pesan ke konsol Message
(define (send-to-error-console message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Tujuan: Mengirim pesan ke jendela terminal
(define (send-to-terminal message)
  ;; Memvalidasi pesan sebelum melanjutkan
  (validate-message message 'terminal)
  (display message))

;; Tujuan: Mengirim pesan ke tujuan output yang sesuai
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))

;; Tujuan: Memvalidasi bahwa pesan bukan string kosong dan argumen output valid
(define (validate-message message output)
  ;; Memeriksa apakah pesan adalah string tidak kosong
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Memeriksa apakah output termasuk tujuan yang diharapkan
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

Apakah hanya itu yang bisa Anda lakukan? TIDAK! masih ada lagi yang harus dilakukan, silakan baca terus.