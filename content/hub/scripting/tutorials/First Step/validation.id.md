---
title: "Validasi"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
translation_lock: true
url: "hub/scripting/tutorials/First Step/validation"
---
Saat membuat plug-in yang kuat, penting untuk memastikan bahwa fungsi Anda menangani kesalahan dengan baik dan berfungsi sesuai harapan, bahkan jika terjadi penyalahgunaan atau masukan yang tidak terduga. Validasi membantu melindungi integritas fungsi dan mencegah error atau perilaku yang tidak diinginkan.

Mari Anda lihat bagaimana Anda dapat meningkatkan fungsi `send-message` dengan menambahkan pemeriksaan validasi untuk memastikan fungsi tersebut menangani input dengan benar.

### Validasi Masukan

Sebelum mengirim pesan, Anda harus memastikan argumen `output` yang diteruskan ke fungsi `send-message` valid. Anda dapat menambahkan tanda centang untuk mengonfirmasi bahwa tujuan keluaran adalah salah satu nilai yang diharapkan (gui, konsol kesalahan, atau terminal).

Contoh:

```scheme
(define (send-message message output)
  ;; Memvalidasi argumen output
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))
```

Dalam contoh ini, Anda menggunakan `member` untuk memeriksa apakah argumen `output` valid. Jika tidak, fungsi tersebut akan memunculkan kesalahan dengan pesan yang jelas, mencegah nilai yang tidak valid menyebabkan masalah.

### Menangani Pesan Kosong

Hal ini juga berguna untuk memastikan bahwa argumen `message` valid. Misalnya, jika string kosong atau #f (false) diteruskan sebagai pesan, fungsi harus menanganinya dengan baik.

Contoh penanganan pesan kosong:

```scheme
(define (send-message message output)
  ;; Memeriksa apakah pesan kosong
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Pendekatan ini memastikan bahwa fungsi selalu menerima masukan yang valid, meningkatkan keandalannya, dan mencegah perilaku yang tidak diharapkan.

### Contoh Validasi Gabungan

```scheme
;; Fungsi untuk menangani output pesan ke berbagai tujuan
(define (send-message message output)

  ;; Memvalidasi argumen pesan dan output
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Mengembalikan handler pesan default ke konsol Message
  (lumi-message-set-handler 2))
```

Dalam versi ini:
- Fungsi memeriksa apakah `message` kosong atau tidak valid terlebih dahulu. Jika pesannya valid, pesan akan dilanjutkan dengan memeriksa apakah `output` adalah salah satu nilai yang diterima (`gui`, `error-console`, atau `terminal`).
- Jika kedua pemeriksaan lolos, pesan dikirim ke output yang sesuai. Jika tidak, pesan kesalahan akan muncul dengan penjelasan yang jelas.
- Pemeriksaan tambahan dilakukan untuk memastikan pesan tersebut juga berupa string.

Fungsi validasi gabungan ini menjaga kode tetap bersih dan memastikan bahwa kedua input divalidasi sebelum tindakan apa pun diambil, sehingga menjadikan fungsi lebih tangguh. Perhatikan, Anda juga sedang membangun sistem pesan debug. Ketika
kode gagal, Anda mendapat alasan, alasan Anda menulis sendiri.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```