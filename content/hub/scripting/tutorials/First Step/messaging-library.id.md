---
title: "Perpustakaan Pesan"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: 0833643efbceb6ebd9977656657b3ba57f290758c0d400aaf7d02ab054869278
translation_lock: true
url: "hub/scripting/tutorials/First Step/messaging-library"
---
Seiring waktu, apa yang awalnya hanya fungsi tunggal untuk mengirim pesan telah berkembang menjadi kumpulan fungsi terkait. Fungsi-fungsi ini sekarang menjadi dasar **Perpustakaan Perpesanan**, yang dirancang untuk menangani keluaran ke berbagai tujuan, seperti GUI, konsol Pesan, dan terminal OS.

### Mengapa Perpustakaan Pesan?

Seiring dengan meningkatnya kebutuhan Anda, penanganan pesan di berbagai keluaran memerlukan pendekatan yang lebih modular dan dapat diperluas. Daripada hanya menggunakan satu fungsi saja, Anda memecah proses menjadi beberapa komponen yang dapat digunakan kembali, sehingga memberikan fleksibilitas yang lebih besar. Pustaka ini sekarang dapat digunakan sebagai alat perpesanan umum yang dapat dipinjam oleh plug-in atau fungsi lain.

### Apa Fungsi Perpustakaan Pesan?

Perpustakaan Pesan saat ini mencakup fungsi-fungsi berikut:

- **send-to-gui**: Mengirim pesan ke kotak dialog Lumi GUI.
- **send-to-error-console**: Mengirim pesan ke konsol Lumi Message.
- **kirim-ke-terminal**: Mengirim pesan ke jendela terminal.
- **kirim-pesan**: Fungsi operator yang mengarahkan pesan ke output yang sesuai.
- **validasi-pesan**: Memastikan pesan dan keluaran valid sebelum dikirim.

### Memperluas Perpustakaan

**Perpustakaan Perpesanan** dapat dengan mudah diperluas untuk mendukung keluaran tambahan. Misalnya:

- **kirim ke file**: Menyimpan pesan ke file log.
- **send-to-logger**: Integrasikan dengan sistem logging eksternal.
- **kirim-ke-pemberitahuan**: Menampilkan pesan sebagai notifikasi sistem.

Dengan mengikuti pola desain modular dan fungsi yang dapat digunakan kembali, perpustakaan ini dapat berkembang menjadi alat komprehensif untuk menangani semua jenis tugas perpesanan.

## Manfaat Perpustakaan Pesan

- **Dapat digunakan kembali**: Fungsi dapat digunakan kembali di berbagai plug-in atau proyek.
- **Modularitas**: Setiap fungsi menangani satu tugas tertentu, membuat kode lebih mudah dipelihara dan diperluas.
- **Konsistensi**: Menggunakan fungsi validasi dan penanganan pesan yang sama memastikan perilaku yang konsisten di seluruh aplikasi.

**Perpustakaan Perpesanan** adalah awal dari kerangka kerja yang lebih luas yang dapat menyederhanakan cara pengelolaan pesan di proyek Anda. Seiring berkembangnya perpustakaan, plug-in baru dapat dengan mudah memanfaatkannya untuk mengirim pesan ke mana pun mereka pergi.

Anda dapat menyesuaikan struktur file:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Dan ingatlah untuk menyesuaikan `load` di plug-in utama:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plug-ins/funky-library/messaging.scm")

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