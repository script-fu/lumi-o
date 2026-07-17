---
title: "Peramban Prosedur"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: f2ea095c0407f9641d28803e937a992e044584f6bcbed960239d0c0df4b430d2
url: "hub/scripting/tutorials/first-step/the-procedure-browser"
translation_lock: true
---
**Lumi Procedure Browser** memungkinkan Anda mencari prosedur yang tersedia (yang tersedia bawaan dan plug-in) dan memeriksa parameter serta nilai yang dikembalikan.

### Di mana Menemukan Procedure Browser Lumi

Anda dapat mengakses Prosedur Browser di Lumi melalui menu **Bantuan**:

- **Bantuan** -> **Procedure Browser**

### Apa yang Dilakukan Procedure Browser

Procedure Browser mencantumkan semua prosedur internal Lumi, beserta prosedur yang ditambahkan oleh plug-in, termasuk prosedur yang baru saja Anda instal. Setiap entri prosedur memberikan informasi berguna, termasuk:

- Nama prosedur.
- Deskripsi tentang fungsinya.
- Parameter yang diterimanya (nilai input).
- Nilai kembalian (output).

Cari berdasarkan kata kunci atau nama prosedur ketika Anda perlu memverifikasi tanda tangan panggilan atau mengonfirmasi nama prosedur yang sebenarnya.

#### (lumi-message) di Procedure Browser

Telusuri `lumi-message` untuk melihat parameternya dan mengembalikan nilai.

### Menemukan Plug-in Anda

Setelah Anda menginstal aplikasi "Hello World!" plug-in, Anda dapat menemukannya terdaftar di Prosedur Browser. Cukup cari nama fungsi yang Anda daftarkan pada Lumi, dalam hal ini, "scheme-hello-world". Entri tersebut akan menampilkan parameter dan nilai kembalian apa pun yang terkait dengan plug-in, bersama dengan deskripsi singkat. Anda juga akan melihat di mana beberapa baris teks yang Anda masukkan sebagai parameter masukan selama proses pendaftaran ditampilkan di bagian **Informasi Tambahan**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Nama prosedur
  "Hello world!"                                        ;; Nama item menu
  "A Scheme procedure plug-in"                       ;; Tooltip dan deskripsi
  "Your Name"                                           ;; Penulis
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; Lisensi
  "2024")                                               ;; Tanggal hak cipta
```

Hal ini memudahkan untuk memverifikasi bahwa plug-in Anda telah terdaftar dengan benar dan memberi Anda cara cepat untuk meninjau bagaimana plug-in tersebut berinteraksi dengan prosedur lain di Lumi. Procedure Browser adalah alat yang ampuh untuk melakukan debug dan memperluas plug-in Anda dengan menjelajahi semua prosedur yang tersedia dalam Lumi.