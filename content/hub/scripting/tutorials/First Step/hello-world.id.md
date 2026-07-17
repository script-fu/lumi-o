---
title: "Hello World!"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_source_sha256: c250d07dff926c7b51434efc644786f35b5189e03449dcdf4ec5916c1c151886
translation_lock: true
url: "hub/scripting/tutorials/First Step/hello-world"
---
Tutorial ini membahas struktur minimal plug-in Scheme. Beberapa baris bersifat “boilerplate”: baris tersebut diperlukan agar Lumi dapat memuat file, meskipun Anda belum sepenuhnya memahaminya.

```bash
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

Pada tingkat tinggi Anda akan:

1. Definisikan suatu fungsi
2. Daftarkan agar muncul di Procedure Database
3. (Opsional) Tambahkan entri menu
4. Instal file di folder plug-in

### Tentukan Fungsi

Fungsi, juga dikenal sebagai _procedure_, adalah sekumpulan kode dengan nama dan tujuan, mengambil masukan dan menghasilkan keluaran.

**Masukan** > **_Fungsi_** > **Keluaran**

### Daftarkan Fungsi

Mendaftarkan adalah tindakan memasukkan nama fungsi ke dalam daftar sehingga Lumi mengetahuinya.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Tautan ke Menu

Ini memberitahu Lumi di mana menemukan fungsi Anda dalam sistem menunya.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Ini menampilkan menu "Funky" di bilah menu utama. Ubah jalur untuk meletakkan plug-in di tempat lain. Jalur `<Image>/Funky` berarti plug-in akan muncul di kategori menu **Gambar**. Anda dapat mengubah `<Image>` menjadi `<Tools>`, `<Filters>`, dll., tergantung di mana Anda ingin plug-in tersebut muncul.

### Komentar

Dalam Scheme, komentar umumnya dilakukan dengan mengawali baris teks bermanfaat dengan `;;`. Penggunaan komentar Anda akan bergantung pada kelancaran Anda sebagai pembuat kode—jika Anda sesekali membuat kode, lebih banyak komentar akan membantu. Jika Anda membuat kode sepanjang waktu, kode tersebut akan mudah dibaca seperti halnya komentar. Selain itu, ketika memprogram secara fungsional, kodenya cenderung menjadi cukup deskriptif untuk dibaca seperti skrip.

### Sintaks

Kode cenderung memiliki sedikit aturan tentang bagaimana menempatkan item dalam sebuah baris, sehingga Anda dapat membaca baris tersebut dengan mudah. Misalnya, sebuah kalimat mungkin memiliki spasi setelah koma atau titik. Ini membantu keterbacaan.

Kode mungkin mengatur hal-hal dengan cara serupa, yang mungkin terlihat aneh pada awalnya:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Contoh Kode

Berikut contoh lengkapnya. Kebanyakan prosedur Lumi diawali dengan `lumi-`. Misalnya, `lumi-message` mencetak string ke pengendali pesan yang dikonfigurasi.

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Mengatur handler pesan untuk menampilkan pesan di kotak dialog GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Mengatur handler pesan untuk menampilkan pesan di Konsol Error
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Mengirim pesan ke terminal, jendela OS yang meluncurkan Lumi
  (display "Hello world!\n"))


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

### Instal Ekstensi

1. Buka **Lumi -> Sunting -> Preferensi -> Folder -> Plug-in**.
2. Tambahkan folder plug-in [repo](/hub/scripting/tools/git) Anda ke dalam daftar.
3. Buat folder untuk plug-in dan simpan contoh kode di atas sebagai `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Klik kanan pada file `hello-world.scm`.
5. Buka **Properti -> Izin -> Izinkan mengeksekusi file sebagai program**.
6. Mulai ulang Lumi.

### Coba Plug-innya

Plug-in sekarang akan muncul di bawah menu "Funky" di jendela utama Lumi. Klik, dan itu akan menampilkan pesan "Hello World". Coba modifikasi kodenya, seperti mengubah teks pesan, dan simpan file. Saat Anda menjalankan kembali plug-in tersebut, perubahan Anda akan diterapkan tanpa memulai ulang Lumi.

Cobalah bereksperimen dengan mengubah jalur menu. Misalnya, `"<Image>/File"` akan memasukkannya ke dalam menu File, dan `"<Image>/File/Funky"` akan membuat bagian baru di menu File. Ini adalah cara terbaik untuk menyesuaikan tempat plug-in Anda muncul dan mengatur alat Anda.