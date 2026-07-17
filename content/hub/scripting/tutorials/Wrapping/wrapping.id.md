---
title: "Pembungkus"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_source_sha256: 7b176d9b546b2566812e825fb2e10da5dd4e86f0e79be2c362a4775546110ac6
translation_lock: true
url: "hub/scripting/tutorials/Wrapping/wrapping"
---
Perintah Scheme beroperasi pada tingkat rendah, artinya tugas sederhana pun memerlukan banyak langkah. Namun, perincian ini menawarkan fleksibilitas, Anda dapat menggabungkan perintah menjadi fungsi-fungsi kecil yang dapat digunakan kembali yang melakukan apa yang Anda perlukan. Pembungkusan bukanlah konsep hitam-putih; itu bisa berkisar dari alias sederhana untuk perintah yang sering digunakan hingga fungsi yang lebih kompleks yang mengelola seluruh alur kerja. Terkadang, wrapper hanyalah fungsi praktis untuk meningkatkan keterbacaan, sementara di kasus lain, wrapper berkembang menjadi utilitas berfitur lengkap yang merangkum beberapa operasi.

### Mengapa Fungsi Pembungkus?

Ada beberapa manfaat utama fungsi pembungkus:

- **Menyederhanakan tugas yang berulang** – Daripada mengulangi perintah tingkat rendah, gabungkan perintah tersebut dalam fungsi pembantu dan gunakan kembali.
- **Meningkatkan keterbacaan** – Memberikan fungsi terbungkus dengan nama yang jelas dan deskriptif membuat kode Anda lebih mudah dipahami secara sekilas.
- **Merangkum kompleksitas** – Daripada berurusan dengan daftar perintah yang panjang dan samar, loop yang sangat bertumpuk, atau pernyataan pesan yang rumit, Anda dapat memecahnya menjadi fungsi pembantu yang lebih kecil dan terstruktur dengan baik.
- **Meningkatkan kemudahan pemeliharaan** – Jika fungsi inti dari suatu perintah berubah, Anda hanya perlu memperbarui fungsi terbungkus satu kali, mengisolasi plug-in Anda dari detail perubahan tersebut.
- **Mendorong penggunaan kembali kode** – Setiap helper menjadi bagian dari perpustakaan Anda, membuat skrip masa depan lebih cepat untuk ditulis dan di-debug.

Seiring berkembangnya plug-in Anda, wrapper membantu Anda menjaga logika inti tetap terbaca dan mengisolasi detail yang berulang.

Keuntungan lain dari fungsi pembungkusan adalah mengintegrasikannya ke dalam penyorot sintaksis seperti Visual Studio Code. Hal ini meningkatkan keterbacaan dan navigasi, membuat skrip lebih jelas. Dalam plug-in yang menggunakan fungsi khusus, fungsi apa pun yang disorot hijau mengonfirmasi bahwa fungsi tersebut direferensikan dengan benar dari perpustakaan Anda.

Jika Anda mengelola pustaka pembantu Anda sendiri, pertimbangkan untuk menambahkan nama fungsi proyek Anda ke penyorotan sintaksis editor Anda. Itu membuat navigasi dan pemfaktoran ulang lebih cepat.

Contoh:

### Seed acak

```scheme
;; Tujuan: Mengembalikan bilangan bulat acak untuk benih filter
(define (random-seed)
  (msrg-rand))
```

Meskipun Anda dapat menggunakan ***msrg-rand*** secara langsung dalam kode Anda, membungkusnya di dalam fungsi yang disebut ***random-seed*** akan meningkatkan keterbacaan. Dengan memberi nama fungsi yang jelas dan deskriptif, akan lebih mudah untuk memahami tujuannya secara sekilas.

Selain itu, mendefinisikan ***random-seed*** sebagai fungsi mandiri memungkinkan Anda menggunakannya di mana saja di plug-in sambil memusatkan implementasi di satu lokasi. Jika Anda perlu mengubah cara pembuatan seed, Anda hanya perlu memperbarui fungsi ini, membiarkan sisa kode Anda tidak tersentuh.

Misalnya, jika Anda memutuskan untuk beralih ke ***acak***:

```scheme
;; Tujuan: Mengembalikan bilangan bulat acak untuk benih filter
(define (random-seed)
  (random 1000))
```

Nama fungsinya tetap sama, memastikan skrip Anda terus bekerja tanpa modifikasi. Pendekatan ini menjaga kode Anda tetap fleksibel, mudah dipelihara, dan mudah dibaca.

### Mengekspor JPEG

Fungsi ekspor JPEG di Scheme hadir dengan banyak parameter, menawarkan kontrol yang baik atas cara gambar disimpan. Namun, dalam kebanyakan kasus, Anda hanya memperhatikan beberapa pengaturan utama, seperti nama file dan kualitas. Untuk menyederhanakan prosesnya, Anda dapat menggabungkan fungsinya.

```scheme
;; Tujuan: Menyimpan gambar sebagai JPEG dengan kualitas tertentu
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Menghindari jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

Dalam fungsi wrapper ini, sebagian besar opsi ekspor di-hardcode, hanya memperlihatkan parameter yang mungkin Anda sesuaikan: nama dan kualitas file. Pendekatan ini meningkatkan keterbacaan dan membuat penyimpanan gambar menjadi lebih sederhana.Selain itu, jika pengekspor Lumi berubah di masa mendatang, Anda hanya perlu memperbarui fungsi yang satu ini daripada mengubah setiap skrip yang mengekspor JPEG.

### Menggunakan Pembungkus

Untuk mengekspor JPEG di plug-in Anda, Anda cukup menyertakan perpustakaan dan memanggil fungsi khusus Anda:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Hal ini membuat kode Anda tetap bersih, mudah dibaca, dan mudah beradaptasi sekaligus memungkinkan Anda mengekspor JPEG secara efisien dengan sedikit usaha.

### Mengganti `car`

Fungsi ***car*** bisa jadi samar dan rentan terhadap kesalahan skrip. Sangat mudah untuk salah menerapkan ***car*** ke vektor atau item non-daftar, sehingga menyebabkan perilaku yang tidak terduga. Untuk membuat kode Anda lebih kuat dan mudah dibaca, Anda dapat menggabungkan fungsi ini dalam fungsi yang lebih aman.

```scheme
;; Tujuan: Mengembalikan item pertama dari daftar atau vektor.
;;          Memperingatkan jika input tidak valid atau kosong.
(define (first-item collection)
  (cond
    ;; Menangani daftar tidak kosong
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Menangani vektor tidak kosong
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Input tidak valid atau kosong
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Fungsi ini dengan aman mengambil item pertama dari daftar atau vektor sambil memberikan peringatan berguna ketika ditemukan input yang tidak valid atau kosong. Dengan menggunakan ***first-item*** dan bukan ***car***, Anda mengurangi risiko kesalahan yang tidak disengaja dan meningkatkan kejelasan skrip Anda.

#### Mengapa Menggunakan Pembungkus Ini?

- **Mencegah kerusakan skrip** – Menghindari kesalahan yang disebabkan oleh penerapan ***car*** ke non-daftar.
- **Mendukung daftar dan vektor** – Memperluas kegunaan lebih dari sekadar daftar.
- **Memberikan peringatan yang berarti** – Membantu men-debug masalah masukan yang tidak terduga.
- **Meningkatkan keterbacaan** – Nama fungsi menyampaikan tujuannya dengan jelas.

Dengan merangkum logika ini di item pertama, Anda membuat plug-in Anda lebih kuat dan mudah dikelola. Tentu saja, ini tergantung pada preferensi pribadi, Anda mungkin merasa nyaman menggunakan fungsi car, caar, cadr, dan Scheme serupa secara langsung.

### Membungkus Fungsi yang Dibungkus

Membungkus fungsi yang sudah dibungkus dapat lebih meningkatkan keterbacaan dan pemeliharaan. Misalnya, ketika bekerja dengan pasangan koordinat seperti ***pixel-coords (list 100 200)***, Anda dapat menggunakan:

```scheme
(first-item pixel-coords)
```

untuk mengambil koordinat ****x***. Namun, meski fungsional, ini tidak terlalu ekspresif. Sebaliknya, Anda bisa membungkus ***first-item*** dalam definisi yang lebih tepat untuk memperjelas maksud Anda.

```scheme
;; Tujuan: Mengembalikan koordinat x, untuk keterbacaan
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Tujuan: Mengembalikan koordinat y, untuk keterbacaan
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Mengapa Menggunakan Pendekatan Ini?

- **Meningkatkan kejelasan kode** – Daripada menggunakan fungsi akses daftar umum, Anda secara eksplisit mendefinisikan fungsi yang menjelaskan tujuannya.
- **Meningkatkan kemudahan pemeliharaan** – Jika representasi koordinat Anda berubah (misalnya menggunakan vektor, bukan daftar), Anda hanya perlu memperbarui fungsi kecil ini.
- **Mendorong konsistensi** – Menggunakan ***x-coord*** dan ***y-coord*** membuat skrip lebih mudah dibaca dan dipahami secara sekilas.

Sekarang, alih-alih menulis dalam Scheme generik:

```scheme
(car pixel-coords) ;; Mendapatkan koordinat x
(cadr pixel-coords) ;; Mendapatkan koordinat y
```

Anda dapat menulis dalam Scheme _kami_:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Dengan menggabungkan fungsi tingkat rendah dalam nama yang bermakna, Anda menciptakan cara yang lebih intuitif untuk bekerja dengan data, mengurangi kebingungan dan potensi kesalahan.

### Pembungkus yang Dikirim: Utilitas Stdlib

Lumi mengirimkan sekumpulan wrapper siap pakai yang dimuat secara otomatis saat startup, sehingga tersedia di plug-in apa pun atau di Konsol Scheme tanpa panggilan `(load ...)` apa pun. Pustaka ini (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm`, dan `paths.scm`) dibangun dengan prinsip yang persis sama dengan contoh di atas: mereka memberikan nama yang jelas untuk operasi tingkat rendah, menyembunyikan boilerplate berulang, dan menyediakan satu tempat untuk memperbarui jika perintah yang mendasarinya berubah.

Misalnya, `images.scm` menyediakan `image-get-open-list` sebagai pembungkus yang dapat dibaca di sekitar panggilan PDB mentah, dan `files.scm` mengekspos pembantu pembuatan jalur yang seharusnya memerlukan rantai `string-append` yang berulang.

Anda dapat menelusuri setiap nama yang diekspor, membaca dokumennya, dan melihat dari pustaka mana nama tersebut berasal di **[Utility Browser]({{< ref "/hub/scripting/reference/utility-browser" >}})** (Bantuan → Pemrograman → Utility Browser). Ini adalah demonstrasi praktis pembungkusan dalam skala besar, dan sumber pola yang berguna untuk dipinjam saat membangun perpustakaan pembantu Anda sendiri.

### Kesimpulan

Fungsi pembungkus adalah cara ampuh untuk menyederhanakan pengembangan Scheme, membuat skrip lebih mudah dibaca, dipelihara, dan kuat. Dengan merangkum kompleksitas dan hanya menampilkan detail yang diperlukan, Anda menciptakan pendekatan yang lebih terstruktur dalam menulis plug-in.

Poin penting dari pendekatan ini:

- **Menyederhanakan tugas yang berulang** – Daripada mengulangi perintah tingkat rendah secara manual, Anda membuat fungsi yang dapat digunakan kembali.
- **Meningkatkan keterbacaan kode** – Nama wrapper yang tepat membuat skrip lebih mudah dipahami.
- **Merangkum kompleksitas** – Detail tingkat rendah ditangani di dalam wrapper, menjaga skrip utama tetap bersih.
- **Meningkatkan kemudahan pemeliharaan** – Jika fungsi inti berubah, Anda hanya perlu memperbarui wrappernya, tidak semua skrip yang bergantung padanya.
- **Mendorong penggunaan kembali dan konsistensi** – Perpustakaan fungsi pribadi Anda berkembang seiring waktu, menjadikan pengembangan lebih cepat dan efisien.

Dengan menggunakan pembungkusan fungsi secara konsisten, Anda dapat mengubah cara Anda menulis plug-in Scheme, menciptakan lingkungan skrip yang lebih modular dan ekspresif.

Dengan mengingat prinsip-prinsip ini, Anda dapat terus menyempurnakan pendekatan Anda, mengembangkan versi Scheme yang lebih efisien dan disesuaikan untuk memenuhi kebutuhan spesifik Anda.

Langkah selanjutnya: identifikasi blok berulang dalam skrip Anda dan ekstrak pembantu kecil dengan nama yang jelas.