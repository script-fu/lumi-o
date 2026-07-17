---
title: "File"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
url: "hub/scripting/tutorials/files"
translation_lock: true
---
Bekerja dengan file dan direktori sangat penting untuk pengembangan Scheme. Baik Anda menyimpan keluaran, memuat sumber daya, atau mengatur struktur proyek, memahami operasi file akan membuat skrip Anda lebih kuat dan ramah pengguna.

Halaman ini mencakup tugas umum file dan direktori: membaca jalur, membuat direktori, dan mengumpulkan input folder melalui parameter GUI.

## Direktori Beranda Pengguna

Lumi hanya untuk Linux, jadi direktori home pengguna berasal dari variabel lingkungan `HOME`.

Untuk mendapatkan direktori home pengguna sebagai string:

```scheme
(getenv "HOME")
```

Contoh keluaran:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Ada juga variabel global `DIR-SEPARATOR`, yang merupakan pemisah jalur khusus platform. Di Lumi (Linux), selalu `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Mendapatkan Lokasi Direktori

Anda dapat menanyakan lokasi direktori kepada pengguna dalam dialog Scheme untuk sebuah plug-in.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME` menyediakan browser ke direktori.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Di sini Anda memvalidasi dua input direktori (sumber dan tujuan) dan kembali ke default jika jalur GUI kosong/tidak valid.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Jika Anda tertarik dengan detail penerapannya, cari sumber plug-in untuk `validate-path-and-dir`.

## Membuat Direktori

Scheme menyediakan perintah ```dir-make``` untuk membuat direktori. Perintah ini mengambil jalur yang dipisahkan "/" dan membuat satu direktori dengan parameter opsional untuk hak istimewa. Kami tidak memberikan jalur khusus platform.

Biasanya Anda perlu membuat banyak direktori untuk jalur praktis. Anda dapat menggunakan pembungkus untuk ```dir-make``` untuk membantu Anda di sini.

```scheme
;; Tujuan: Pembungkus (dir-make) yang membuat jalur dari platform
;;          jalur yang disediakan. Selalu memancarkan pemisah gaya Linux untuk dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Direktori root
    ;; Membuat sisa direktori langkah demi langkah
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; membangun jalur
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Catatan: Fungsi ini juga menggunakan ```file-exists?``` bawaan untuk melewati panggilan yang tidak perlu. Ia mengembalikan #t jika file atau direktori yang ditunjukkan ada, dan #f jika tidak ada atau jika tidak dapat diakses oleh pengguna yang meminta.

## Membangun Jalan

Anda juga perlu memecah dan membangun kembali jalur di Scheme.

Untuk membagi jalur menjadi beberapa bagian, gunakan ```strbreakup```:

### Contoh Jalur Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Catatan: Garis miring di depan dan di belakang menjadi elemen string kosong dalam daftar yang dihasilkan.

Untuk membangun kembali jalur, gunakan ```string-append```:

### Pembuatan Jalur Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```