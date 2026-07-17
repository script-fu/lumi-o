---
title: "Men-debug"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
url: "hub/scripting/tutorials/debugging"
translation_lock: true
---
Dalam skrip, tidak ada fungsi yang sempurna. Bahkan perintah yang paling andal pun bisa gagal ketika dihadapkan pada masukan atau kondisi yang tidak terduga. Untuk mencegah hal ini, Anda dapat menerapkan sistem debugging khusus dan mengadopsi teknik pemrograman defensif. Dengan menggabungkan fungsi standar dengan mekanisme penanganan kesalahan dan memberikan umpan balik yang informatif, Anda dapat membuat skrip Anda lebih kuat dan lebih mudah untuk memecahkan masalah.

Bagian penting dari strategi ini adalah menggunakan tanda debug global untuk mengontrol keluaran verbose, memungkinkan Anda mengaktifkan informasi debug mendetail bila diperlukan sekaligus menjaga keluaran tetap bersih selama eksekusi normal.

## Bendera Debug Global

Bendera debug global adalah cara sederhana namun efektif untuk mengontrol tingkat keluaran informasi selama eksekusi skrip. Saat diaktifkan, ini memberikan pesan debug terperinci yang sangat berharga untuk melacak masalah. Jika dinonaktifkan, output akan tetap ringkas untuk penggunaan produksi.

```scheme
;; Tujuan: Flag global untuk mengontrol output debug.
(define debug #f)
```

Secara default, proses debug dinonaktifkan. Untuk mengaktifkan keluaran verbose selama pengembangan, cukup setel tanda ke `#t`:

```scheme
;; Tujuan: Flag global untuk mengontrol output debug.
(define debug #t)
```

Kami juga dapat mengaktifkan atau menonaktifkan sementara debugging untuk bagian kode tertentu menggunakan fungsi pembantu.

### Kontrol Debug Lokal

Untuk kontrol yang lebih baik, Anda dapat mengaktifkan atau menonaktifkan debugging dalam bagian tertentu dari skrip menggunakan fungsi pembantu.

```scheme
;; Tujuan: Menonaktifkan mode debug untuk bagian kode.
(define (debug-off)
  (set! debug #f))

;; Tujuan: Mengaktifkan mode debug untuk bagian kode.
(define (debug-on)
  (set! debug #t))
```

Hal ini memungkinkan Anda mengontrol proses debug secara dinamis:

```scheme
(debug-on)  ;; Mengaktifkan output verbose

;; Logika skrip di sini

(debug-off) ;; Menonaktifkan output verbose
```

## Debug Sistem Pesan

Untuk menangani keluaran debug di Scheme secara efisien, Anda menggunakan pendekatan terstruktur yang melibatkan beberapa fungsi pembantu. Fungsi-fungsi ini memastikan bahwa pesan debug dan peringatan jelas, dapat dibaca, dan dipelihara.

### Ikhtisar Sistem Pesan Debug

Sistem pesan debug Anda terdiri dari komponen berikut:

1. `debug-message` – Menampilkan pesan debug saat debugging diaktifkan.
2. `serialize-item` – Mengubah berbagai tipe data Scheme menjadi representasi string.
3. `concat` – Menggabungkan beberapa item menjadi satu string.
4. `list->string` – Memformat daftar menjadi string yang dapat dibaca.
5. `message` – Menampilkan output di konsol pesan Lumi.
6. `warning-message` – Menampilkan pesan peringatan saat peringatan diaktifkan.

Masing-masing fungsi berperan dalam memformat dan menampilkan pesan terstruktur.

---

### Fungsi Pesan Debug

Fungsi `debug-message` adalah metode inti untuk menampilkan keluaran debug. Ini memastikan pesan hanya ditampilkan ketika debugging diaktifkan.

```scheme
;; Tujuan: Menampilkan pesan debug.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- Kondisi `when debug` memastikan pesan hanya muncul ketika debugging diaktifkan.
- Pesan diawali dengan `"> "` untuk kejelasan.
- Fungsi ini menggunakan `concat` untuk memformat konten pesan.
- Terakhir, ia memanggil `message` untuk mengirim hasilnya ke konsol pesan Lumi.

Contoh penggunaan:

```scheme
;; Tujuan: Mengembalikan posisi item di pohon, atau #f jika item tidak valid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Dengan mengaktifkan debugging, outputnya mungkin:

```scheme
> item: background-layer has tree position : 3
```

### Serialisasi Data untuk Pesan Debug

Pesan mungkin berisi tipe data berbeda seperti daftar, vektor, dan angka. Untuk memastikan formatnya benar, Anda menggunakan `serialize-item`.

```scheme
;; Tujuan: Mengonversi berbagai tipe data Scheme (daftar, vektor, pasangan, dll.)
;;          menjadi representasi string.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Daftar kosong
    ((and (string? item) (string=? item "")) "\"\"")  ; String kosong
    ((list? item) (list->string item))                ; Daftar bersarang
    ((vector? item)                                   ; Menangani vektor
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Menangani pasangan
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Angka
    ((symbol? item) (symbol->string item))            ; Simbol
    ((boolean? item) (if item "#t" "#f"))             ; Boolean
    ((string? item) item)                             ; String
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Contoh penggunaan:

```scheme
(serialize-item '(1 2 3))
```

Keluaran:

```scheme
list:
1
2
3
```

### Rangkaian Pesan

Untuk menggabungkan beberapa komponen pesan menjadi satu string, Anda menggunakan `concat`.

```scheme
;; Tujuan: Menggabungkan beberapa item menjadi satu string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Contoh penggunaan:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Memformat Daftar sebagai String

Fungsi `list->string` mengubah daftar menjadi string yang diformat.

```scheme
;; Tujuan: Mengonversi daftar item menjadi string yang mudah dibaca.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Pesan Peringatan

Fungsi `warning-message` bekerja serupa dengan `debug-message`, namun fungsi ini menampilkan peringatan meskipun proses debug dinonaktifkan.

```scheme
;; Tujuan: Menampilkan pesan peringatan.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Memastikan pesan hanya ditampilkan ketika peringatan diaktifkan (tanda `warning` disetel di `common.scm` sebagai `#t`).
- Panggilan `concat` untuk memformat konten pesan.
- Menggunakan `message` untuk mengirim output ke Lumi.

## Meningkatkan Fungsi Standar

Setelah sistem debugging diterapkan, Anda dapat meningkatkan perpustakaan fungsi Anda dengan memasukkan pesan terperinci. Ini memberikan wawasan tentang status item, nilai variabel, dan pemanggilan fungsi.

Contoh umum adalah `item-is-valid?`, yang menggabungkan `lumi-item-id-is-valid` menjadi `#t` atau `#f`. Jika `#f` dikembalikan, Anda dapat memicu `warning-message` pada kode panggilan, jika inputnya bukan angka, Anda dapat memberikan peringatan pada fungsi tersebut.

```scheme
;; Tujuan: Memeriksa apakah item valid, mengembalikan #t atau #f.
;;          Mengeluarkan peringatan jika item bukan angka.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Penggunaan Praktis

Saat mengembangkan plug-in Scheme, membungkus fungsi dengan cara ini secara signifikan mengurangi waktu debugging dan memastikan kode yang kuat dan dapat dipelihara. Dengan sistem debugging yang ada, Anda dapat menghasilkan aliran debug terstruktur di konsol kesalahan hanya dengan menekan satu tombol.

Dalam aliran debug ini, pemanggilan fungsi ditandai dengan tanda bintang (*), sehingga memudahkan pelacakan eksekusi skrip dan menentukan kegagalan, khususnya pada plug-in yang kompleks. Visibilitas ini membantu Anda memahami alur operasi dan mendiagnosis perilaku tak terduga secara efisien.

Pembungkus fungsi pesan Anda untuk menggunakan `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Contoh `call` yang digunakan dalam praktik:

```scheme
;; Tujuan: Menerapkan proses tekstur ke daftar mask grup yang diberikan
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Contoh aliran debug saat plug-in dijalankan:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Log terstruktur ini memberikan garis waktu yang jelas mengenai pemanggilan fungsi dan perubahan data, membuat proses debug dan analisis kinerja jauh lebih mudah.

## Kesimpulan

Dengan menerapkan sistem debugging terstruktur, Anda membuat skrip yang lebih aman dan mudah dikelola yang menawarkan wawasan real-time tentang eksekusinya.

### Poin Penting

- **Kontrol verbositas** – Gunakan tanda debug global untuk mengelola tingkat keluaran.
- **Berikan masukan yang jelas** – Menggabungkan fungsi standar dengan pesan debug yang informatif.
- **Meningkatkan ketahanan** – Tangani masukan tak terduga dengan baik untuk mencegah kesalahan.
- **Sederhanakan pemecahan masalah** – Pesan debug terstruktur memudahkan diagnosis dan perbaikan masalah.

Dengan pendekatan ini, skrip Anda secara efektif "menjelaskan dirinya sendiri" saat memproses data, mengurangi frustrasi, dan meningkatkan efisiensi alur kerja. Proses debug menjadi alat yang proaktif dan bukan tugas reaktif, sehingga membuat proses pembuatan skrip Anda menjadi lebih lancar dan bermanfaat.