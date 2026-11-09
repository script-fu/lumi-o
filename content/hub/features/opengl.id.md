---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi dapat menggambar tumpukan lapisan dengan OpenGL, dan dapat melukis goresan kuas dengan cara yang sama. Navigasi kanvas tetap mulus. Airbrush besar dan cap tekstur besar jauh lebih cepat. Kalau suatu alat melukis dengan cara ini, pilihan itu tersimpan di preset alat tersebut.

## Tumpukan lapisan

Jendela gambar menampilkan tumpukan lapisan yang sudah disusun. Dengan OpenGL, gambar itu disimpan sebagai petak pada prosesor grafis, sudah disiapkan untuk tampilan. Geseran, pembesaran, rotasi, dan pembalikan mengubah pandangan atas petak-petak itu. Pada kanvas besar gesturnya tetap mulus, karena navigasi adalah perubahan pandangan atas gambar yang sudah ada di sana.

Suntingan memperbarui petak yang tersentuh. Panduan, kursor kuas, dan garis seleksi digambar di atas gambar itu.

Saat pena menekan, goresan yang memakai OpenGL dapat ditampilkan di tempatnya di antara lapisan di sekelilingnya, termasuk lapisan di atas lapisan yang sedang dilukis. Tanda ditulis ke dalam lapisan saat pena diangkat.

## Goresan, per preset alat

Melukis OpenGL adalah pilihan pada preset alat, di samping kuas dan dinamika. Satu preset dapat melukis pada prosesor grafis. Preset lain dapat mempertahankan jalur goresan biasa. Kembali ke suatu alat membawa pilihan itu kembali.

Keuntungan terbesar ada di tempat cap berukuran besar. Pada jalur biasa, airbrush lebar tertinggal dari stylus saat tanda tumbuh: cap mengantri, dan goresan membentuk celah, termasuk saat pena ditahan dan kuas terus memancarkan cat. Dengan OpenGL airbrush itu tetap menempatkan tanda seiring tangan.

Cap tekstur yang besar menunjukkan perubahan yang sama. Jalur biasa menghabiskan waktu lebih lama pada setiap cap saat cap itu membesar, sampai goresan macet. Goresan OpenGL tetap mengikuti gerakan. Cap yang membawa butiran permukaan juga dapat digambar dengan OpenGL.

Tanda bulu juga dapat digambar dengan cara ini. Cap bulu berujung bulat dapat dilukis dengan OpenGL sementara bulu berubah sepanjang goresan. Goresan kaligrafi dapat digambar sebagai satu pita yang kontinu, dan tekstur permukaan dapat memasker pita itu. [Tekstur permukaan](/hub/features/paintbrush/#tekstur-permukaan) dan [bulu sepanjang goresan](/hub/features/paintbrush/#bulu-sepanjang-goresan) milik kuas itu sendiri.

Sebagian goresan tetap pada jalur biasa. Pencampuran spektral, filter pada lapisan yang dilukis, dan sebagian mode campuran ada di antaranya. Tanda-tanda itu tetap dilukis.

## Saat kanvas biasa tetap dipakai

OpenGL dipakai saat perangkat grafis dapat menjalankannya dan gambarnya muat dalam memori yang disisihkan untuk kanvas. Jika tidak, Lumi menggambar jendela seperti sebelumnya. AppImage yang diterbitkan membawa pustaka grafisnya sendiri. Pada driver yang tidak dapat dipakai pustaka itu, termasuk driver proprietari NVIDIA, kanvas tetap pada jalur biasa itu.

## Cinnamon di X11, dan GNOME di Wayland

Lumi dikembangkan di Debian dengan Cinnamon dan X11. Itu tetap lingkungan pengembangan. GNOME di Wayland telah diuji untuk tampilan umum aplikasi dan untuk rendering OpenGL ini. Navigasi kanvas mulus di sana juga.
