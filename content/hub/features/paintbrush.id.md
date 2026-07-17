---
title: "Alat Kuas"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
url: "hub/features/paintbrush"
translation_lock: true
---
Alat Kuas adalah instrumen melukis inti Lumi: cara responsif dan ekspresif untuk menggambar, melukis, membayangi, memberi tekstur, dan membuat tanda langsung di kanvas. Dirancang terasa langsung sambil tetap memberi ruang bagi seniman membentuk perilaku goresan.

Alih-alih satu kuas tetap, alat ini bertindak sebagai sistem melukis. Bentuk kuas, tekstur, gerakan, tekanan, waktu, dan warna semuanya berkontribusi pada tanda akhir—cocok untuk garis bersih, lukisan lembut, efek media kering, goresan kaligrafi, tekstur tersebar, dan formasi kuas multi-kepala.

![brush-tool](/images/screens/brush-tool.jpg)

## Tanda kuas ekspresif

Kuas dapat berbasis cap bitmap, bentuk prosedural, atau sumber animasi berbasis frame. Goresan dapat berkisar dari tanda bulat lembut sederhana hingga kepala kuas kaya tekstur atau berkembang. Mesin melukis yang sama mendukung gambar presisi, penumpukan pelukis, tanda dekoratif, dan pecahan gaya media alami.

Saat kuas menjadi rumit secara visual, pratinjau dapat tetap disederhanakan agar melukis tetap responsif dan mudah dibaca.

![tool-setup](/images/screens/tool-setup.jpg)


## Dinamika dan respons input

Alat Kuas merespons input langsung seperti tekanan stylus, kecepatan, arah, kemiringan, dan nilai kontroler lain. Sinyal ini dapat memengaruhi goresan terlihat dalam banyak cara: ketebalan, opasitas, sudut, respons tekstur, perilaku warna, jarak, dan kualitas lain dapat berubah seiring gerakan tangan.

Kuas terasa kurang seperti pola cap dan lebih seperti instrumen gambar fisik. Sentuhan ringan menghasilkan tanda halus, gerakan cepat membuka tekstur atau bentuk, dan perilaku peka arah membantu goresan mengikuti gerakan tangan.

![dynamics](/images/screens/dynamics.jpg)

## Perilaku goresan

Goresan dapat langsung dan seketika, atau dibantu smoothing dan stabilisasi. Fitur ini mengurangi jitter yang tidak diinginkan, memperhalus perubahan mendadak, dan membuat gerakan panjang terasa lebih terkontrol tanpa menghilangkan karakter input seniman.

Kuas juga mendukung pendekatan berbeda untuk penumpukan cat. Kuas dapat berperilaku seperti goresan kontinu, mengakumulasi cap berulang, atau mengeluarkan tanda seiring waktu saat penunjuk ditahan. Fleksibilitas ini berguna untuk garis disengaja maupun konstruksi tonal lebih lambat.

Untuk tanda kaligrafi atau seperti tinta, Kuas dapat menghasilkan goresan berbentuk lebih kontinu alih-alih hanya mengandalkan cap berulang. Hasilnya bentuk seperti pita yang mengalir dan merespons gerakan serta kecepatan secara alami.

![stroke](/images/screens/stroke.jpg)

## Pengambilan goresan dan rendering simulasi

Kuas dapat menangkap sampel kecil cara preset biasanya digambar tangan, lalu memakai profil itu saat merender goresan yang ditentukan geometri, bukan gerakan langsung. Garis lurus Shift-klik, path bergoresan, dan seleksi bergoresan dapat memakai pola tekanan dan kecepatan yang ditangkap preset aktif alih-alih berperilaku seperti garis mekanis datar.

Goresan konstruksi lebih dekat ke karakter kuas. Garis dari path dapat mulai lembut, membangun tekanan, mengecil, atau memvariasikan respons kecepatan dengan cara serupa sampel goresan tangan, sambil tetap mengikuti bentuk path, tepi seleksi, atau gerakan garis lurus persis.

## Pasca-pemrosesan

Kuas dapat merekam goresan saat Anda menggambar, lalu memutar ulang gerakan yang ditangkap setelah pena diangkat, menyempurnakan path sebelum tanda akhir ditetapkan. Anda dapat sketsa bebas dan tetap mendapat arah lebih rapi, sudut lebih tajam, atau struktur lebih disengaja tanpa menggambar dengan presisi mekanis.

Ini membuka arsiran dan tanda konstruksi bergaris yang snap ke sudut bersih sambil mempertahankan panjang dan karakter gambar tangan, goresan pita stabil kemiringan, dan replay sadar sudut yang memperlakukan tikungan dan lintasan lurus berbeda. Kuas multi-kepala dapat berbagi path yang dikoreksi sementara masing-masing kepala mempertahankan variasinya; dinamika masih dapat membentuk goresan sepanjang kurva akhir saat replay. Pemrosesan pasca-gambar berlaku untuk goresan yang ditarik, bukan emisi airbrush kontinu.

## Warna dan tekstur

Goresan kuas dapat memakai warna cat aktif, merespons gradien, atau memvariasikan warna melalui dinamika. Penanganan tekstur memungkinkan kuas bergeser antara cakupan solid dan tanda permukaan pecah—berguna untuk efek kuas kering, grain, dan bayangan ekspresif.

Karena warna dan tekstur bagian dari sistem dinamis yang sama dengan bentuk dan opasitas, satu goresan dapat berevolusi saat bergerak melintasi kanvas alih-alih tetap seragam secara visual.

## Kepala kuas dan formasi

Alat Kuas dapat melukis dengan lebih dari satu kepala sekaligus. Beberapa kepala dapat diatur di sekitar path goresan untuk membuat tanda ujung pena, goresan kipas, perilaku seperti bulu, pola semprot, formasi bertekstur, atau arsiran terstruktur.

Kepala dapat mengikuti arah perjalanan, bervariasi satu sama lain, dan tersebar sehingga goresan terasa organik alih-alih berulang mekanis. Sangat berguna untuk kuas media alami, goresan dekoratif, dedaunan, bulu, arsiran, dan tanda lain yang mendapat manfaat dari ketidakteraturan terkendali.

![brush-heads](/images/screens/brush-heads.jpg)

## Muatan kuas dan pickup cat

Kuas juga dapat mensimulasikan berapa banyak cat atau material yang dibawa kuas. Saat goresan berlanjut, muatan dapat berkurang bertahap, sehingga tanda menjadi lebih ringan, kering, tipis, kasar, atau lebih pecah—tergantung dinamika kuas.

Muatan dapat diperkenalkan kembali antar goresan, dipertahankan di level tertentu, atau dipakai sebagai sinyal kontrol langsung untuk perilaku kuas lain. Memungkinkan kuas terasa lebih seperti media nyata: basah di awal goresan, habis seiring jarak, lalu dicelupkan lagi untuk lintasan berikutnya.

![material-state](/images/screens/material-state.jpg)

## Kontak permukaan kuas

Kuas juga dapat mensimulasikan hilangnya kontak intermittens dengan permukaan lukisan—tanda pecah yang muncul saat pensil, arang, kuas kering, atau spidol hampir habis hanya sebagian menyentuh kertas.

Saat simulasi kontak diaktifkan, kuas bersentuhan atau terangkat. Saat bersentuhan, tanda didepositkan normal. Saat terangkat, tidak ada material yang didepositkan dan goresan meninggalkan celah dengan panjang acak antara jarak minimum dan maksimum. Transisinya biner: efek tidak mengubah opasitas, ukuran, hardness, jarak, atau flow—hanya apakah cat ditaruh.

Seberapa mudah kontak hilang dibentuk ambang kontak, tekanan stylus, dan opsional muatan kuas. Ambang lebih tinggi membuat jeda lebih sering. Tekanan bertindak sebagai kekuatan stabilisasi: tekanan ringan meningkatkan peluang kehilangan kontak, tekanan kuat membuat goresan lebih mungkin tetap menempel. Saat muatan kuas diaktifkan, muatan rendah membuat tanda lebih pecah dan muatan tinggi membantu mempertahankan kontak—seperti alat yang masih membawa cukup material untuk menggrip permukaan.

Evaluasi kerugian kontak berdasarkan jarak tempuh goresan, bukan jumlah cap, sehingga kuas dengan jarak rapat atau jarang berperilaku konsisten. Fitur ini bekerja dengan rendering berbasis cap dan kaligrafi, menghasilkan celah koheren sepanjang goresan alih-alih cap terlewat terisolasi.

## Animasi dan variasi

Sumber kuas animasi dapat mengganti frame seiring goresan maju, memberi kuas rasa gerak dan variasi. Randomisasi dan variasi per goresan menjaga tanda berulang tidak terlihat identik; seed stabil mempertahankan karakter konsisten saat repeatability diperlukan.

Perilaku ini berguna untuk kuas yang seharusnya terasa hidup: bulu bergeser sepanjang goresan, cap bertekstur berubah halus seiring waktu, atau alat multi-kepala di mana setiap kepala punya kepribadian sendiri.

## Alur kerja berfokus seniman

Alat Kuas diatur agar keputusan melukis umum tetap dekat, sementara pilihan setup yang jarang dipakai tetap tidak mengganggu. Tujuannya menjaga alat mudah dipakai saat melukis sambil tetap mendukung kustomisasi mendalam untuk desain kuas.

Secara keseluruhan, Kuas dibuat menutupi melukis sehari-hari dan mark-making khusus: sketsa cepat, ilustrasi halus, rendering bertekstur, pekerjaan tinta ekspresif, dan efek kuas prosedural kompleks—semuanya berbagi fondasi fleksibel yang sama.
