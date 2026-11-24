---
title: "Alat Kuas"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: b11c0248addcd834613763c198cf5d00356b8d31e99d3dad18a1cea96e6b7e3f
url: "hub/features/paintbrush"
translation_lock: true
---
Alat Kuas adalah instrumen melukis inti Lumi: cara responsif dan ekspresif untuk menggambar, melukis, membayangi, memberi tekstur, dan membuat tanda langsung di kanvas. Dirancang terasa langsung sambil tetap memberi ruang bagi seniman membentuk perilaku goresan.

Alih-alih satu kuas tetap, alat ini bertindak sebagai sistem melukis. Bentuk, tekstur, butiran permukaan, gerakan, tekanan, waktu, warna, dan seberapa penuh kuas menemui permukaan semuanya berkontribusi pada tanda akhir—cocok untuk garis bersih, lukisan lembut, media kering, goresan kaligrafi, tekstur tersebar, ikatan bulu, dan formasi multi-kepala.

## Tanda kuas ekspresif

Kuas dapat berbasis cap bitmap, bentuk prosedural, atau sumber animasi berbasis frame. Goresan dapat berkisar dari tanda bulat lembut sederhana hingga kepala kuas kaya tekstur atau berkembang. Mesin melukis yang sama mendukung gambar presisi, penumpukan pelukis, tanda dekoratif, dan pecahan gaya media alami.

Saat kuas menjadi rumit secara visual, pratinjau dapat tetap disederhanakan agar melukis tetap responsif dan mudah dibaca.

## Dinamika dan respons input

Alat Kuas merespons input langsung seperti tekanan stylus, kecepatan, arah, kemiringan, dan nilai kontroler lain. Sinyal ini dapat memengaruhi goresan terlihat dalam banyak cara: ketebalan, opasitas, sudut, respons tekstur, perilaku warna, jarak, dan kualitas lain dapat berubah seiring gerakan tangan.

Kuas terasa kurang seperti pola cap dan lebih seperti instrumen gambar fisik. Sentuhan ringan menghasilkan tanda halus, gerakan cepat membuka tekstur atau bentuk, dan perilaku peka arah membantu goresan mengikuti gerakan tangan.

## Perilaku goresan

Goresan dapat langsung dan seketika, atau dibantu smoothing dan stabilisasi. Fitur ini mengurangi jitter yang tidak diinginkan, memperhalus perubahan mendadak, dan membuat gerakan panjang terasa lebih terkontrol tanpa menghilangkan karakter input seniman.

Kuas juga mendukung pendekatan berbeda untuk penumpukan cat. Kuas dapat berperilaku seperti goresan kontinu, mengakumulasi cap berulang, atau mengeluarkan tanda seiring waktu saat penunjuk ditahan. Fleksibilitas ini berguna untuk garis disengaja maupun konstruksi tonal lebih lambat.

Untuk tanda kaligrafi atau seperti tinta, Kuas dapat menghasilkan goresan berbentuk lebih kontinu alih-alih hanya mengandalkan cap berulang. Hasilnya bentuk seperti pita yang mengalir dan merespons gerakan serta kecepatan secara alami.

## Pengambilan goresan dan rendering simulasi

Kuas dapat menangkap sampel kecil cara preset biasanya digambar tangan, lalu memakai profil itu saat merender goresan yang ditentukan geometri, bukan gerakan langsung. Garis lurus Shift-klik, path bergoresan, dan seleksi bergoresan dapat memakai pola tekanan dan kecepatan yang ditangkap preset aktif alih-alih berperilaku seperti garis mekanis datar.

Goresan konstruksi lebih dekat ke karakter kuas. Garis dari path dapat mulai lembut, membangun tekanan, mengecil, atau memvariasikan respons kecepatan dengan cara serupa sampel goresan tangan, sambil tetap mengikuti bentuk path, tepi seleksi, atau gerakan garis lurus persis.

## Pasca-pemrosesan

Kuas dapat merekam goresan saat Anda menggambar, lalu memutar ulang gerakan yang ditangkap setelah pena diangkat, menyempurnakan path sebelum tanda akhir ditetapkan. Anda dapat sketsa bebas dan tetap mendapat arah lebih rapi, sudut lebih tajam, atau struktur lebih disengaja tanpa menggambar dengan presisi mekanis.

Ini membuka arsiran dan tanda konstruksi bergaris yang snap ke sudut bersih sambil mempertahankan panjang dan karakter gambar tangan, goresan pita stabil kemiringan, dan replay sadar sudut yang memperlakukan tikungan dan lintasan lurus berbeda. Kuas multi-kepala dapat berbagi path yang dikoreksi sementara masing-masing kepala mempertahankan variasinya; dinamika masih dapat membentuk goresan sepanjang kurva akhir saat replay. Pemrosesan pasca-gambar berlaku untuk goresan yang ditarik, bukan emisi airbrush kontinu.

## Warna dan tekstur

Goresan kuas dapat memakai warna cat aktif, merespons gradien, atau memvariasikan warna melalui dinamika. Penanganan tekstur memungkinkan kuas bergeser antara cakupan solid dan tanda permukaan pecah—berguna untuk efek kuas kering, grain, dan bayangan ekspresif.

Karena warna dan tekstur bagian dari sistem dinamis yang sama dengan bentuk dan opasitas, satu goresan dapat berevolusi saat bergerak melintasi kanvas alih-alih tetap seragam secara visual.

Tekstur pada ujung kuas memecah cap itu sendiri. Tekstur permukaan, yang dijelaskan berikutnya, adalah butiran dan gigi permukaan yang ditemui goresan.

## Tekstur permukaan

Tekstur permukaan memberi goresan butiran dan gigi suatu permukaan: kertas, kanvas, atau pola lain. Lumi menyertakan butiran yang sesuai untuk itu. Bagian terang pola meloloskan cat. Bagian gelap menahannya, sehingga tanda pecah seperti media kering pecah di atas gigi.

Letak butiran itu dapat berubah bersama goresan. Setiap goresan dapat mulai di tempatnya sendiri pada pola, dan sebuah belokan dapat menggulirkan pergeseran baru. Bayangan bolak-balik adalah tempat hal ini terlihat: setiap pembalikan mengambil fase baru dari gigi, dan lintasan yang bertumpuk membangun bayangan yang pecah dan meyakinkan. Kurva yang landai dapat mempertahankan fase yang sudah ada di bawah goresan, sehingga gigi tetap koheren sepanjang sapuan.

Kuas cap dan goresan kaligrafi sama-sama dapat menerima butiran ini. Sederet cap dan pita tinta yang kontinu dapat pecah di atas jenis gigi yang sama. Pada goresan kaligrafi garis tetap satu tanda yang terbentuk, dan butiran memutuskan di mana pita itu terbuka dan di mana ia tertahan.

Seberapa terbuka gigi dapat berubah sepanjang goresan. Tekanan dan masukan lain dapat bergerak antara pembacaan yang lebih kering dan yang lebih penuh. Pada pembacaan yang lebih kering, lubang tetap bersih. Saat goresan terbuka, pulau cat dapat tumbuh ke dalam lubang itu, sehingga butiran terisi dari tepinya. Pada pembacaan penuh, goresan sesuai dengan pola yang lebih terbuka.

Cap yang bertumpuk di dalam satu goresan dapat membangun opasitas di pulau-pulau itu. Cap-cap itu juga dapat meletakkan satu lembar cakupan yang ditulis ulang oleh putaran berikutnya dari goresan yang sama, sehingga butiran tidak menumpuk pada dirinya sendiri.

## Kepala kuas dan formasi

Alat Kuas dapat melukis dengan lebih dari satu kepala sekaligus. Beberapa kepala dapat diatur di sekitar path goresan untuk membuat tanda ujung pena, goresan kipas, perilaku seperti bulu, pola semprot, formasi bertekstur, atau arsiran terstruktur.

Kepala dapat mengikuti arah perjalanan, bervariasi satu sama lain, dan tersebar sehingga goresan terasa organik alih-alih berulang mekanis. Sangat berguna untuk kuas media alami, goresan dekoratif, dedaunan, bulu, arsiran, dan tanda lain yang mendapat manfaat dari ketidakteraturan terkendali.

Kuas yang dihasilkan juga dapat dibangun sebagai bidang bulu. Bentuk bidang itu, dan cara bidang itu bergerak sepanjang goresan, dijelaskan di bawah.

## Bentuk bulu

Kuas bulu adalah bidang tanda terpisah yang tertahan di dalam bentuk yang dihasilkan. Garis luar memutuskan di mana bulu dapat duduk: lingkaran, persegi, belah ketupat, segitiga, bentuk berduri, sudut yang dilunakkan, atau proporsi yang diregangkan. Setiap bulu menyimpan tandanya sendiri di dalam garis luar itu.

Jarak dan ukuran mengatur kerapatan dan ukuran rambut. Variasi ukuran berjalan dari kepala yang seragam hingga rambut dari ukuran penuh turun mendekati kosong. Keteraturan menggerakkan bidang antara kisi yang rata dan sebaran yang lebih longgar. Penggumpalan menarik bulu ke lebih sedikit pusat sehingga kepala terbaca sebagai ikatan. Bidang dapat dijarangkan.

Ujung dapat tetap bulat atau menjadi poligon kecil di dalam setiap bulu, berjarak rata atau dibiarkan compang-camping, duduk di tepi atau ditarik ke dalam. Di tempat ujung bertumpuk, yang tampak adalah tanda yang lebih kuat.

Setiap kepala kuas multi-kepala dapat membawa susunannya sendiri, sebuah kipas dari ikatan yang sejenis.

Ukuran bulu dapat mengikuti tekanan stylus dan masukan lain. Saat ukuran berubah, kepala dapat digambar ulang, sehingga sentuhan ringan dan tekanan kuat berbeda pada rambutnya.

## Bulu sepanjang goresan

Saat goresan digambar, bulu dapat bergerak sebagai ikatan yang hidup. Bulu tertinggal di belakang tangan, berayun pada belokan lalu menetap di arah baru, menolak saat goresan berbalik, terpuntir mengikuti putaran kuas, membuka di bawah tekanan, dan memegas kembali ke susunan istirahat. Bulu yang tertinggal dapat melewati garis luar saat istirahat.

Seberapa jauh mereka berjalan, seberapa erat gumpalan saling menahan, dan seberapa cepat mereka menetap dapat dibentuk. Ikatan yang kaku dapat tetap pada satu jejak. Yang lebih lunak dapat terbelah, terlambat pada puntiran, dan menyimpan sedikit lengkungan sebelum kembali. Gerakan yang sama menghasilkan gerak yang sama.

Airbrush meletakkan susunan bulu yang baru saat ia memancarkan cat, sehingga goresan yang ditahan terus memperbarui semprotan. Goresan kaligrafi mempertahankan ujung pena yang kontinu, yang tetap dapat mengambil tekstur permukaan. Gerakan hidup ini milik kuas bulu yang dicap.

## Gradien tepi

Sebuah goresan dapat memegang sisi yang tajam dan sisi yang meluruh, seperti arang atau pastel. Ke arah sisi yang lembut, cat dapat memudar, ujung mengering dan pecah, dan gigi kertas lebih banyak terlihat. Bulu dapat menipis dan rontok di sana, sementara formasi kepala tetap rapat dan padat di sisi yang keras, dan sisi yang lembut mengendur lalu menyebar.

Turunnya gradien itu, dan arah yang dihadapinya, dapat dibentuk dengan bebas. Gradien dapat berputar mengikuti goresan, atau mengikuti kemiringan stylus, sehingga bayangan bolak-balik mempertahankan tepi keras yang sama.

## Muatan kuas dan pickup cat

Kuas juga dapat mensimulasikan berapa banyak cat atau material yang dibawa kuas. Saat goresan berlanjut, muatan dapat berkurang bertahap, sehingga tanda menjadi lebih ringan, kering, tipis, kasar, atau lebih pecah—tergantung dinamika kuas.

Muatan dapat diperkenalkan kembali antar goresan, dipertahankan di level tertentu, atau dipakai sebagai sinyal kontrol langsung untuk perilaku kuas lain. Memungkinkan kuas terasa lebih seperti media nyata: basah di awal goresan, habis seiring jarak, lalu dicelupkan lagi untuk lintasan berikutnya.

## Kontak kuas

Kontak kuas adalah cara goresan menemui kertas lalu melepaskannya. Sepanjang perjalanan, tanda dapat menggembung dan menyempit, terangkat sejenak, lalu kembali menempel. Pensil, arang, kuas kering, atau spidol yang lelah dapat menebal, putus, lalu menyambar lagi.

Seberapa mudah itu terjadi dapat mengikuti tangan. Sentuhan ringan putus lebih cepat. Tekanan kuat lebih lama tetap menempel. Tanda yang lebih tebal dapat menahan kontak sementara yang lebih halus membuka lubang, dan penyempitan dapat menjadi saat goresan yang berat mulai putus. Terangkatnya dapat terjadi tiba-tiba, atau tanda menyusut ke dalam celah lalu tumbuh kembali.

Kuas yang bermuatan penuh dapat menemui kertas dengan cara lain. Gembungan dan penyempitan berjalan lebih kuat, dan tanda menahan kontak pada rentang yang lebih panjang. Saat muatan habis, interaksi itu memudar, dan goresan menjadi lebih tenang serta lebih mudah putus.

Sekumpulan bulu dapat melakukan ini helai demi helai, sehingga goresan terurai menjadi untai terpisah lalu berkumpul kembali. Kepala dalam satu formasi dapat putus bersama, atau masing-masing sendiri. Goresan kaligrafi terangkat sebagai satu ujung, dan pita berhenti di tempat kontak berhenti. Setiap goresan baru menemukan iramanya sendiri.

## Animasi dan variasi

Sumber kuas animasi dapat mengganti frame seiring goresan maju, memberi kuas rasa gerak dan variasi. Randomisasi dan variasi per goresan menjaga tanda berulang tidak terlihat identik; seed stabil mempertahankan karakter konsisten saat repeatability diperlukan.

Perilaku ini berguna untuk kuas yang seharusnya terasa hidup: cap bertekstur yang berubah halus seiring waktu, atau alat multi-kepala di mana setiap kepala punya kepribadian sendiri. Kuas bulu punya animasi tersendiri di dalam goresan, dijelaskan di atas.

## Goresan OpenGL

Preset alat dapat melukis goresannya dengan OpenGL. Perbedaannya paling terasa pada airbrush besar dan cap tekstur besar, yang tetap mengikuti tangan saat tanda tumbuh. Jendela gambar juga dapat menggambar tumpukan lapisan dengan OpenGL, yang menjaga navigasi kanvas tetap mulus. Keduanya dijelaskan di [OpenGL](/hub/features/opengl/).

## Alur kerja berfokus seniman

Alat Kuas diatur agar keputusan melukis umum tetap dekat, sementara pilihan setup yang jarang dipakai tetap tidak mengganggu. Tujuannya menjaga alat mudah dipakai saat melukis sambil tetap mendukung kustomisasi mendalam untuk desain kuas.

Secara keseluruhan, Kuas dibuat menutupi melukis sehari-hari dan mark-making khusus: sketsa cepat, ilustrasi halus, rendering bertekstur, pekerjaan tinta ekspresif, dan efek kuas prosedural kompleks—semuanya berbagi fondasi fleksibel yang sama.
