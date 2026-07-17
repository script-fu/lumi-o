---
title: "Filter"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
url: "hub/features/filters"
translation_lock: true
---
Menu Filter Lumi menyatukan penyesuaian korektif, efek lensa bergaya, generator tekstur prosedural, perlakuan terinspirasi cetak, dan alat analisis di satu tempat. Urutan menu praktis, bukan akademis: alat blur dan enhancement berdampingan, distorsi dan efek pencahayaan dikelompokkan menurut tampilan, generator tekstur atau pola disatukan saat tujuannya membangun materi sumber, bukan memodifikasi gambar yang ada.

Dialog filter mengikuti alur kerja umum yang sama. Preset, pratinjau, split view, dan kontrol opasitas atau blend memungkinkan efek disetel cepat; pada lapisan, hasil dapat tetap sebagai filter non-destruktif yang dapat diedit alih-alih langsung digabungkan. Lumi juga menyimpan riwayat filter terkini, sehingga mengulangi efek terakhir atau membuka dialog terakhir menjadi bagian ritme melukis normal—bukan tugas terpisah.

## Blur

### Gaussian Blur

Gaussian Blur adalah filter pelembut standar Lumi: blur bersih dan merata dengan kontrol ukuran horizontal dan vertikal terpisah, penanganan tepi, dan opsi kernel. Pilihan tujuan umum untuk soft focus, mask yang dilembutkan, kedalaman atmosfer, dan alur kerja di mana blur itu sendiri harus tetap netral.

### Pixelize

Pixelize mengurangi detail menjadi struktur blok disengaja, bukan blur lembut. Dialog mengekspos lebar blok, tinggi blok, offset, bentuk piksel, dan perilaku fill—berfungsi sebagai efek sensor kasar maupun mosaik terkontrol atau perlakuan grafis resolusi rendah.

### Selective Gaussian Blur

Selective Gaussian Blur melembutkan di dalam wilayah sambil berusaha mempertahankan tepi lebih kuat. Berguna saat gambar butuh tekstur lebih tenang atau mengurangi gangguan visual halus tanpa kehilangan batas bentuk besar yang masih perlu terbaca jelas.

### Lens Blur

Lens Blur salah satu filter blur Lumi yang lebih berfokus ilustrasi. Kontrolnya dibangun di sekitar bentuk iris poligon, kelengkungan blade, stretch anamorphic, highlight boosting, dan wilayah fokus yang dapat dikonfigurasi—berperilaku kurang seperti pelembut generik dan lebih seperti alat depth-of-field bergaya dengan bokeh berbentuk.

### Tilt-shift

Tilt-shift menjaga pita fokus terkontrol tetap tajam sambil secara progresif memblur gambar di atas dan di bawahnya. Sudut pita, feather, bias perspektif, bentuk iris, dan miniature boost dialog membuatnya cocok untuk pemandangan miniatur, tampilan arsitektur, dan komposisi di mana fokus terbaca sebagai garis desain, bukan isyarat kedalaman melingkar.

### Circular Motion Blur

Circular Motion Blur menyebarkan detail di sekitar titik pusat, mengubah tepi menjadi jejak rotasi. Pilihan alami untuk subjek berputar, energi seperti turbin, atau ilustrasi yang butuh kesan gerakan orbital.

### Linear Motion Blur

Linear Motion Blur membentangkan detail dalam satu arah, mensimulasikan perjalanan, gerakan kamera, atau gestur cepat melintasi frame. Sangat berguna saat gerakan perlu terasa terarah dan grafis, bukan menyebar.

### Zoom Motion Blur

Zoom Motion Blur memancarkan detail keluar dari pusat, menghasilkan kesan terburu-buru menuju atau menjauhi pemirsa. Cocok untuk momen impact, garis kecepatan, dan komposisi yang butuh energi zoom kamera tanpa mengecat ulang seluruh gambar.

## Enhance

### High Pass

High Pass mengisolasi kontras lokal halus alih-alih perubahan tonal luas. Dengan hanya skala dan kontras yang dikelola, alat sederhana untuk mengekstrak detail tepi, membangun overlay tajam, atau menyiapkan pass sharpening yang menekankan struktur lebih dari warna.

### Noise Reduction

Noise Reduction langkah berlawanan: menekan variasi halus yang tidak diinginkan agar bentuk lebih besar terbaca lebih jelas. Berguna saat material scan, tekstur terkompresi, atau bagian overworked perlu disederhanakan sebelum melukis atau memfilter lebih lanjut.

### Sharpen

Sharpen memakai model unsharp-mask, dengan radius, amount, dan threshold mengontrol seberapa kuat kontras lokal didorong. Dalam praktik, cocok memulihkan kejernihan setelah blur, resize ekspor, atau pass finishing halus di mana detail perlu maju tanpa mengubah setiap piksel jadi noise.

## Warna

### Tonal Grading

Tonal Grading memetakan ulang warna menurut rentang tonal, bukan dengan membentuk ulang kontras atau menggambar kurva. Luminance setiap piksel memilih blend halus tiga warna pengguna untuk shadow, midtone, dan highlight; gambar mempertahankan struktur terang-gelap sementara palet bergeser. Kekuatan per wilayah, bias balance gaya Lightroom (kiri favor grade shadow, kanan highlight), dan kelembutan transisi mengontrol seberapa jauh setiap warna menjangkau dan seberapa lembut grade tumpang tindih. Ditujukan untuk ilustrasi, komik, concept art, dan foto saat tujuannya grade atau look koheren.

## Distort

### Chromatic Aberration

Chromatic Aberration memisahkan saluran warna keluar dari pusat yang dipilih, dengan kontrol arah radial atau tangensial, bias antar pasangan saluran, falloff, dan pelestarian luminance. Kode dan dialog memperlakukannya sebagai alat dua arah: dapat menambah fringing lensa bergaya untuk energi, atau membalik tanda memperbaiki aberation ringan pada materi sumber.

### Lens Distortion

Lens Distortion membentuk ulang gambar melalui kelengkungan barrel atau pincushion, term tepi, kompensasi zoom, offset pusat, dan corner brightening. Berguna mengoreksi gambar yang terasa optically bent maupun sengaja mendorong gambar ke karakter lensa wide-angle atau retro.

## Pencahayaan

### Bloom

Bloom mengubah area terang menjadi glow terkontrol, dengan threshold, softness, radius, dan strength menentukan seberapa jauh cahaya menyebar dan seberapa kuat cahaya mengangkat gambar. Kontrol exposure-limiting tambahan menjaga Bloom berguna sebagai efek highlight, bukan washout otomatis.

### Sky

Sky lebih dari overlay tint atau gradien: Sky merender langit analitik memakai model Preetham, Hosek/Wilkie, atau Nishita. Dialog mengekspos proyeksi, sudut matahari, turbidity, kepadatan atmosfer, altitude, kontrol sun-disc, dan exposure—dapat membangun dari backdrop jernih sederhana hingga langit sunset atau twilight yang lebih grounded secara fisik.

### Vignette

Vignette menggelapkan, mewarnai, atau bahkan menghapus menuju tepi gambar, dengan kontrol shape, radius, softness, gamma, proportion, squeeze, rotation, dan posisi on-canvas. Berfungsi sebagai perlakuan tepi fotografi klasik, namun cukup fleksibel sebagai framing mask atau spotlight komposisi tidak beraturan.

## Noise

### HSV Noise

HSV Noise mengacak hue, saturation, dan value secara independen. Berguna saat gambar butuh keaktifan warna atau ketidakstabilan analog tanpa sepenuhnya merusak struktur lokal.

### Hurl

Hurl versi ekstrem noise: mengganti piksel dengan warna sepenuhnya acak. Paling baik dipikirkan sebagai sumber chaos destruktif untuk pekerjaan glitch, tekstur distressed, atau mask yang butuh breakup agresif.

### Pick

Pick mengganti setiap piksel dengan tetangga yang dipilih acak, sehingga gambar tetap terkait sumbernya alih-alih menjadi static murni. Hasilnya variasi granular acak yang terasa lebih organik daripada noise sepenuhnya acak.

### Spread

Spread menyebarkan piksel dengan memindahkannya acak dalam radius. Berguna saat Anda ingin gangguan tanpa gerakan: permukaan pecah, tepi tersmeared, atau tekstur distressed yang masih membawa hubungan warna gambar sumber.

### Fractal

Fractal menghasilkan fractal Perlin noise tileable, sangat berharga sebagai sumber reusable untuk mask, awan, tekstur kertas, breakup seperti terrain, dan overlay prosedural. Karena tileable, dapat memenuhi alur kerja lebih besar tanpa jahitan jelas.

### Blue Noise Grain

Blue Noise Grain generator grain monokrom gaya film-dan-cetak Lumi. Preset ukuran grain, blue-noise masking, midtone bias, shadow bias, dan kontrol seed pada dialog menunjukkan desain menempatkan grain merata dan terkontrol—bukan hanya menyemprot bintik monokrom acak ke gambar.

### Risograph Grain

Risograph Grain membangun logika grain yang sama tetapi mengubahnya menjadi efek cetak dua-plate. Warna tinta terpisah, plate balance, misregistration disengaja, dan variasi seeded cocok untuk poster, estetika cetak indie, dan ilustrasi yang seharusnya terasa overprinted fisik alih-alih sempurna digital.

### Halftone (FM)

Halftone (FM) menciptakan halftone stokastik frequency-modulated memakai blue-noise atau metode thresholding terkait. Dengan mode warna monokrom, duotone, dan CMYK, plus kontrol dot-gain dan plate decorrelation, ditujukan untuk tekstur seperti cetak yang tetap irregular dan hidup alih-alih jatuh ke grid kaku.

## Tepi

### Difference of Gaussians

Difference of Gaussians mendeteksi tepi dengan mengurangkan dua versi blur gambar satu sama lain. Operator ringkas dan berguna untuk edge map, ekstraksi garis bergaya, dan menemukan transisi struktural tanpa commit ke outline threshold penuh.

## Morfologi

### Median

Median mengganti setiap piksel dengan nilai median dari neighborhood-nya, cenderung menghilangkan noise terisolasi sambil mempertahankan batas lebih kuat daripada blur sederhana. Filter cleanup praktis meratakan gangguan visual kecil tanpa langsung melembutkan seluruh gambar.

### Dilate

Dilate menumbuhkan wilayah lebih terang ke luar memakai logika neighborhood shape-aware yang sama. Dalam istilah image-making, dapat menebal tanda terang, memperluas bentuk terang, atau menutup celah gelap kecil.

### Erode

Erode gerakan komplementer: menumbuhkan wilayah lebih gelap dan menarik kembali yang lebih terang. Berguna menipiskan detail terang, memperbesar massa gelap, atau mengencangkan mask dan bentuk grafis.

## Pola

### Checkerboard

Checkerboard menghasilkan pola tile bergantian teratur. Sederhana, namun kesederhanaan itu berguna menguji transparansi, membangun mask, memblokir latar grafis, atau membuat materi sumber geometris bersih.

### Grid

Grid menggambar pembagian horizontal dan vertikal berulang, berguna untuk panduan layout, backdrop desain, ilustrasi teknis, dan masking prosedural. Karena dihasilkan sebagai filter, jarak dan tampilan dapat disetel tanpa membangun pola manual.

### Voronoi

Voronoi menghasilkan tekstur seluler tileable dari titik seeded, dengan kontrol feature type, distance metric, randomness, fractal detail, dan seamless wrapping. Dalam praktik dapat bergerak dari struktur sel retak bersih ke pola batu, kulit, peta, atau jaringan abstrak lebih organik.

### Wave

Wave menghasilkan pola bergaris atau bercincin dibentuk profil waveform, susunan geometris, distorsi, fractal detail, dan phase offset. Lebih dari alat garis sederhana: dapat menghasilkan riak terkontrol, pita topografi, grafis moire-like, atau bidang pola konsentris berisik.

### Halftone (AM)

Halftone (AM) menerapkan dot screen amplitude-modulated klasik, dengan frekuensi, bentuk dot, sharpness, mode warna, dan kontrol sudut CMYK untuk struktur cetak rosette-style. Dibanding FM halftoning, opsi lebih teratur dan mekanis yang dapat dikenali saat look yang diinginkan newsprint, offset lithography, atau geometri screen sengaja terlihat.
