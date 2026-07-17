---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1bf50df22bdb2af7931727f82bc3c90eee5242be66847535aec8e41c47087e53
---
Lumi adalah aplikasi cepat dan efisien khusus Linux untuk membuat gambar raster, dikembangkan secara terbuka. Keputusan desain, dokumen arsitektur, dan riwayat pengembangannya publik agar pengguna dapat memahami evolusinya.

Lumi condong ke lukisan dan ilustrasi digital, sambil tetap mampu melakukan pengeditan gambar terstruktur dan koreksi fotografi. Penyesuaian korektif, efek gaya lensa, gradasi warna berbasis nada, dan lapisan filter non-destruktif mendukung beragam alur kerja editing.

Lumi dibangun dengan prinsip bahwa perangkat lunak lukisan digital harus berperilaku seperti alat studio yang andal: dapat diprediksi, transparan, dan fokus pada proses membuat gambar.

## Tujuan

Lumi mendukung metode pembuatan gambar yang terstruktur dan non-destruktif, baik sumbernya dilukis, digambar, maupun fotografi. Ini alternatif yang terfokus dan berpendirian tegas dibanding editor gambar umum maupun perangkat lunak lukisan khusus: gratis dan open source, tanpa langganan, lock-in, ketergantungan cloud, atau pembuatan gambar AI.

Keandalan dan akses jangka panjang adalah fitur inti. Format file terbuka berbasis direktori tetap dapat dibaca tanpa perangkat lunak proprietari, dengan impor dan ekspor XCF serta PSD.

## Landasan artistik

Lumi dikembangkan oleh seniman independen dengan pengalaman di pixel art, gambar dan lukisan tradisional, pengembangan game, technical art, ilustrasi, dan animasi 3D. Latar belakang itu membentuk pendekatannya terhadap warna, garis, lapisan, performa, pemulihan data, scripting, dan pengalaman pengguna.

## Filosofi

Lumi menggabungkan sistem warna berbasis pigmen dengan alur kerja berbasis lapisan yang responsif dan non-destruktif. Sistem warnanya sengaja berbeda dari slider HSV konvensional dan pemilih RGB sembaran.

- **Warna berpusat pada pigmen**: Profil pigmen dunia nyata (kode Colour Index) dicampur secara spektral sehingga palet berperilaku lebih seperti cat sungguhan.
- **Alur kerja berbasis palet**: Palet tersimpan yang dapat diganti mengatur pigmen, campuran, pita nilai, dan gradien, menjaga keputusan warna tetap koheren dalam satu lukisan atau proyek.
- **Alat taktil dan terfokus**: Kuas mengintegrasikan tekanan, kemiringan, dan kecepatan stylus untuk kontrol langsung yang nuans; kontrol mendukung keputusan yang disengaja tanpa kerumitan berlebihan.
- **Keandalan non-destruktif**: Lapisan dan filter yang dapat diedit skala ke proyek kompleks sambil tetap prediktif. Autosave, penyimpanan cepat, penyimpanan inkremental, dan recovery melindungi sesi melukis panjang dan proyek besar.
- **Ruang kerja langsung**: Profil bernama menyimpan dock, alat, preset, palet, dan binding perangkat, lalu mengalihkannya secara atomik saat runtime.
- **Scripting Scheme**: Lumi melanjutkan tradisi Script-Fu dengan bahasa plug-in berbasis Scheme dan fungsi utilitas tambahan untuk membangun plug-in dan mengotomatisasi alur kerja.

[Filter](/hub/features/filters/), termasuk lens blur bokeh berbentuk, tilt shift, gradasi tonal, sharpening, dan noise reduction, dapat tetap dapat diedit bersama pekerjaan kuas.

## Batasan

- **Terfokus, bukan ekshaustif**: Lumi tidak ditujukan untuk desain web, desktop publishing, atau setiap niche yang coba dijangkau editor luas seperti GIMP.
- **Khusus Linux**: Lumi dioptimalkan khusus untuk Linux dan tidak mendukung Windows atau macOS.

## Ucapan terima kasih

Lumi dibangun di atas GNU Image Manipulation Program (GIMP). Lumi mengakui dan sangat berterima kasih atas bertahun-tahun kerja para pengembang, seniman, dan kontributor.

![Lumi logo placeholder](/images/lumi.png)
