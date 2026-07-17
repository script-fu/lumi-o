---
title: "Manajemen Warna"
type: docs
weight: 15
translation_provenance: ai-reviewed
translation_source_sha256: 60e00f1b5e0b4a7bb3034ca99dd3f8f51f6bc52b1629a9ab717d2ac2166393ee
translation_lock: true
url: "hub/technical-guides/Color-Management"
---

Lumi-o dikonfigurasi agar langsung siap pakai. Selama Anda mengerjakan gambar dengan **presisi 16-bit atau lebih tinggi**, perangkat lunak sudah disetel untuk menggunakan paket soft-proofing (CMYK) bawaan dan profil sRGB internal; semuanya seharusnya berfungsi tanpa konfigurasi apa pun.

Bagi mereka yang membutuhkan kontrol lebih dalam, panduan ini menjelaskan model manajemen warna inti Lumi, perbedaan antara profil gambar dan profil soft-proof, lokasi kontrol, dan bagaimana profil default digabungkan dengan aplikasi.

## Ringkasan singkat

Lumi menggunakan tiga peran profil berbeda:

1. **Profil kerja gambar**
   - Mendefinisikan arti angka RGB atau skala abu-abu pada gambar.
   - Digunakan untuk operasi assign/convert.
   - Contoh umum: sRGB bawaan, Adobe RGB.

2. **Profil tampilan**
   - Menjelaskan monitor Anda.
   - Digunakan untuk menampilkan gambar dengan benar di layar.
   - Biasanya disediakan oleh sistem atau dipilih di Preferensi.

3. **Profil soft-proof**
   - Mensimulasikan perangkat keluaran lain atau kondisi pencetakan.
   - **Tidak** mendefinisikan ulang nilai piksel gambar.
   - Contoh umum: profil pers CMYK seperti `CoatedFOGRA39`.

## Profil gambar vs profil soft-proof

### Profil gambar

Gunakan ini saat Anda ingin memberi tahu Lumi ruang warna apa yang sebenarnya ada pada gambar.

Dua operasi umum:

- **Assign profil**
  - Mengubah label profil yang dilampirkan pada gambar.
  - **Tidak** mengonversi nilai piksel.
  - Gunakan hanya jika angka piksel sudah ada di ruang profil tersebut.

- **Convert ke profil**
  - Mengonversi nilai piksel dari profil gambar saat ini ke profil baru.
  - Gunakan saat Anda ingin gambar benar-benar berpindah ke ruang kerja yang berbeda.

**Lokasi menu:**
- Gambar > Manajemen Warna > Assign Color Profile...
- Gambar > Manajemen Warna > Convert to Color Profile...

### Profil soft-proof

Gunakan ini saat Anda ingin melihat pratinjau bagaimana gambar akan direproduksi pada perangkat target atau kondisi pencetakan.

Soft-proofing:
- membiarkan ruang kerja gambar tetap utuh
- mengubah pipeline pratinjau
- dapat menandai warna di luar gamut
- dimaksudkan untuk pratinjau, bukan penugasan ulang data gambar

**Lokasi menu:**
- Gambar > Manajemen Warna > Soft-Proof Settings > Choose Soft-Proof Profile...
- Gambar > Manajemen Warna > Soft-Proof Settings > Rendering Intent
- Gambar > Manajemen Warna > Soft-Proof Settings > Black Point Compensation
- Lihat > Manajemen Warna > Enable Soft-Proof Preview
- Lihat > Manajemen Warna > Mark Out of Gamut Colors

## Cara melihat pratinjau soft-proof

Ada dua titik masuk utama untuk mengaktifkan soft-proof.

### 1. Menu Lihat

Gunakan:
- Lihat > Manajemen Warna > Enable Soft-Proof Preview

Ini mengaktifkan atau menonaktifkan simulasi pratinjau untuk tampilan saat ini.

### 2. Tombol bilah status

Lumi juga menampilkan soft-proofing langsung di bilah status bawah.

- **Klik kiri** (toggle): mengaktifkan atau menonaktifkan warna proof
- **Klik kanan**: buka popover soft-proofing tempat Anda dapat menyesuaikan:
  - profil saat ini
  - pemilih profil
  - rendering intent
  - black point compensation
  - penandaan di luar gamut

{{< callout type="warning" >}}
**Catatan penting tentang presisi**
Pratinjau soft-proof hanya diaktifkan untuk gambar **16-bit dan 32-bit**.
Untuk gambar **8-bit**, tombol toggle dinonaktifkan dan Lumi akan meminta Anda mengonversi presisi ke kedalaman yang lebih tinggi terlebih dahulu sebelum mempratinjau warna secara akurat.
{{< /callout >}}

## Preferensi dan default

Default global ada di:
- Edit > Preferensi > Manajemen Warna

Bagian yang relevan:
- **Manual Monitor Profile**
- **Preferred RGB profile**
- **Preferred grayscale profile**
- **Soft-Proofing**

### Default Lumi saat ini

#### Ruang kerja

ICC ruang kerja bawaan saat ini ditawarkan dari folder data bersama:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Untuk pekerjaan sRGB standar, Lumi juga menyediakan **profil kerja sRGB bawaan secara internal**.

#### Default soft-proof

Paket profil soft-proof bawaan yang saat ini terpasang:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Jika tersedia, `CoatedFOGRA39.icc` digunakan sebagai profil referensi soft-proof/CMYK bawaan secara default.

## Alur kerja praktis

### Untuk pengecatan dan pekerjaan layar normal

- Simpan gambar dalam sRGB internal atau ruang kerja RGB lain yang valid.
- Biarkan Lumi menggunakan profil monitor sistem jika tersedia.

### Untuk pratinjau cetak

- Simpan gambar di ruang kerja RGB standarnya.
- Pilih profil soft-proof yang sesuai dengan kondisi pencetakan target (misalnya FOGRA39).
- Aktifkan pratinjau soft-proof.
- Secara opsional aktifkan peringatan gamut untuk melihat rendering intent yang terpotong.
