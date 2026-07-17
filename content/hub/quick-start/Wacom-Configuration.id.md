---
title: "Konfigurasi Wacom"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 3af66b116d9f361052280ac9636ae4b23bf5fc30f10f7227fb42d2d9e654ea95
url: "hub/quick-start/Wacom-Configuration"
translation_lock: true
---

Untuk pengecatan digital di Lumi, disarankan **pengaturan tekanan linier** yang sederhana.

- Jaga kurva tekanan driver tablet tetap linier.
- Jaga kurva tekanan/input di Lumi sebagian besar linier.
- Bentuk nuansa dengan kuas itu sendiri, karena dinamika kuas sudah dapat bersifat non-linier.

Kami merekomendasikan mempertahankan kurva tekanan linier default pada tingkat driver OS. Menggabungkan beberapa kurva non-linier sering menyebabkan perilaku input yang tidak dapat diprediksi; dengan menjaga driver tetap netral, penyesuaian apa pun di Lumi-o tetap intuitif dan dapat direproduksi. Penyesuaian ringan pada kurva global Lumi masih masuk akal bila diperlukan.

## Kurva stylus global di Lumi

Di Lumi, buka:

Edit → Preferensi → Perangkat Input → Konfigurasikan Tablet, Stylus, dan Perangkat Lainnya...

Di sini Anda dapat mengatur kurva tekanan global untuk stylus.

## Cincin sentuh Wacom

Lumi kini mendukung input Wacom Touch Ring secara langsung, termasuk input cincin berbasis modifier.

Dalam dialog konfigurasi perangkat yang sama, Anda dapat menetapkan aksi cincin per input, termasuk:

- Ukuran Kuas
- Ukuran Relatif Kuas
- Sudut Kuas
- Sudut Tampilan
- Zoom Tampilan

Catatan: Suatu gambar harus aktif agar Touch Ring dapat memengaruhi atribut. Cincin default-nya adalah perubahan ukuran kuas relatif. Untuk mencegah penyesuaian yang tidak disengaja, sapuan setengah lingkaran diperlukan untuk memicu perintah (misalnya, sapuan setengah searah jarum jam akan menggandakan ukuran kuas).
