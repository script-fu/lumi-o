---
title: "Pencadangan Sistem Menggunakan Clonezilla"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
url: "hub/install-linux/System-Backup-Clonezilla"
translation_lock: true
---

Membackup file penting agar dapat kembali ke versi sebelumnya atau mengganti data yang rusak adalah hal yang umum. Namun, jenis pencadangan penting lainnya adalah **kloning disk** — cadangan lengkap seluruh status sistem Anda.

Setelah sistem Anda siap dan berfungsi dengan baik, membuat cadangan lengkap sangat penting untuk memulihkan lingkungan Anda jika terjadi bencana. Pencadangan ini melengkapi penyimpanan rutin data kerja Anda.

[Clonezilla](https://clonezilla.org/) adalah perangkat lunak pencitraan dan kloning disk sumber terbuka yang gratis. Perangkat lunak ini memungkinkan pengguna membuat dan memulihkan cadangan penuh hard drive komputer mereka, sehingga populer di kalangan profesional TI dan pengguna rumahan.

Lebih baik memiliki cadangan dan tidak memerlukannya daripada memerlukan cadangan namun tidak memilikinya.


## Fitur utama Clonezilla

- **Pencitraan disk**: Clonezilla membuat salinan persis hard drive, termasuk sistem operasi, aplikasi, dan data.
- **Pencadangan dan pemulihan**: Memungkinkan Anda membuat image cadangan hard drive dan memulihkannya jika terjadi kegagalan atau migrasi ke drive baru.
- **Gratis dan sumber terbuka**: Clonezilla sepenuhnya gratis, dan kode sumbernya tersedia untuk modifikasi dan penyesuaian.


## Menggunakan Clonezilla untuk mencadangkan

### Langkah persiapan

Anda memerlukan drive USB untuk Clonezilla dan hard drive eksternal yang lebih besar dari drive internal yang ingin Anda kloning.

Langkah-langkah ini merangkum proses berdasarkan [panduan resmi](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Sebaiknya tinjau panduan lengkapnya, yang mencakup tangkapan layar untuk menambah kejelasan.

1. **Buat Clonezilla Live USB atau CD/DVD**: Ikuti petunjuk rinci di [situs web Clonezilla](https://clonezilla.org/liveusb.php) untuk membuat USB atau CD/DVD yang dapat di-boot.

2. **Hubungkan drive cadangan eksternal**: Hubungkan drive eksternal Anda dan pastikan drive tersebut dikenali oleh sistem. Ini akan menjadi tujuan pencadangan Anda.

3. **Verifikasi tata letak partisi**: Gunakan perintah `lsblk` di terminal untuk memverifikasi tata letak partisi hard drive utama Anda. Catat nama perangkat utama.

4. **Boot dari Clonezilla Live USB**: Nyalakan ulang komputer Anda dan boot dari media Clonezilla yang Anda buat. Anda mungkin perlu mengakses pengaturan BIOS/UEFI (biasanya dengan menekan F2, F12, ESC, atau DEL saat boot) dan menyesuaikan urutan boot agar drive USB diprioritaskan.



### Cadangkan dengan Clonezilla

1. **Pilih mode cadangan**: Setelah Clonezilla boot, pilih mode device-device. Mode ini memungkinkan Anda mengkloning drive internal secara langsung ke perangkat eksternal.

2. **Pilih perangkat sumber**: Pilih drive internal utama.

3. **Pilih perangkat target**: Pilih drive cadangan eksternal sebagai perangkat target. Berhati-hatilah saat memilih perangkat agar tidak menimpa data penting. Pastikan drive target berukuran sama atau lebih besar dari drive sumber.

4. **Mulai proses pencadangan**: Clonezilla akan memulai proses pencadangan. Tergantung pada ukuran partisi dan kecepatan drive, proses ini dapat memakan waktu mulai dari beberapa menit hingga beberapa jam.

5. **Beri label cadangan**: Setelah pencadangan selesai, beri label drive USB dan hard drive eksternal dengan tanggal dan sistem yang dicadangkan. Simpan di tempat yang aman.

---

### Memulihkan dari cadangan

Jika Anda perlu memulihkan sistem Debian dari cadangan, ikuti langkah-langkah berikut:

1. **Boot dari media Clonezilla**: Masukkan USB Clonezilla dan boot darinya, ikuti langkah yang sama seperti saat proses pencadangan.

2. **Pilih mode pemulihan**: Pilih mode device-device lagi, namun kali ini Anda akan memulihkan dari image cadangan. Ini akan menyalin semua data dari drive eksternal kembali ke drive internal.

3. **Pilih perangkat sumber**: Pilih drive eksternal tempat cadangan disimpan.

4. **Pilih perangkat target**: Pilih drive internal tempat Anda ingin memulihkan cadangan.

5. **Mulai proses pemulihan**: Clonezilla akan memulai proses pemulihan. Seperti halnya pencadangan, waktu yang diperlukan bergantung pada ukuran drive dan kecepatan perangkat keras.

---

## Catatan akhir

Pencadangan disk dengan Clonezilla memastikan seluruh sistem Anda — sistem operasi, pengaturan, dan aplikasi — terlindungi. Dengan sedikit usaha, Anda dapat melindungi sistem dari kegagalan besar dan meminimalkan waktu henti jika terjadi kerusakan.

Ingat, **cadangan itu penting**. Perbarui cadangan secara rutin dan uji cadangan secara berkala untuk memastikan Anda dapat memulihkan sistem bila diperlukan.

Setelah boot, Anda dapat menyambungkan drive cadangan eksternal dan memeriksa struktur partisinya menggunakan utilitas Disk di Linux. Drive cadangan harus mencerminkan struktur drive internal, dengan partisi yang sama dan ruang tidak terpakai jika drive eksternal lebih besar.
