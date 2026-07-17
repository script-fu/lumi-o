---
title: "Git"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: c2c03721fbcc205a8c33d945786290712bc60e71beb18b9a1dda1a34d975051f
url: "hub/scripting/tools/git"
translation_lock: true
---
Gunakan Git untuk melacak perubahan pada plug-in Anda, mengembalikan kesalahan, dan berbagi kode ke seluruh mesin.

## Mengapa Mengatur Kode Anda?

Setelah Anda memiliki lebih dari satu skrip, struktur folder yang konsisten menghemat waktu dan memudahkan kontrol versi.

## Menyiapkan Struktur Folder Kode

Salah satu cara paling sederhana untuk mengatur proyek Anda adalah dengan membuat **folder kode** khusus di mesin lokal Anda. Di dalam folder ini, Anda dapat membuat subfolder untuk setiap proyek atau repositori. Berikut struktur folder yang direkomendasikan:

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Setiap sub-folder (misalnya, `project1`) mewakili **repositori**, yang merupakan tempat Anda menyimpan file dan kode untuk proyek tersebut.

## Apa itu Repositori?

**Repositori** (atau **repo**) pada dasarnya adalah folder dengan konten yang dilacak Git. Saat Anda membuat repo secara lokal, Anda menginisialisasi Git di dalam folder tersebut, sehingga Anda dapat menyimpan perubahan apa pun ke klon online.

### Repositori Lokal dan Remote

- **Repo Lokal**: Ini adalah repositori yang disimpan di komputer Anda, di salah satu folder proyek Anda.
- **Remote Repo**: Versi repositori yang disimpan online (misalnya, di GitLab atau GitHub).

## Menggunakan Git dan GitHub

Setelah struktur folder Anda siap, Anda dapat menginisialisasi Git dan menghubungkan proyek lokal Anda ke GitHub. Ikuti langkah-langkah berikut untuk memulai:

### Langkah Dasar Menggunakan Git dan GitHub

1.**Instal Git**
2. **Buat Akun GitHub**
3. **Buat Repositori Kosong di GitHub**
4. **Inisialisasi Git di Proyek Lokal Anda**
5. **Hubungkan Repo Lokal Anda ke GitHub**
6. **Panggungkan File Anda**
7. **Komit Perubahan Anda**
8. **Dorong Perubahan Anda ke GitHub**
9. **Lihat Repositori Anda Secara Online**

### 1. Instal Git

Jika Anda belum menginstal Git, Anda dapat melakukannya di Linux menggunakan:

```sh
sudo apt install git
```

### 2. Buat Akun GitHub

Jika Anda belum memiliki akun, kunjungi [GitHub](https://github.com/) untuk mendaftar. Setelah terdaftar, Anda dapat membuat repositori di GitHub untuk menyimpan kode Anda secara online.

### 3. Buat Repositori Kosong di GitHub

1. **Masuk ke GitHub**: Buka [GitHub](https://github.com/) dan masuk ke akun Anda.
2. **Buat Repositori Baru**:
   - Klik ikon **** di pojok kanan atas dan pilih **Repositori baru**.
   - Masukkan nama repositori (misalnya `your-repository`).
   - Tambahkan deskripsi jika diinginkan.
   - Pilih visibilitas **Publik** atau **Pribadi**.
   - **Jangan** menginisialisasi repositori dengan README, `.gitignore`, atau lisensi (untuk menghindari konflik).
   - Klik **Buat repositori**.

### 4. Inisialisasi Git di Proyek Lokal Anda

Untuk mulai melacak folder proyek dengan Git, buka terminal Anda, navigasikan ke folder proyek, dan jalankan:

```sh
cd code/your/project/folder
git init
```

Perintah ini menginisialisasi repositori Git kosong di folder proyek Anda.

### 5. Hubungkan Repo Lokal Anda ke GitHub

Selanjutnya, Anda ingin menghubungkan repositori lokal Anda ke GitHub. Setelah membuat repositori kosong di GitHub, tambahkan repositori tersebut sebagai remote ke proyek lokal Anda:

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Ganti `your-username` dan `your-repository` dengan nama pengguna GitHub Anda yang sebenarnya dan nama repositori. Perintah ini menghubungkan proyek lokal Anda dengan repositori jarak jauh di GitHub.

### 6. Panggung File Anda

Sebelum Anda dapat menyimpan perubahan Anda di Git, Anda perlu memberi tahu Git file mana yang telah Anda ubah dan ingin Anda simpan. Ini disebut "pementasan" file Anda. Gunakan perintah berikut untuk menampilkan semua file yang dimodifikasi atau baru:

```sh
git add .
```Ini memberitahu Git untuk melacak perubahan yang Anda buat pada semua file di proyek Anda. Anda juga dapat menampilkan file tertentu dengan mengganti `.` dengan nama file.

### 7. Komit Perubahan Anda

Setelah melakukan staging, langkah berikutnya adalah menyimpan (atau "mengkomit") perubahan pada repositori Git lokal Anda. Saat melakukan, Anda harus selalu menyertakan pesan yang menjelaskan perubahan apa yang telah Anda buat. Misalnya:

```sh
git commit -m "Add new feature"
```

Bendera `-m` memungkinkan Anda menulis pesan yang merangkum perubahan yang Anda buat. Pesan ini membantu Anda dan orang lain memahami apa yang diubah dalam penerapan ini.

### 8. Dorong Perubahan Anda ke GitHub

Setelah Anda melakukan perubahan secara lokal, kini Anda dapat "mendorong" perubahan tersebut ke GitHub sehingga repositori jarak jauh Anda diperbarui. Jalankan perintah berikut untuk mengunggah perubahan Anda:

```sh
git push -u origin main
```

Cabang `main` adalah cabang default di GitHub tempat kode disimpan, dan perintah ini mengunggah perubahan lokal Anda ke repositori jarak jauh, sehingga dapat diakses secara online.

### 9. Lihat Kode Anda di GitHub

Setelah Anda memasukkan kode ke GitHub, Anda dapat melihat repositori Anda di antarmuka web GitHub. Anda akan melihat file dari repo lokal Anda, bersama dengan riwayat penerapan yang menunjukkan perubahan yang Anda buat.

## Kesimpulan

Dengan mengatur kode Anda ke dalam folder khusus dan menggunakan GitHub untuk mengelola dan mencadangkan repositori Anda, proyek Anda akan tetap terstruktur dengan baik dan mudah diakses. Setelah Anda memiliki versi kode yang berfungsi, kirimkan ke GitHub. Anda kemudian dapat dengan mudah melacak perubahan apa pun menggunakan antarmuka web GitHub atau Visual Studio Code, yang menyoroti baris yang dimodifikasi. Pendekatan ini memungkinkan Anda untuk terus menyempurnakan dan memperluas kode Anda tanpa kehilangan jejak kemajuan atau perubahan.

Git dan platform seperti GitHub dan GitLab adalah alat yang ampuh, dan meskipun rumit, ada banyak sumber daya yang tersedia online untuk membantu Anda memahaminya dengan lebih baik. Salah satu sumber daya paling berharga yang saya temukan adalah pembantu AI seperti ChatGPT. Anda dapat menjelaskan apa yang perlu Anda capai, dan alat ini akan dengan sabar memandu Anda melalui proses langkah demi langkah.

## Glosarium

Berikut beberapa istilah umum yang akan Anda temui saat bekerja dengan Git dan GitHub:- **Commit**: Cuplikan perubahan Anda di repositori. Setiap penerapan menyertakan pesan yang menjelaskan apa yang diubah dan membuat catatan sejarah yang dapat Anda rujuk atau kembalikan nanti.
- **Repositori (Repo)**: Kumpulan file dan riwayatnya yang dilacak oleh Git. Repositori bisa ada secara lokal di komputer Anda atau dari jarak jauh di platform seperti GitHub. Setiap proyek biasanya disimpan dalam repositorinya sendiri.
- **Remote**: Repositori jarak jauh adalah versi proyek Anda yang dihosting di platform seperti GitHub. Versi lokal proyek Anda di komputer Anda ditautkan ke remote ini sehingga Anda dapat mengunggah (mendorong) dan mengunduh (menarik) perubahan.
- **Staging**: Proses menyiapkan file untuk penerapan. Saat Anda menampilkan file, Anda memberi tahu Git bahwa Anda ingin memasukkannya ke dalam commit berikutnya. Staging memungkinkan Anda memilih perubahan mana yang akan disertakan dalam penerapan.
- **Push**: Tindakan mengirimkan perubahan yang Anda lakukan dari repositori lokal ke repositori jarak jauh (misalnya, GitHub), sehingga orang lain dapat mengakses versi terbaru kode Anda.
- **Tarik**: Tindakan mengambil perubahan dari repositori jarak jauh untuk memperbarui salinan lokal Anda. Anda menarik perubahan saat Anda ingin menyinkronkan repositori lokal Anda dengan versi terbaru dari jarak jauh.
- **Origin**: Nama default untuk repositori jarak jauh saat Anda pertama kali menghubungkan repositori lokal Anda ke remote. Biasanya mengacu pada URL utama proyek Anda di GitHub.