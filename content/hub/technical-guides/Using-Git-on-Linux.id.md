---
title: "Menggunakan Git di Linux"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
url: "hub/technical-guides/Using-Git-on-Linux"
translation_lock: true
---

Selamat datang di panduan pemula untuk menggunakan Git di Linux! Panduan ini dirancang untuk membantu Anda memulai dengan Git dan GitLab, serta memberikan pemahaman dasar tentang cara menggunakan alat-alat ini.

## Ikhtisar Git

Kode yang digunakan untuk membuat aplikasi disimpan dalam kumpulan folder dan file di sistem Anda. Git adalah aplikasi yang memungkinkan kita membackup, berbagi, dan menyalin koleksi tersebut. Git dikenal sebagai sistem kontrol versi yang memungkinkan Anda melacak perubahan pada kode dan berkolaborasi dengan orang lain. Ini adalah alat canggih yang banyak digunakan di komunitas sumber terbuka. GitLab adalah platform berbasis web yang memungkinkan Anda menghosting dan mengelola repositori Git secara online, sehingga memudahkan kolaborasi dan pelacakan perubahan kode.

## Apa itu repositori?

_repo_, kependekan dari repositori, adalah folder lokal yang dikelola Git dengan salinan online. Repositori GitLab adalah kumpulan file dan folder yang membentuk sebuah proyek. Repositori dapat memiliki _branch_ — salinan independen dari proyek yang sama. Branch adalah versi terpisah proyek yang memungkinkan Anda melakukan perubahan tanpa mempengaruhi versi utama. Ini berguna untuk menguji fitur baru atau memperbaiki bug tanpa mengganggu proyek utama. Ada repositori lokal Anda, disimpan di hard drive, dan repositori remote, disimpan online melalui Git dan GitLab.

## Menggunakan Git

Anda harus menginstal Git di sistem. Pada sistem berbasis Debian, Anda dapat menggunakan perintah apt untuk menginstal paket perangkat lunak. Dalam hal ini, kami menggunakannya untuk menginstal Git, paket yang menyediakan sistem kontrol versi Git. Perintah sudo memberikan izin kepada installer untuk menginstal di sistem Anda.

```bash
sudo apt install git
```

## Akses GitLab

Sebelum Anda dapat menggunakan [GitLab](https://gitlab.com/users/sign_up), Anda harus membuat akun dengan mengunjungi situs web GitLab dan menyelesaikan proses pendaftaran.

GitLab memerlukan _SSH_ untuk komunikasi aman dan terautentikasi antara klien (misalnya Anda) dan server GitLab saat melakukan operasi Git seperti _cloning_, _pushing_, dan _fetching_ repositori. Cloning adalah membuat salinan lokal repo, fetching adalah membawa perubahan dari repo ke salinan lokal Anda, dan pushing adalah mengirim perubahan ke repositori server. SSH (Secure Shell) adalah protokol jaringan yang memungkinkan akses remote aman dan menggunakan _pasangan kunci_ untuk mengautentikasi dan membuat koneksi aman. Untuk menghasilkan pasangan kunci SSH, Anda dapat menggunakan perintah ssh-keygen di terminal.

```bash
ssh-keygen
```

Tentukan nama file, atau gunakan default dengan menekan Enter, dan kata sandi opsional. Di direktori home Anda, di folder tersembunyi bernama `.ssh`, kini ada dua file id_rsa jika Anda menggunakan nama default. File `.pub` adalah kunci publik dan Anda dapat melihat isinya dengan editor teks.

Masuk ke akun GitLab Anda dan buka pengaturan pengguna. Klik 'SSH Keys' di menu navigasi sebelah kiri. Salin dan tempel kunci publik ke bidang Kunci dan beri judul yang relevan, seperti PC@Home. Klik tombol 'Add Key' untuk menyimpan kunci. Kunci publik SSH Anda kini ditambahkan ke akun GitLab dan dapat digunakan untuk mengautentikasi dengan repositori GitLab. Uji apakah kunci dan koneksi berfungsi dengan perintah `ssh -T` untuk melihat pesan selamat datang dari GitLab.

```bash
ssh -T git@ssh.gitlab.gnome.org
Welcome to GitLab, @username!
```

## Perintah dasar Git

Setelah Anda menginstal Git dan menyiapkan kunci SSH dengan GitLab, mari bahas beberapa perintah Git penting untuk mengelola repositori. Perintah-perintah ini membantu Anda bekerja dengan proyek yang ada, menjaganya tetap mutakhir, dan membuat perubahan dengan aman.

### 1. **Mengkloning repositori**

Cloning adalah proses membuat salinan lokal repositori remote. Ini berguna saat Anda ingin mengerjakan proyek yang sudah ada di GitLab. Untuk mengkloning repositori, gunakan perintah `git clone` diikuti URL repositori:

```sh
git clone https://gitlab.com/username/repository.git
```

Ganti `https://gitlab.com/username/repository.git` dengan URL repositori yang ingin Anda kloning. Perintah ini membuat salinan lokal repositori di direktori baru.

### 2. **Memeriksa status repositori**

Untuk melihat apakah repositori lokal mengalami perubahan atau untuk melihat statusnya saat ini, gunakan:

```sh
git status
```

Perintah ini menunjukkan file mana yang telah diubah, ditambahkan, atau dihapus di salinan repositori lokal.

### 3. **Repositori remote**

Repositori remote adalah versi proyek yang dihosting secara online, seperti di GitLab. Mereka berfungsi sebagai lokasi pusat tempat kode disimpan dan dapat diakses oleh orang lain. Repositori remote default yang dibuat Git saat Anda mengkloning proyek disebut `origin`. Anda dapat menambah, menghapus, atau mendaftar repositori remote menggunakan perintah berikut:

- **Mendaftar remote:**

  Untuk melihat repositori remote mana yang ditautkan ke proyek lokal, gunakan:

  ```sh
  git remote -v
  ```

  Perintah ini mencantumkan semua remote dan URL-nya. Biasanya, Anda akan melihat `origin` tercantum di sini.

- **Menambahkan remote:**

  Jika Anda perlu menambahkan repositori remote baru:

  ```sh
  git remote add <name> <url>
  ```

  Ganti `<name>` dengan nama remote, dan `<url>` dengan URL repositori.

- **Menghapus remote:**

  Untuk menghapus repositori remote:

  ```sh
  git remote remove <name>
  ```

  Ganti `<name>` dengan nama remote yang ingin dihapus.

### 4. **Mengambil perubahan dari repositori remote**

Jika Anda ingin melihat perubahan pada repositori remote tanpa menerapkannya ke salinan lokal, gunakan:

```sh
git fetch origin
```

Perintah ini mengambil perubahan terbaru dari repositori remote tetapi tidak menggabungkannya ke branch lokal. Ini cara memeriksa pembaruan sebelum memutuskan menerapkannya.

### 5. **Mengatur ulang repositori lokal**

Jika Anda ingin mengatur ulang repositori lokal agar sama persis dengan repositori remote, Anda dapat menggunakan reset 'hard'. **Peringatan:** Ini akan menimpa perubahan lokal apa pun.

```sh
git reset --hard origin/branch-name
```

Ganti `branch-name` dengan nama branch yang ingin di-reset. Perintah ini membuang semua perubahan lokal dan membuat repositori lokal identik dengan repositori remote.

### 6. **Melihat riwayat commit**

Untuk melihat daftar perubahan pada repositori dari waktu ke waktu:

```sh
git log
```

Perintah ini menampilkan riwayat commit, termasuk pembuat, tanggal, dan pesan untuk setiap perubahan. Berguna untuk memahami perubahan apa yang dilakukan dan kapan.

### Ringkasan

Perintah dasar Git ini membantu Anda bekerja dengan repositori, menjaga salinan lokal tetap mutakhir, dan mengelola repositori remote dengan aman. Mengkloning repositori, memeriksa status salinan lokal, dan mengelola repositori remote adalah keterampilan utama untuk mengelola proyek menggunakan Git.
