---
title: "Tinjauan Sistem Linux"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
url: "hub/install-linux/A-Linux-System-Overview"
translation_lock: true
---

Linux adalah sistem operasi yang andal dan serbaguna dengan komunitas pengembang yang luas. Inti sistem Linux terdiri dari beberapa komponen utama yang bekerja sama untuk memberikan pengalaman pengguna yang lancar. Ikhtisar ini menguraikan bagian-bagian penting dari sistem Linux, termasuk kernel, distribusi, manajer paket, display manager, lingkungan desktop, dan display server (X11 atau Wayland).

Lumi berjalan paling baik di Debian dengan Cinnamon (X11), dan dikembangkan serta diuji di lingkungan tersebut.

**Konfigurasi default distribusi Linux umum saat ini**

| **Distribusi** | **Manajer paket** | **Display manager** | **Lingkungan desktop** | **Display server** |
|----------------|-------------------|---------------------|------------------------|--------------------|
| Debian         | APT               | GDM                 | GNOME                  | Wayland            |
| Ubuntu         | APT               | GDM                 | GNOME                  | Wayland            |
| Debian         | APT               | GDM                 | Cinnamon               | X11                |
| Fedora         | DNF               | GDM                 | GNOME                  | Wayland            |
| Arch Linux     | Pacman            | Pilihan pengguna    | Pilihan pengguna       | Pilihan pengguna   |

### Istilah utama

#### Kernel

Inti sistem operasi yang berinteraksi langsung dengan perangkat keras — biasanya kernel Linux.

#### Distribusi

Distribusi Linux yang mengemas kernel bersama alat user space, pustaka, dan perangkat lunak. Contohnya: Debian, Arch Linux, dan Fedora.

#### Manajer paket

Alat untuk menginstal, memperbarui, dan menghapus aplikasi dari repositori. Contohnya: APT untuk distribusi berbasis Debian, DNF untuk Fedora, dan Pacman untuk Arch Linux.

#### Display manager

Mengelola layar login grafis dan inisiasi sesi. Contohnya: GDM (GNOME Display Manager), LightDM, dan SDDM (Simple Desktop Display Manager).

#### Lingkungan desktop

Menyediakan antarmuka grafis (GUI) dan mengelola tampilan serta pengalaman pengguna secara keseluruhan. Contohnya: GNOME, Cinnamon, dan KDE Plasma.

#### Display server

Mengelola keluaran tampilan dan peristiwa input. Contohnya: X11 (X Window System) dan Wayland. X11 adalah display server tradisional; Wayland adalah alternatif yang lebih baru dan lebih aman.
