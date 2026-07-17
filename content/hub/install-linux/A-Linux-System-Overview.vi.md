---
title: "Tổng quan về hệ thống Linux"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
url: "hub/install-linux/A-Linux-System-Overview"
translation_lock: true
---
Linux là một hệ điều hành mạnh mẽ và linh hoạt với cộng đồng các nhà phát triển rộng lớn. Về cốt lõi, một hệ thống Linux bao gồm một số thành phần chính phối hợp với nhau để mang lại trải nghiệm liền mạch cho người dùng. Tổng quan này sẽ phác thảo các phần thiết yếu của hệ thống Linux, bao gồm kernel, bản phân phối, trình quản lý gói, trình quản lý hiển thị, môi trường máy tính để bàn và máy chủ hiển thị (X11 hoặc Wayland).

Lumi hoạt động tốt nhất trên Debian với Cinnamon (X11) và được phát triển và thử nghiệm trong môi trường đó.

**Cấu hình mặc định phổ biến của các bản phân phối Linux**

| **Phân phối** | **Trình quản lý gói** | **Trình quản lý hiển thị** | **Môi trường máy tính để bàn** | **Máy chủ hiển thị** |
|-------------------|----------------------|----------------------|----------------------|-------------------|
| Debian | APT | GDM | GNOME | Wayland |
| Ubuntu | APT | GDM | GNOME | Wayland |
| Debian | APT | GDM | Cinnamon | X11 |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arch Linux | Pacman | Lựa chọn của người dùng | Lựa chọn của người dùng | Lựa chọn của người dùng |

### Điều khoản chính

#### Hạt nhân

Lõi của hệ điều hành giao tiếp trực tiếp với phần cứng — thường là Linux.

#### Phân phối

Bản phân phối Linux đóng gói hạt nhân cùng với các công cụ, thư viện và phần mềm trong không gian người dùng. Ví dụ bao gồm Debian, Arch Linux và Fedora.

#### Trình quản lý gói

Một công cụ được sử dụng để cài đặt, cập nhật và xóa các ứng dụng phần mềm khỏi kho lưu trữ. Các ví dụ bao gồm APT cho các bản phân phối dựa trên Debian, DNF cho Fedora và Pacman cho Arch Linux.

#### Trình quản lý hiển thị

Quản lý màn hình đăng nhập đồ họa và bắt đầu phiên. Các ví dụ bao gồm GDM (Trình quản lý hiển thị Gnome), LightDM và SDDM (Trình quản lý hiển thị màn hình đơn giản).

#### Môi trường máy tính để bàn

Cung cấp giao diện đồ họa người dùng (GUI) và quản lý giao diện tổng thể cũng như trải nghiệm người dùng. Các ví dụ bao gồm GNOME, Cinnamon và KDE Plasma.

#### Máy chủ hiển thị

Quản lý các sự kiện đầu ra và đầu vào hiển thị. Ví dụ bao gồm X11 (Hệ thống X Window) và Wayland. X11 là máy chủ hiển thị truyền thống, trong khi Wayland là máy chủ thay thế mới hơn, an toàn hơn.