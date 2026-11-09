---
title: "Una visión general del sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux es un sistema operativo potente y versátil, con una amplia comunidad de desarrolladores. En esencia, un sistema Linux consta de varios componentes clave que trabajan juntos para ofrecer una experiencia de usuario fluida. Esta visión general describe las partes esenciales de un sistema Linux: el kernel, la distribución, el gestor de paquetes, el gestor de pantalla, el entorno de escritorio y el servidor gráfico (X11 o Wayland).

Lumi se desarrolla y se prueba en Debian con Cinnamon (X11). Ese sigue siendo el entorno de desarrollo. GNOME en Wayland se ha probado junto a esa configuración, en cuanto al aspecto general de la aplicación y al renderizado OpenGL.

**Valores predeterminados habituales en las distribuciones Linux actuales**

| **Distribución**   | **Gestor de paquetes** | **Gestor de pantalla** | **Entorno de escritorio** | **Servidor gráfico** |
|--------------------|------------------------|------------------------|---------------------------|----------------------|
| Debian             | APT                    | GDM                    | GNOME                     | Wayland              |
| Ubuntu             | APT                    | GDM                    | GNOME                     | Wayland              |
| Debian             | APT                    | GDM                    | Cinnamon                  | X11                  |
| Fedora             | DNF                    | GDM                    | GNOME                     | Wayland              |
| Arch Linux         | Pacman                 | Elección del usuario   | Elección del usuario      | Elección del usuario |

### Términos clave

#### Kernel

El núcleo del sistema operativo que se comunica directamente con el hardware; normalmente, Linux.

#### Distribución

La distribución Linux empaqueta el kernel junto con herramientas, bibliotecas y software del espacio de usuario. Entre los ejemplos están Debian, Arch Linux y Fedora.

#### Gestor de paquetes

Herramienta para instalar, actualizar y eliminar aplicaciones desde repositorios. Entre los ejemplos están APT en distribuciones basadas en Debian, DNF en Fedora y Pacman en Arch Linux.

#### Gestor de pantalla

Gestiona la pantalla de inicio de sesión gráfica y el arranque de la sesión. Entre los ejemplos están GDM (GNOME Display Manager), LightDM y SDDM (Simple Desktop Display Manager).

#### Entorno de escritorio

Proporciona la interfaz gráfica de usuario (GUI) y gestiona la apariencia general y la experiencia de uso. Entre los ejemplos están GNOME, Cinnamon y KDE Plasma.

#### Servidor gráfico

Gestiona la salida de pantalla y los eventos de entrada. Entre los ejemplos están X11 (X Window System) y Wayland. X11 es un servidor gráfico tradicional, mientras que Wayland es una alternativa más reciente y segura.
