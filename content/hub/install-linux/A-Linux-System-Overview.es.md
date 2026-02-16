---
title: "Una descripción general del sistema Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux es un sistema operativo potente y versátil con una vasta comunidad de desarrolladores. En esencia, un sistema Linux consta de varios componentes clave que trabajan juntos para brindar una experiencia de usuario perfecta. Esta descripción general describirá las partes esenciales de un sistema Linux, incluido el kernel, la distribución, el administrador de paquetes, el administrador de pantalla, el entorno de escritorio y el servidor de pantalla (X11 o Wayland).

**Ejemplos de configuraciones de distribución de Linux**

| **Distribución** | **Administrador de paquetes** | **Administrador de visualización** | **Entorno de escritorio** | **Servidor de visualización** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian | APTO | DMG | GNOMO | X11 |
| Debian | APTO | LuzDM | canela | X11 |
| Pruebas de Debian | APTO | DMG | GNOMO | Wayland |
| Sombrero de fieltro | No abandonar | DMG | GNOMO | Wayland |
| Arco Linux | Pacman | SDDM | Plasma KDE | X11 |

### Términos clave

#### Núcleo
El núcleo del sistema operativo que interactúa directamente con el hardware, generalmente Linux.

#### Distribución
La distribución de Linux, que empaqueta el kernel junto con herramientas, bibliotecas y software del espacio de usuario. Los ejemplos incluyen Debian, Arch Linux y Fedora.

#### Administrador de paquetes
Una herramienta utilizada para instalar, actualizar y eliminar aplicaciones de software de los repositorios. Los ejemplos incluyen APT para distribuciones basadas en Debian, DNF para Fedora y Pacman para Arch Linux.

#### Administrador de visualización
Gestiona la pantalla gráfica de inicio de sesión y el inicio de sesión. Los ejemplos incluyen GDM (GNOME Display Manager), LightDM y SDDM (Simple Desktop Display Manager).

#### Entorno de escritorio
Proporciona la interfaz gráfica de usuario (GUI) y gestiona la apariencia general y la experiencia del usuario. Los ejemplos incluyen GNOME, Cinnamon y KDE Plasma.

#### Servidor de visualización
Gestiona los eventos de entrada y salida de la pantalla. Los ejemplos incluyen X11 (Sistema X Window) y Wayland. X11 es un servidor de visualización tradicional, mientras que Wayland es una alternativa más nueva y segura.