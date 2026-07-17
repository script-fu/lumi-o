---
title: "Instalación de Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 42b2f95f8ff71be8eff6777dfd9808855f99f943c55575079566588a08fd8fcd
---

Este documento describe el proceso utilizado para instalar Debian Stable como sistema operativo de desarrollo de Lumi-o. Puede resultar útil para quienes configuren un entorno similar.

Se eligió Debian Stable porque Lumi está pensado para compilarse de forma fiable sobre una plataforma predecible a largo plazo. El desarrollo de GIMP apunta a Debian Testing, lo que convierte a Debian Stable en una base muy alineada.

Lumi rinde al máximo en Debian con Cinnamon (X11) y se desarrolla y prueba en ese entorno. Cinnamon ofrece un flujo de trabajo de escritorio familiar, similar al de Windows, mientras que X11 ha proporcionado el entorno más estable para el desarrollo de Lumi.

Si vienes de Windows, el principal cambio conceptual es que la mayor parte de la instalación y configuración del software se realiza mediante gestores de paquetes y comandos sencillos de terminal, en lugar de instaladores descargables.

## ¿Para quién es esta guía?

Esta guía documenta una configuración funcional de Debian Stable utilizada para el desarrollo de Lumi. No es un tutorial general de instalación de Linux.

Es especialmente útil para:

- artistas que pasan de Windows y desean una configuración de Linux predecible
- desarrolladores que compilan Lumi desde el código fuente
- usuarios que prefieren reproducir un entorno de trabajo conocido en lugar de diseñar su propia configuración del sistema

Se asume familiaridad básica con el particionado de discos y el uso sencillo de la línea de comandos.

## Haz una copia de seguridad de tus datos

Antes de instalar Debian, crea una copia de seguridad completa de tu directorio personal en una unidad externa. Incluye cualquier carpeta de datos adicional que quieras conservar.

Nota: en Linux, `~` representa tu directorio personal.

Si utilizas repositorios Git, sube los cambios importantes a sus orígenes para poder restaurarlos fácilmente después de la instalación. Este paso solo es relevante si ya usas Git.

## Crea una partición

Reserva espacio en tu disco principal para Debian. Existen muchas guías y herramientas para este paso, incluido GParted. Según tu configuración, puedes:

- reducir una partición de Windows existente para un arranque dual
- reutilizar una partición de Linux existente
- preparar particiones nuevas de Linux y de intercambio (swap)

Si tienes dudas, consulta guías específicas de tu hardware antes de hacer cambios, ya que los pasos de particionado varían mucho entre sistemas.


## Crea un USB de instalación de Debian

Suponiendo que ya existen una partición de destino y un espacio de intercambio:

1. Descarga la ISO de Debian desde el sitio web oficial: https://www.debian.org/
2. En Windows, utiliza BalenaEtcher para grabar la ISO en una unidad USB.
3. En Linux, utiliza una herramienta de línea de comandos como `dd` para crear un USB de arranque.

## Instala Debian

1. Inserta la unidad USB.
2. Reinicia y pulsa la tecla del menú de arranque (habitualmente `F2`, `F12`, `Esc` o `Del`) durante el inicio.
3. Selecciona el dispositivo USB.
4. Elige un instalador no gráfico.
5. Deja la contraseña de root en blanco cuando se te solicite, para que el instalador conceda acceso sudo a tu cuenta de usuario.
6. Particiona manualmente:

   - Sistema de archivos: ext4 (con journaling)
   - Swap: partición de intercambio existente
   - Punto de montaje: `/`
   - Etiqueta: `linux`
   - Nombre de host: nombre del sistema que se muestra como `user@hostname`
   - Cuenta de usuario: tu nombre completo
   - Nombre de usuario: nombre de inicio de sesión en la terminal

7. El instalador de Debian ofrece una opción de entorno de escritorio en esta etapa; selecciona **Cinnamon** para la configuración recomendada por Lumi.
8. Completa la instalación y reinicia en Debian Stable.

## Configuración del sistema

### Escala de la pantalla

Debian Stable gestiona el escalado fraccional de forma inconsistente, especialmente en pantallas 4K. En lugar de reducir la resolución de la pantalla, ajusta los elementos de la interfaz directamente.

Ajustes recomendados:

- Evita el escalado fraccional de la pantalla.
- Menú → Selección de fuente → Configuración de fuente → Factor de escala de texto: `2.5`
- Fuente de escritorio: `14`
- Panel → Personalizar → Altura del panel: `60`
- Apariencia del panel → Tamaño del icono simbólico de la zona derecha: `48px`
- Ratón y panel táctil → Ajuste del tamaño del puntero
- Escritorio (clic derecho) → Personalizar → Tamaño de icono más grande

Ajuste de Firefox:

- Barra de direcciones → `about:config`
- Establece `layout.css.devPixelsPerPx` en `1`

### Terminal

Configura las preferencias de la terminal:

1. Menú → Terminal → Editar → Preferencias
2. Texto → Tamaño inicial: `140 columns`, `40 rows`
3. Texto → Fuente personalizada: `Monospace 10`
4. Colores → Esquemas integrados → Solarized Dark

## Restaura los datos

Restaura los archivos de la copia de seguridad en el directorio personal según sea necesario, por ejemplo:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: las carpetas que comienzan con `.` son directorios de configuración ocultos en Linux.

## Opcional: configuración de Git

Solo es necesario si planeas compilar Lumi o restaurar repositorios.

### Instala Git

```bash
sudo apt install git
```

Configura tu identidad:

```bash
git config --global --edit
```

#### Acceso a GitLab

Restaura el acceso a los repositorios en GitLab o GitHub:

1. Cambia los permisos del archivo de clave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Añade la clave a la nueva instalación de Git: `ssh-add ~/.ssh/id_rsa`
3. Prueba la conexión: `ssh -T git@ssh.gitlab.gnome.org` o `ssh -T git@github.com`

Para cada repositorio, obtén los orígenes y restablece la rama local para que coincida:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Ejecuta `git status` para confirmar que los repositorios están limpios.

Ya tienes un nuevo sistema operativo con los datos y repositorios restaurados. Esta configuración refleja un entorno de trabajo conocido utilizado para el desarrollo de Lumi y puede adaptarse a flujos de trabajo individuales según sea necesario.

## Compila Lumi después de configurar el sistema operativo

Los scripts de compilación de Lumi se encuentran en:

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```
