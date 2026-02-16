---
title: "Instalación de Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Este documento describe el proceso utilizado para instalar Debian Stable como sistema operativo de desarrollo Lumi·o. Puede resultar útil para otros que establezcan un entorno similar.

Se seleccionó Debian Stable porque Lumi pretende construir de manera confiable sobre una plataforma predecible a largo plazo. El desarrollo de GIMP apunta a Debian Testing, lo que convierte a Debian Stable en un sistema base estrechamente alineado.

Si viene de Windows, el principal cambio conceptual es que la mayor parte de la instalación y configuración del software se realiza a través de administradores de paquetes y comandos simples de terminal en lugar de instaladores descargables.

## ¿Para quién es esta guía?

Esta guía documenta una configuración funcional de Debian Stable utilizada para el desarrollo de Lumi. No es un tutorial general de instalación de Linux.

Es más útil para:

- artistas que pasan de Windows y desean una configuración de Linux predecible
- desarrolladores que construyen Lumi desde la fuente
- usuarios que prefieren reproducir un entorno de trabajo conocido en lugar de diseñar su propia configuración del sistema

Se asume una familiaridad básica con la partición de discos y el uso simple de la línea de comandos.

## Haga una copia de seguridad de sus datos

Antes de instalar Debian, cree una copia de seguridad completa de su directorio de inicio en una unidad externa. Incluya cualquier carpeta de datos adicional que desee conservar.

Nota: En Linux, `~` representa su directorio de inicio.

Si utiliza repositorios Git, envíe cualquier cambio importante a sus orígenes para que puedan restaurarse fácilmente después de la instalación. Este paso sólo es relevante si ya usas Git.

## Crear una partición

Cree espacio en su disco principal para Debian. Existen muchas guías y herramientas para este paso, incluido GParted. Dependiendo de su configuración, puede:

- reducir una partición de Windows existente para arranque dual
- reutilizar una partición de Linux existente
- preparar nuevo Linux e intercambiar particiones

Si no está seguro, consulte las guías específicas del hardware antes de realizar cambios, ya que los pasos de partición varían significativamente entre sistemas.


## Crear un USB de instalación de Debian

Suponiendo que ya existen una partición de destino y un espacio de intercambio:

1. Descargue la ISO de Debian desde el sitio web oficial: https://www.debian.org/
2. En Windows, utilice BalenaEtcher para escribir el ISO en una unidad USB.
3. En Linux, utilice una herramienta de línea de comandos como `dd` para crear un USB de arranque.

## Instalar Debian

1. Inserte la unidad USB.
2. Reinicie y presione la tecla del menú de inicio (comúnmente `F2`, `F12`, `Esc` o `Del`) durante el inicio.
3. Seleccione el dispositivo USB.
4. Elija un instalador no gráfico.
5. Deje la contraseña de root en blanco cuando se le solicite para que el instalador le otorgue acceso sudo a su cuenta de usuario.
6. Partición manual:

   - Sistema de archivos: ext4 (diario)
   - Intercambio: partición de intercambio existente
   - Punto de montaje: `/`
   - Etiqueta: `linux`
   - Nombre de host: nombre del sistema que se muestra como `user@hostname`
   - Cuenta de usuario: su nombre completo
   - Nombre de usuario: nombre de inicio de sesión del terminal

7. Seleccione **Cinnamon** como entorno de escritorio.
8. Complete la instalación y reinicie en Debian Stable.

## Configuración del sistema

### Escala de visualización

Actualmente, Debian Stable maneja el escalado fraccionario de manera inconsistente, especialmente en pantallas 4K. En lugar de reducir la resolución de la pantalla, ajuste los elementos de la interfaz directamente.

Ajustes recomendados:- Evite el escalado de visualización fraccional.
- Menú → Selección de fuente → Configuración de fuente → Factor de escala de texto: `2.5`
- Fuente de escritorio: `14`
- Panel → Personalizar → Altura del panel: `60`
- Apariencia del panel → Tamaño del icono simbólico de la zona derecha: `48px`
- Ratón y panel táctil → Ajuste del tamaño del puntero
- Escritorio (clic derecho) → Personalizar → Tamaño de icono más grande

Ajuste de Firefox:

- Barra de direcciones → `about:config`
- Establece `layout.css.devPixelsPerPx` en `1`

### terminal

Configurar las preferencias del terminal:

1. Menú → Terminal → Editar → Preferencias
2. Texto → Tamaño inicial: `140 columns`, `40 rows`
3. Texto → Fuente personalizada: `Monospace 10`
4. Colores → Esquemas integrados → Solarizado Oscuro

## Restaurar datos

Restaure los archivos respaldados en el directorio de inicio según sea necesario, por ejemplo:

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Nota: Las carpetas que comienzan con `.` son directorios de configuración ocultos en Linux.

## Opcional: Configuración de Git

Solo es necesario si planeas construir Lumi o restaurar repositorios.

### Instalar Git

```bash
sudo apt install git
```

Configura tu identidad:

```bash
git config --global --edit
```

#### Acceso a GitLab

Restaurar el acceso al repositorio en GitLab o GitHub:

1. Cambie los permisos en el archivo de clave SSH: `chmod 600 ~/.ssh/id_rsa`
2. Agregue el usuario a la nueva instalación de Git: `ssh-add ~/.ssh/id_rsa`
3. Pruebe la conexión: `ssh -T git@ssh.gitlab.gnome.org` o `ssh -T git@github.com`

Para cada repositorio, busque los orígenes y restablezca la rama local para que coincida:

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Ejecute `git status` para confirmar que los repositorios estén limpios.

Ahora tenemos un nuevo sistema operativo con todos los datos y repositorios restaurados. Esta configuración refleja un entorno de trabajo conocido utilizado para el desarrollo de Lumi y se puede adaptar a flujos de trabajo individuales según sea necesario.

## Construya Lumi después de la configuración del sistema operativo

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