---
title: "Imagen de aplicación"
type: docs
url: "hub/technical-guides/folder/AppImage"
---
Una AppImage es un paquete de aplicación de Linux de un solo archivo. Descarga un archivo, lo marca como ejecutable y lo ejecuta sin instalar software en todo el sistema.

Sitio oficial de AppImage: https://appimage.org/

AppImage proporciona una versión portátil de Lumi que se ejecuta sin instalación ni modificación del sistema. Es ideal para artistas que desean utilizar el software inmediatamente sin administrar dependencias, compilar código fuente o configurar un entorno de desarrollo.

Como ejecutable autónomo, AppImage se puede almacenar en cualquier parte del sistema. Esto facilita probar nuevas versiones, conservar varias versiones o mover el software entre máquinas.

Para el proceso de desarrollo de Lumi, AppImage funciona como una versión de prueba portátil que se asemeja mucho al resultado de integración continua. Esto permite realizar pruebas confiables en un entorno consistente y al mismo tiempo mantener las compilaciones de fuentes locales enfocadas en el trabajo de desarrollo.

## Imagen de aplicación de lanzamiento versus desarrollo

- **Release AppImage**: aún no disponible (Lumi no ha sido lanzada).
- **Imagen de aplicación de desarrollo (artefacto de CI)**: generada automáticamente a partir de confirmaciones de desarrollo en curso para pruebas.

Esta guía cubre principalmente el flujo de trabajo de **desarrollo de AppImage**.

Página de artefacto actual:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Conceptos básicos de descarga de CI AppImage

CI produce archivos zip de artefactos (por ejemplo `lumi-appimage*.zip`).

Flujo manual básico:

1. Descargue el archivo zip de artefactos de CI más reciente.
2. Extraerlo.
3. Ejecute el archivo `Lumi*.AppImage` incluido.

Los scripts siguientes son ayudas opcionales que automatizan estos pasos.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Scripts auxiliares opcionales

- `lumi-appimage-unpack-zip.sh`
  - encuentra el último `lumi-appimage*.zip` en `~/Downloads`
  - instala AppImage en `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instala recursos de escritorio en `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - inicia AppImage en una terminal
  - habilita la salida en tiempo de ejecución (`APPIMAGE_DEBUG=1`)

## Notas comunes

- Si ejecuta AppImage manualmente (sin scripts auxiliares), primero hágalo ejecutable:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` ya aplica permisos ejecutables automáticamente.

- Si Lumi ya se está ejecutando desde otra versión, ciérrela antes de iniciar AppImage.