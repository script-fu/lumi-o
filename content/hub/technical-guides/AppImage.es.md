---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 939beb6f4f1657ab77f785d1753385360cca7920b6291dbcd09f4687bfdfc502
---

Un AppImage es un paquete de aplicación de Linux en un solo archivo. Descargas un archivo, lo marcas como ejecutable y lo ejecutas sin instalar software en todo el sistema.

Sitio oficial de AppImage: https://appimage.org/

El AppImage ofrece una versión portátil de Lumi que funciona sin instalación ni modificación del sistema. Es ideal para artistas que quieren usar el software de inmediato sin gestionar dependencias, compilar código fuente ni configurar un entorno de desarrollo.

Como ejecutable autónomo, el AppImage puede guardarse en cualquier parte del sistema. Esto facilita probar nuevas versiones, conservar varias versiones o mover el software entre equipos.

En el proceso de desarrollo de Lumi, el AppImage funciona como una compilación de prueba portátil que se ajusta de cerca a la salida de integración continua. Esto permite realizar pruebas fiables en un entorno coherente, manteniendo las compilaciones locales centradas en el trabajo de desarrollo.

Nota: CI compila el AppImage usando las fuentes de dependencias integradas en el repositorio de Lumi (BABL/GEGL/GTK3), por lo que la pila de dependencias es coherente con el flujo de trabajo local `lumi-build-script.sh`.

## AppImage de release frente a AppImage de desarrollo

- **Release AppImage**: aún no disponible (Lumi no se ha publicado).
- **AppImage de desarrollo (artefacto de CI)**: generado automáticamente a partir de los commits de desarrollo en curso para pruebas.

Esta guía cubre principalmente el flujo de trabajo del **AppImage de desarrollo**.

Página de artefactos actual:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## Conceptos básicos de descarga del AppImage de CI

CI produce archivos zip de artefactos (por ejemplo, `lumi-appimage*.zip`).

Flujo manual básico:

1. Descarga el zip de artefactos de CI más reciente.
2. Extráelo.
3. Ejecuta el archivo `Lumi*.AppImage` incluido.

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
  - encuentra el `lumi-appimage*.zip` más reciente en `~/Downloads`
  - instala el AppImage en `~/AppImage/Lumi/Lumi_CI.AppImage`
  - instala recursos de escritorio en `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - inicia el AppImage en una terminal
  - habilita la salida en tiempo de ejecución (`APPIMAGE_DEBUG=1`)

## Notas habituales

- Si ejecutas el AppImage manualmente (sin scripts auxiliares), hazlo ejecutable primero:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` ya aplica los permisos de ejecución automáticamente.

- Si Lumi ya se está ejecutando desde otra compilación, ciérralo antes de iniciar el AppImage.
