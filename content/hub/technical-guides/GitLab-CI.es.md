---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
---
La integración continua (CI) es una forma de probar, crear y validar automáticamente su código cada vez que se realizan cambios.

**GitLab** proporciona funciones CI/CD integradas a través de su archivo `.gitlab-ci.yml`. Este archivo, ubicado en la raíz de su repositorio, le dice a GitLab cómo construir y probar su proyecto. Define etapas y scripts que se ejecutan en un entorno limpio cada vez que se realizan cambios.

Este documento describe cómo funciona la canalización GitLab CI/CD de Lumi, incluida la función del archivo `.gitlab-ci.yml`, scripts de shell y herramientas externas como Meson y Ninja.

Para obtener documentación técnica detallada del proceso de compilación de Lumi CI, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) en el repositorio.

## Conceptos básicos de GitLab CI/CD

El CI está controlado por un archivo llamado `.gitlab-ci.yml`. Este archivo define:

- **Etapas**: grupos ordenados de trabajos (por ejemplo, `build-this`, `build-that`, `package-up`)
- **Trabajos**: Tareas individuales para ejecutar dentro de cada etapa
- **Scripts**: comandos de Shell ejecutados para cada trabajo
- **Runners**: Computadoras que GitLab usa para ejecutar trabajos definidos en el pipeline.

En Lumi, las etapas del proceso son:

- `dependencies`
- `build lumi`
- `appimage`

## Construcciones basadas en contenedores

El canal Lumi utiliza contenedores para compilaciones consistentes:

1. **Creación del contenedor de compilación**: la primera etapa utiliza Buildah para crear una imagen de Docker con todas las dependencias.
2. **Uso del contenedor**: las etapas posteriores se ejecutan dentro de este contenedor, lo que garantiza un entorno coherente.
3. **Compilaciones reproducibles**: el aislamiento del contenedor garantiza los mismos resultados en diferentes ejecutores

Este enfoque garantiza que las compilaciones funcionen de la misma manera en cualquier ejecutor de GitLab y proporciona un entorno controlado para procesos de compilación complejos.

## Función de los scripts de Shell

Los trabajos en `.gitlab-ci.yml` normalmente invocan comandos de shell directamente. Las operaciones complejas a menudo se trasladan a scripts separados almacenados en el repositorio.

Lumi CI utiliza scripts de shell modulares para organizar la lógica de construcción:

**Ejemplo de invocación de script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Beneficios de este enfoque:**
- **Limpiar YAML**: mantiene el archivo `.gitlab-ci.yml` centrado en la estructura del trabajo.
- **Mantenibilidad**: la lógica compleja es más fácil de depurar y modificar en scripts de shell
- **Reutilizabilidad**: los scripts se pueden utilizar en diferentes contextos o entornos.
- **Modularidad**: diferentes aspectos de la construcción se pueden separar en scripts enfocados

Esto mantiene limpia la configuración de CI y al mismo tiempo permite procesos de compilación sofisticados.

## Integración con sistemas de construcción

Lumi usa **Meson** y **Ninja** para preparar y luego compilar el código.

Por ejemplo:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Aquí:

- `meson setup` prepara el directorio de compilación y genera `build.ninja`
- `ninja` ejecuta los comandos de compilación tal como se definen

## Estructura del sistema de construcción de Meson

El sistema de compilación **Meson** utiliza un archivo raíz `meson.build` ubicado en el directorio raíz del proyecto. Este archivo define la configuración de compilación de nivel superior y el punto de entrada para el proceso de compilación.

- La raíz `meson.build` normalmente se encuentra en el mismo directorio que `.gitlab-ci.yml`
- Desde allí, **cae en cascada recursivamente** en subdirectorios, cada uno de los cuales puede tener su propio archivo `meson.build`
- Estos archivos de subdirectorio definen destinos, fuentes, dependencias y crean instrucciones relevantes para ese directorio.

## Variables de entorno

Las variables clave en el proceso de Lumi incluyen:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variables específicas del trabajo:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```Estas variables controlan el comportamiento de la construcción y garantizan la coherencia entre las diferentes etapas y corredores.

## Estructura de ejemplo

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

En esta estructura:

- El archivo raíz `meson.build` configura el entorno de compilación general.
- Los archivos del subdirectorio `meson.build` manejan detalles de compilación para componentes o módulos específicos.
- Este diseño jerárquico mantiene la lógica de construcción modular y mantenible.

## Artefactos entre etapas

Los artefactos son archivos generados por trabajos que se necesitan en etapas posteriores:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Etapas y dependencias del pipeline

El oleoducto Lumi consta de tres etapas principales:

1. **Dependencias**: crea un entorno de compilación en contenedores con todas las herramientas y bibliotecas necesarias.
2. **Build Lumi**: Compila Lumi usando Meson y Ninja en el entorno preparado.
3. **AppImage**: empaqueta la aplicación creada en un formato AppImage distribuible.

**Dependencias de etapa:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Cada etapa se ejecuta solo después de que sus dependencias se completen exitosamente, lo que garantiza el orden de compilación adecuado y la disponibilidad de los artefactos.

## Nombres de trabajos actuales

Lumi `.gitlab-ci.yml` actualmente define estos nombres de trabajo:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Resumen

- `.gitlab-ci.yml` define la estructura y lógica de la canalización
- Los trabajos contienen comandos de shell o scripts externos
- Herramientas como Meson y Ninja se utilizan dentro de los trabajos como parte del proceso de construcción.

Lumi utiliza GitLab CI para crear automáticamente su AppImage para plataformas basadas en Debian. La canalización crea dependencias, compila Lumi y luego empaqueta una AppImage.

Para obtener detalles a nivel de fuente, utilice:

- `.gitlab-ci.yml` en la raíz del repositorio de Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Para obtener detalles técnicos completos sobre el proceso de compilación de Lumi CI, incluida la configuración del entorno, la arquitectura del script y la solución de problemas, consulte [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).