---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

La integración continua (CI) es una forma de probar, compilar y validar automáticamente tu código cada vez que se realizan cambios.

**GitLab** ofrece funciones CI/CD integradas mediante su archivo `.gitlab-ci.yml`. Este archivo, ubicado en la raíz de tu repositorio, indica a GitLab cómo compilar y probar tu proyecto. Define etapas y scripts que se ejecutan en un entorno limpio cada vez que se envían cambios.

Este documento describe cómo funciona la pipeline CI/CD de GitLab de Lumi, incluido el papel del archivo `.gitlab-ci.yml`, los scripts de shell y herramientas externas como Meson y Ninja.

Para documentación técnica detallada del proceso de compilación CI de Lumi, consulta [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) en el repositorio.

## Conceptos básicos de GitLab CI/CD

La CI se controla mediante un archivo llamado `.gitlab-ci.yml`. Este archivo define:

- **Etapas**: grupos ordenados de jobs (p. ej., `build-this`, `build-that`, `package-up`)
- **Jobs**: tareas individuales que se ejecutan en cada etapa
- **Scripts**: comandos de shell ejecutados para cada job
- **Runners**: equipos que GitLab usa para ejecutar los jobs definidos en la pipeline

En Lumi, las etapas de la pipeline son:

- `dependencies`
- `build lumi`
- `appimage`

## Compilaciones basadas en contenedores

La pipeline de Lumi usa contenedores para compilaciones coherentes:

1. **Creación del contenedor de compilación**: la primera etapa usa Buildah para crear una imagen Docker con todas las dependencias
2. **Uso del contenedor**: las etapas posteriores se ejecutan dentro de este contenedor, garantizando un entorno coherente
3. **Compilaciones reproducibles**: el aislamiento del contenedor garantiza los mismos resultados en distintos runners

Este enfoque garantiza que las compilaciones funcionen igual en cualquier runner de GitLab y proporciona un entorno controlado para procesos de compilación complejos.

### Fuentes de dependencias integradas

La imagen de dependencias CI de Lumi compila la pila bifurcada a partir de **fuentes integradas en el repositorio** (no clones externos):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Estos directorios se copian en el contexto de compilación del contenedor y se compilan en el prefijo de dependencias (normalmente `/opt/lumi-deps`). Esto mantiene la CI reproducible y garantiza que la compilación del AppImage use la misma fuente de verdad que el desarrollo local.

## Papel de los scripts de shell

Los jobs en `.gitlab-ci.yml` suelen invocar comandos de shell directamente. Las operaciones complejas a menudo se trasladan a scripts separados almacenados en el repositorio.

La CI de Lumi usa scripts de shell modulares para organizar la lógica de compilación:

**Ejemplo de invocación de script:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Ventajas de este enfoque:**
- **YAML limpio**: mantiene el archivo `.gitlab-ci.yml` centrado en la estructura de los jobs
- **Mantenibilidad**: la lógica compleja es más fácil de depurar y modificar en scripts de shell
- **Reutilización**: los scripts pueden usarse en distintos contextos o entornos
- **Modularidad**: distintos aspectos de la compilación pueden separarse en scripts específicos

Esto mantiene limpia la configuración de CI y permite procesos de compilación sofisticados.

## Integración con sistemas de compilación

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
- `ninja` ejecuta los comandos de compilación definidos

## Estructura del sistema de compilación Meson

El sistema de compilación **Meson** usa un archivo raíz `meson.build` ubicado en el directorio raíz del proyecto. Este archivo define la configuración de compilación de nivel superior y el punto de entrada del proceso de compilación.

- El `meson.build` raíz suele estar en el mismo directorio que `.gitlab-ci.yml`
- Desde ahí, **se extiende recursivamente** a subdirectorios, cada uno de los cuales puede tener su propio archivo `meson.build`
- Estos archivos de subdirectorio definen objetivos, fuentes, dependencias e instrucciones de compilación relevantes para ese directorio

## Variables de entorno

Las variables clave en la pipeline de Lumi incluyen:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Variables específicas del job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Estas variables controlan el comportamiento de la compilación y garantizan coherencia entre etapas y runners.

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

- El archivo raíz `meson.build` configura el entorno general de compilación
- Los archivos `meson.build` de subdirectorio gestionan los detalles de compilación de componentes o módulos concretos
- Este diseño jerárquico mantiene la lógica de compilación modular y mantenible

## Artefactos entre etapas

Los artefactos son archivos generados por jobs que se necesitan en etapas posteriores:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Etapas y dependencias de la pipeline

La pipeline de Lumi consta de tres etapas principales:

1. **Dependencies**: crea un entorno de compilación en contenedor con todas las herramientas y bibliotecas necesarias
2. **Build Lumi**: compila Lumi usando Meson y Ninja en el entorno preparado
3. **AppImage**: empaqueta la aplicación compilada en un formato AppImage distribuible

**Dependencias entre etapas:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Cada etapa se ejecuta solo cuando sus dependencias se completan correctamente, garantizando el orden de compilación adecuado y la disponibilidad de los artefactos.

## Nombres de jobs actuales

El `.gitlab-ci.yml` de Lumi define actualmente estos nombres de job:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Resumen

- `.gitlab-ci.yml` define la estructura y la lógica de la pipeline
- Los jobs contienen comandos de shell o scripts externos
- Herramientas como Meson y Ninja se usan dentro de los jobs como parte del proceso de compilación

Lumi usa GitLab CI para compilar automáticamente su AppImage en plataformas basadas en Debian. La pipeline compila dependencias, compila Lumi y empaqueta un AppImage.

Para detalles a nivel de código fuente, consulta:

- `.gitlab-ci.yml` en la raíz del repositorio de Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Para detalles técnicos completos sobre el proceso de compilación CI de Lumi, incluida la configuración del entorno, la arquitectura de scripts y la resolución de problemas, consulta [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
