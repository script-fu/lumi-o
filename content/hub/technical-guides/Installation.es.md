---
title: "Instalación"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Necesitas Git para el paso de clonación inicial que se indica a continuación. Si Git aún no está instalado, instálalo primero (Debian/Ubuntu: `sudo apt install git`) o consulta: [Usar Git en Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clonar Lumi (configuración inicial)

Crea el directorio para Lumi y usa Git para clonar el código fuente.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Instalar dependencias (configuración inicial)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Compilar Lumi (configuración inicial)

Primera compilación completa de configuración (la primera vez o tras cambios importantes):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Iniciar Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opcional: recompilar / compilar

Recompilación habitual tras cambios en el código:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Ruta rápida de solo compilación:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Compilar un único componente integrado (sustituye `babl` por `gegl` o `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opcional: tipos de compilación

Usa `--type` cuando sea necesario:

- `debug` – flujos de trabajo de depuración
- `debugoptimized` – valor predeterminado equilibrado para el desarrollo
- `release` – máximo rendimiento en tiempo de ejecución

Ejemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
