---
title: "Instalación"
type: docs
---
Necesitas Git para el paso de clonación inicial a continuación. Si Git aún no está instalado, instálelo primero (Debian/Ubuntu: `sudo apt install git`) o siga: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Clonar Lumi (configuración por primera vez)

Cree el directorio para Lumi y use Git para clonar el código fuente.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Instalar dependencias (configuración por primera vez)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Construir Lumi (configuración por primera vez)

Primera compilación de configuración completa (por primera vez o después de cambios importantes):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Inicie Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Opcional: reconstruir/compilar

Reconstrucción normal después de cambios de código:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Ruta rápida de solo compilación:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Cree un único componente integrado (reemplace `babl` con `gegl` o `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Opcional: tipos de compilación

Utilice `--type` cuando sea necesario:

- `debug` – flujos de trabajo de depuración
- `debugoptimized` – valor predeterminado equilibrado para el desarrollo
- `release` – tiempo de ejecución más rápido

Ejemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```