---
title: "Instalación"
type: docs
url: "hub/technical-guides/Installation"
---
Esta guía utiliza los scripts de compilación actuales de Lumi en:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Instalar dependencias (configuración por primera vez)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Construye Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Inicie Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Tipos de compilación

Utilice `--type` cuando sea necesario:

- `debug` – flujos de trabajo de depuración
- `debugoptimized` – valor predeterminado equilibrado para el desarrollo
- `release` – tiempo de ejecución más rápido

Ejemplo:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```