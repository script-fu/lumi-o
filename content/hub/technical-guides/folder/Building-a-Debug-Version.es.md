---
title: "Construyendo una versión de depuración"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
Esta guía describe el **flujo de trabajo de depuración local** para Lumi usando scripts en `build/lumi/scripts`.

El flujo de trabajo está diseñado para:

- utilizar artefactos de compilación locales (no se requieren descargas de símbolos),
- verificar que los símbolos de depuración estén realmente presentes,
- Inicie GDB con el modo de símbolo fuera de línea de forma predeterminada.

## Requisitos previos

- Linux basado en Debian (línea base del proyecto: Debian 13)
- El árbol fuente de Lumi ya está clonado.

## Configuración de GDB por única vez (opcional pero recomendada)

Instalar herramientas GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuración de registro local opcional:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: Los scripts de depuración locales de Lumi desactivan `debuginfod` de forma predeterminada para mantener la resolución de los símbolos local y reproducible.

## Inicio rápido

Desde el directorio de scripts:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Depuración, compilación + lanzamiento (predeterminado)

Úselo para sesiones de depuración normales.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Este comando:

1. construye Lumi en modo de depuración,
2. verifica los símbolos de depuración,
3. lanza Lumi bajo GDB.

### Solo compilación de depuración (para sesión TTY/remota posterior)

Úselo cuando desee compilar ahora e iniciar/depurar más tarde.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Uso de TTY en Linux

Las TTY (consolas de texto) suelen ser la forma más confiable de depurar congelaciones totales.

- Cambie a un TTY con `Ctrl + Alt + F1` a través de `Ctrl + Alt + F6`
- Inicie sesión desde el mensaje de texto
- Regresar a la sesión gráfica con `Ctrl + Alt + F7` (o `F2` en algunos sistemas)

Por qué esto es importante: si la sesión de escritorio se detiene, un TTY a menudo aún responde, por lo que puede adjuntar GDB, capturar un seguimiento y recuperar datos útiles sobre fallas.

## Opcional: Depuración remota/TTY

Para congelaciones totales o bloqueos de pantalla, use `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Luego desde un TTY (recomendado para escenarios de congelación) u otra terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Para un lanzamiento de GDB local (ruta que no es TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota de rendimiento

Las compilaciones de depuración son más lentas por diseño. Cuando haya terminado de depurar, vuelva a una compilación más rápida:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```