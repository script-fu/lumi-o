---
title: "Compilar una versión de depuración"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Esta guía describe el **flujo de trabajo de depuración local** de Lumi usando los scripts en `build/lumi/scripts`.

El flujo de trabajo está pensado para:

- usar artefactos de compilación locales (no se requieren descargas de símbolos),
- verificar que los símbolos de depuración están realmente presentes,
- iniciar GDB con el modo de símbolos sin conexión activado por defecto.

## Requisitos previos

- Linux basado en Debian (referencia del proyecto: Debian 13)
- Árbol de código fuente de Lumi ya clonado

## Configuración única de GDB (opcional pero recomendada)

Instala las herramientas GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configuración opcional de registro local:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: los scripts de depuración local de Lumi desactivan `debuginfod` por defecto para mantener la resolución de símbolos local y reproducible.

## Inicio rápido

Desde el directorio de scripts:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Compilación de depuración e inicio (predeterminado)

Úsalo para sesiones de depuración habituales.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Este comando:

1. compila Lumi en modo depuración,
2. verifica los símbolos de depuración,
3. inicia Lumi bajo GDB.

### Solo compilación de depuración (para una sesión TTY/remota posterior)

Úsalo cuando quieras compilar ahora e iniciar o depurar más tarde.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Usar TTY en Linux

Las TTY (consolas de texto) suelen ser la forma más fiable de depurar bloqueos totales.

- Cambia a una TTY con `Ctrl + Alt + F1` hasta `Ctrl + Alt + F6`
- Inicia sesión desde el aviso de texto
- Vuelve a la sesión gráfica con `Ctrl + Alt + F7` (o `F2` en algunos sistemas)

Por qué importa: si la sesión de escritorio se bloquea, una TTY suele seguir respondiendo, de modo que puedes adjuntar GDB, capturar un backtrace y recuperar datos útiles del fallo.

## Opcional: depuración remota/TTY

Para bloqueos totales o bloqueos de pantalla, usa `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Luego, desde una TTY (recomendado para escenarios de bloqueo) u otra terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Para un inicio local de GDB (ruta sin TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota sobre el rendimiento

Las compilaciones de depuración son más lentas por diseño. Cuando termines de depurar, vuelve a una compilación más rápida:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
