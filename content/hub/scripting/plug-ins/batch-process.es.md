---
title: "Proceso por lotes"
type: docs
---
Un ejemplo práctico de extremo a extremo para procesar muchos archivos de una sola vez.

## Donde vive

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Dónde aparece en Lumi

- **Archivo → Proceso por lotes**

## Lo que demuestra

- `SF-DIRNAME` parámetros para directorios de origen/destino
- Validación de rutas GUI con respaldos (`validate-path-and-dir`)
- Escaneo e iteración recursiva de directorios.
- Informes de progreso para operaciones de larga duración.