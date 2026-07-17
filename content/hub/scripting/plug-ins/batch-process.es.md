---
title: "Proceso por lotes"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
Un ejemplo práctico de extremo a extremo para procesar muchos archivos de una sola vez.

## Código fuente

- [Ver el código fuente](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menú en Lumi

- **Archivo → Proceso por lotes**

## Qué demuestra

- `SF-DIRNAME` parámetros para directorios de origen/destino
- Validación de rutas GUI con respaldos (`validate-path-and-dir`)
- Escaneo e iteración recursiva de directorios.
- Informes de progreso para operaciones de larga duración.
