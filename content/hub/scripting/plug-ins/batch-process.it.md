---
title: "Processo batch"
type: docs
---
Un esempio pratico e completo per elaborare molti file in una volta sola.

## Dove vive

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Dove appare in Lumi

- **File → Elaborazione batch**

## Cosa dimostra

- Parametri `SF-DIRNAME` per le directory di origine/destinazione
- Convalida dei percorsi della GUI con fallback (`validate-path-and-dir`)
- Scansione e iterazione ricorsiva delle directory
- Reportistica sullo stato di avanzamento delle operazioni di lunga durata