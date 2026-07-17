---
title: "Processo batch"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
Un esempio pratico e completo per elaborare molti file in una volta sola.

## Codice sorgente

- [Visualizza il codice sorgente](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menu in Lumi

- **File → Elaborazione batch**

## Cosa dimostra

- Parametri `SF-DIRNAME` per le directory di origine/destinazione
- Convalida dei percorsi della GUI con fallback (`validate-path-and-dir`)
- Scansione e iterazione ricorsiva delle directory
- Reportistica sullo stato di avanzamento delle operazioni di lunga durata
