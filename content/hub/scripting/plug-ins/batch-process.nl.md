---
title: "Batchproces"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
Een praktisch, end-to-end voorbeeld voor het in één keer verwerken van veel bestanden.

## Broncode

- [Broncode bekijken](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Menu in Lumi

- **Bestand → Batchproces**

## Wat het laat zien

- `SF-DIRNAME` parameters voor bron-/bestemmingsmappen
- Valideren van GUI-paden met fallbacks (`validate-path-and-dir`)
- Recursief directoryscannen en iteratie
- Voortgangsrapportage voor langlopende operaties
