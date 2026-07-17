---
title: "Processus par lots"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
---
Un exemple pratique de bout en bout pour traiter plusieurs fichiers en une seule fois.

## Code source

- [Voir le code source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Emplacement dans Lumi

- **Fichier → Traitement par lots**

## Ce que cela montre

- Paramètres `SF-DIRNAME` pour les répertoires source/destination
- Validation des chemins GUI avec des solutions de repli (`validate-path-and-dir`)
- Analyse et itération récursives des répertoires
- Rapports d'avancement pour les opérations de longue durée
