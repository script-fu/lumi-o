---
title: "Processus par lots"
type: docs
---
Un exemple pratique de bout en bout pour traiter plusieurs fichiers en une seule fois.

## Où il vit

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Où il apparaît dans Lumi

- **Fichier → Traitement par lots**

## Ce que cela démontre

- Paramètres `SF-DIRNAME` pour les répertoires source/destination
- Validation des chemins GUI avec des solutions de repli (`validate-path-and-dir`)
- Analyse et itération récursives des répertoires
- Rapports d'avancement pour les opérations de longue durée