---
title: "Format de fichier (.lum)"
type: docs
---
Lumi utilise un format de fichier ouvert basé sur un répertoire (`.lum`) conçu pour les performances, la fiabilité et l'accessibilité à long terme.

## Aperçu

Un fichier `.lum` est en fait un répertoire contenant :
- **Métadonnées** (calques, modes de fusion, propriétés).
- **Tampons de calque** (données de pixels individuelles pour chaque calque).
- **Masques** (données en niveaux de gris pour les masques de calque).
- **Historique de récupération** (instantanés incrémentiels).

Cette structure permet une sauvegarde rapide, un chargement paresseux de fichiers volumineux et une récupération du travail même après un crash.

## Propriétés clés

### Ouvert et lisible

Le format `.lum` utilise des métadonnées XML et des tampons binaires compressés. Vous pouvez inspecter la structure des calques, les propriétés et les modes de fusion en texte brut. Pas de codec propriétaire ; les données de pixels sont stockées au format tampon GEGL standard.

### Économie incrémentielle

L'enregistrement incrémentiel doit être activé pour chaque projet dans la boîte de dialogue **Enregistrer sous** (une case à cocher **Enregistrement incrémentiel** et un bouton rotatif **Max Saves**). Une fois activé, Ctrl+S écrit uniquement les calques modifiés plutôt que de réécrire l'intégralité du projet, ce qui réduit considérablement le temps de sauvegarde. Le paramètre est stocké avec le projet et persiste au fil des sessions.

### Chargement paresseux

Les grands projets s'ouvrent rapidement. Les pixels du calque sont chargés à partir du disque uniquement lorsque :
- Le calque est rendu visible.
- Vous peignez sur le calque.
- Le calque est exporté ou composé.

Les très gros projets (plus de 500 couches, plusieurs gigaoctets de données) restent réactifs. Le chargement différé est activé par défaut et peut être activé dans **Édition → Préférences → Performances → Ressources mémoire**.

### Sauvegarde automatique

Lumi enregistre automatiquement les modifications dans un **emplacement de cache distinct** (`~/.cache/lumi/autosave/`) à intervalles réguliers. Les sauvegardes automatiques sont indépendantes du fichier de travail et ne le modifient pas. L'intervalle et l'emplacement du cache sont configurables dans **Édition → Préférences → Performances**.

## Accès

### Enregistrer et enregistrer sous

- **Fichier** → **Enregistrer** (Ctrl+S) : Enregistrer dans le répertoire `.lum` actuel.
- **Fichier** → **Enregistrer sous** (Maj+Ctrl+S) : enregistrez dans un nouveau fichier `.lum`. La boîte de dialogue Enregistrer sous comprend des options pour le type de compression et une bascule **Enregistrement incrémentiel** (avec une limite **Enregistrements maximum**) pour activer ou désactiver l'enregistrement incrémentiel pour ce projet.

Les modifications non enregistrées sont indiquées par un astérisque (*) dans le titre de la fenêtre.

### Exporter

- **Fichier** → **Exporter sous** (Maj+Ctrl+E) : exporter au format PNG, JPEG, TIFF ou autres formats.
- **Fichier** → **Écraser** (Ctrl+E) : réexporter vers le dernier fichier exporté.

L’exportation aplatit les calques visibles et convertit l’espace colorimétrique spectral en espace colorimétrique sRGB.

### Importer

- **Fichier** → **Ouvrir** (Ctrl+O) : Charger un projet `.lum`.
- **Fichier** → **Ouvrir en tant que calques** (Maj+Ctrl+O) : importez des fichiers `.lum`, XCF ou PSD en tant que nouveaux calques.
- **Fichier** → **Fichiers récents** : accès rapide aux projets récemment ouverts.

Les fichiers PSD et XCF sont convertis au format natif de Lumi lors de l'importation.

## Compatibilité d'importation et d'exportation

### Formats d'importation pris en charge
- **.lum** : format natif Lumi.
- **.xcf** : format natif de GIMP (calques et propriétés de base préservés).
- **.psd** : format Photoshop (calques et modes de fusion conservés).
- **PNG, JPEG, TIFF, etc.** : Importation d'images aplaties.

### Formats d'exportation pris en charge
- **PNG** : Sans perte, avec transparence alpha.
- **JPEG** : avec perte, aplati.
- **TIFF** : sans perte ou compressé LZW.
- **XCF** : format de compatibilité GIMP. Exportation uniquement ; couches et propriétés de base préservées.

## Récupération de projetLumi maintient des sauvegardes automatiques en arrière-plan et des points de contrôle incrémentiels manuels, tous deux accessibles depuis **Fichier** → **Récupérer l'image**. Consultez la page [File Recovery](../recovery) pour plus de détails.

## Organisation

Un fichier `.lum` est un répertoire avec une structure fixe :

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Les tampons de couche portent le nom de la couche (`layer-Background.geglbuf`), et non numérotés séquentiellement. Les espaces dans les noms de calques sont stockés sous forme de traits de soulignement ; les calques de groupe reçoivent un suffixe `-GROUP`. Les masques partagent le nom du calque (`mask-Background.geglbuf`).

Chaque `recovery/primary-NN.lum/` est une sauvegarde de base complète. Les pressions suivantes sur Ctrl+S ajoutent les sous-répertoires `delta-NNNN.lum/` contenant uniquement les tampons modifiés depuis la dernière ligne de base, ce qui permet d'enregistrer rapidement les points de contrôle, quelle que soit la taille du projet.

Les sauvegardes automatiques suivent la même structure mais sont stockées séparément dans `~/.cache/lumi/autosave/`, laissant le fichier de travail intact.
- **Très grands projets** : un projet avec plus de 1 000 couches et téraoctets de données bénéficiera le plus du chargement paresseux ; cependant, l'exportation finale au format d'image plate peut prendre du temps.
- **Lecteurs réseau** : l'enregistrement dans des répertoires montés en réseau est pris en charge mais plus lent que le stockage local en raison de la latence d'E/S.