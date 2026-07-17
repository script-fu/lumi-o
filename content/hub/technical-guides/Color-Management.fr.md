---
title: "Gestion des couleurs"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e124f17c1f65c73f4e135c25dd7962eb44f1d0676147a7e4bcbf6dc8ecf51e69
---

Lumi-o est configuré pour fonctionner immédiatement. Tant que vous travaillez sur une image en **précision 16 bits ou supérieure**, le logiciel est déjà configuré pour utiliser l'épreuvage écran (CMYK) fourni par défaut et les profils sRGB intégrés ; tout devrait fonctionner sans configuration.

Pour ceux qui ont besoin d'un contrôle plus approfondi, ce guide explique le modèle de gestion des couleurs de Lumi, la différence entre un profil d'image et un profil d'épreuvage écran, l'emplacement des contrôles et la façon dont les profils par défaut sont fournis avec l'application.

## Résumé rapide

Lumi utilise trois rôles de profil distincts :

1. **Profil de travail d'image**
   - Définit ce que signifient les valeurs RVB ou niveaux de gris de l'image.
   - Utilisé pour les opérations d'assignation et de conversion.
   - Exemples typiques : sRGB intégré, Adobe RGB.

2. **Profil d'affichage**
   - Décrit votre moniteur.
   - Utilisé pour afficher correctement l'image à l'écran.
   - Généralement fourni par le système ou choisi dans les Préférences.

3. **Profil d'épreuvage écran**
   - Simule un autre périphérique de sortie ou une condition d'impression.
   - Ne redéfinit **pas** les valeurs des pixels de l'image.
   - Exemples typiques : profils presse CMYK tels que `CoatedFOGRA39`.

## Profil d'image vs profil d'épreuvage écran

### Profil d'image

Utilisez-le lorsque vous voulez indiquer à Lumi dans quel espace colorimétrique se trouve réellement l'image.

Deux opérations courantes :

- **Assigner un profil**
  - Modifie l'étiquette de profil attachée à l'image.
  - Ne convertit **pas** les valeurs des pixels.
  - À utiliser uniquement lorsque les valeurs des pixels sont déjà dans l'espace de ce profil.

- **Convertir vers un profil**
  - Convertit les valeurs des pixels du profil d'image actuel vers un nouveau profil.
  - À utiliser lorsque vous voulez que l'image passe réellement dans un autre espace de travail.

**Emplacements dans les menus :**
- Image > Gestion des couleurs > Assigner un profil de couleur...
- Image > Gestion des couleurs > Convertir vers un profil de couleur...

### Profil d'épreuvage écran

Utilisez-le lorsque vous voulez prévisualiser la façon dont l'image serait reproduite sur un périphérique cible ou dans des conditions d'impression.

L'épreuvage écran :
- laisse l'espace de travail de l'image inchangé
- modifie le pipeline d'aperçu
- peut marquer les couleurs hors gamut
- est destiné à l'aperçu, pas à la réassignation des données d'image

**Emplacements dans les menus :**
- Image > Gestion des couleurs > Paramètres d'épreuvage écran > Choisir le profil d'épreuvage écran...
- Image > Gestion des couleurs > Paramètres d'épreuvage écran > Intention de rendu
- Image > Gestion des couleurs > Paramètres d'épreuvage écran > Compensation du point noir
- Affichage > Gestion des couleurs > Activer l'aperçu d'épreuvage écran
- Affichage > Gestion des couleurs > Marquer les couleurs hors gamut

## Comment afficher l'aperçu d'épreuvage écran

Il existe deux points d'entrée principaux pour activer ou désactiver l'épreuvage écran.

### 1. Menu Affichage

Utilisez :
- Affichage > Gestion des couleurs > Activer l'aperçu d'épreuvage écran

Cela active ou désactive la simulation d'aperçu pour l'affichage actuel.

### 2. Bascule de la barre d'état

Lumi expose également l'épreuvage écran directement dans la barre d'état inférieure.

- **Clic gauche** (bascule) : activer ou désactiver les couleurs d'épreuve
- **Clic droit** : ouvrir le popover d'épreuvage écran où vous pouvez ajuster :
  - profil actuel
  - sélecteur de profil
  - intention de rendu
  - compensation du point noir
  - marquage hors gamut

{{< callout type="warning" >}}
**Remarque importante sur la précision**
L'aperçu d'épreuvage écran n'est activé que pour les images **16 bits et 32 bits**.
Pour les images **8 bits**, la bascule est désactivée et Lumi vous demandera d'abord de convertir la précision vers une profondeur supérieure avant de prévisualiser les couleurs avec précision.
{{< /callout >}}

## Préférences et valeurs par défaut

Les valeurs par défaut globales se trouvent dans :
- Édition > Préférences > Gestion des couleurs

Sections concernées :
- **Profil de moniteur manuel**
- **Profil RVB préféré**
- **Profil en niveaux de gris préféré**
- **Épreuvage écran**

### Valeurs par défaut actuelles de Lumi

#### Espaces de travail

ICC d'espaces de travail fournis depuis le dossier de données partagé :
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Pour le travail sRGB standard, Lumi fournit également un **profil de travail sRGB intégré en interne**.

#### Valeurs par défaut d'épreuvage écran

Profils d'épreuvage écran fournis actuellement installés :
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Lorsqu'il est disponible, `CoatedFOGRA39.icc` est utilisé comme profil de référence d'épreuvage écran/CMYK fourni par défaut.

## Flux de travail pratiques

### Pour la peinture et le travail normal à l'écran

- Conservez l'image en sRGB intégré ou dans un autre espace de travail RVB valide.
- Laissez Lumi utiliser le profil moniteur système s'il est disponible.

### Pour l'aperçu avant impression

- Conservez l'image dans son espace de travail RVB standard.
- Choisissez un profil d'épreuvage écran correspondant à la condition d'impression cible (par ex. FOGRA39).
- Activez l'aperçu d'épreuvage écran.
- Activez éventuellement les avertissements de gamut pour voir les intentions de rendu tronquées.
