---
title: "Gestion des couleurs"
type: docs
weight: 15
---
Lumi-o est configuré pour fonctionner immédiatement. Tant que vous travaillez sur une image avec une précision de **16 bits ou plus**, le logiciel est déjà configuré pour utiliser l'épreuvage écran (CMYK) fourni par défaut et les profils sRGB intégrés ; tout devrait fonctionner sans aucune configuration.

Pour ceux qui ont besoin d'un contrôle plus approfondi, ce guide explique le modèle de gestion des couleurs de base de Lumi, la différence entre un profil d'image et un profil d'épreuve écran, où se trouvent les contrôles et comment exactement les profils par défaut sont associés à l'application.

## Résumé rapide

Lumi utilise trois rôles de profil différents :

1. **Profil de travail d'image**
   - Définit la signification des nombres RVB ou en niveaux de gris de l'image.
   - Utilisé pour les opérations d'affectation/conversion.
   - Exemples typiques : sRGB intégré, Adobe RVB.

2. **Afficher le profil**
   - Décrit votre moniteur.
   - Utilisé pour afficher correctement l'image sur votre écran.
   - Généralement fourni par le système ou choisi dans les Préférences.

3. **Profil souple**
   - Simule un autre périphérique de sortie ou une condition d'impression.
   - Ne redéfinit **pas** les valeurs des pixels de l'image.
   - Exemples typiques : profils de presse CMJN tels que `CoatedFOGRA39`.

## Profil d'image vs profil Soft-Proof

### Profil d'image

Utilisez-le lorsque vous souhaitez indiquer à Lumi dans quel espace colorimétrique se trouve réellement l'image.

Deux opérations courantes :

- **Attribuer un profil**
  - Modifie l'étiquette de profil attachée à l'image.
  - Ne convertit **pas** les valeurs des pixels.
  - À utiliser uniquement lorsque les numéros de pixels sont déjà dans l'espace de ce profil.

- **Convertir en profil**
  - Convertit les valeurs de pixels du profil d'image actuel vers un nouveau.
  - À utiliser lorsque vous souhaitez que l'image se déplace réellement dans un espace de travail différent.

**Emplacements des menus :**
- Image > Gestion des couleurs > Attribuer un profil de couleur...
- Image > Gestion des couleurs > Convertir en profil de couleur...

### Profil à l'épreuve des logiciels

Utilisez-le lorsque vous souhaitez prévisualiser la manière dont l'image sera reproduite sur un périphérique cible ou dans les conditions d'impression.

Épreuvage à l'écran :
- laisse l'espace de travail de l'image seul
- modifie le pipeline d'aperçu
- peut marquer les couleurs hors gamme
- est destiné à l'aperçu et non à la réaffectation des données d'image

**Emplacements des menus :**
- Image > Gestion des couleurs > Paramètres d'épreuve écran > Choisir le profil d'épreuve écran...
- Image > Gestion des couleurs > Paramètres d'épreuvage > Intention de rendu
- Image > Gestion des couleurs > Paramètres d'épreuvage > Compensation du point noir
- Affichage > Gestion des couleurs > Activer l'aperçu anti-écran
- Affichage > Gestion des couleurs > Marquer les couleurs hors gamme

## Comment voir l'aperçu à l'épreuve du logiciel

Il existe deux points d’entrée principaux pour basculer entre les épreuves écran.

### 1. Afficher le menu

Utilisation :
- Affichage > Gestion des couleurs > Activer l'aperçu anti-écran

Cela active ou désactive la simulation d’aperçu pour l’affichage actuel.

### 2. Basculer la barre d'état

Lumi expose également la vérification à l'écran directement dans la barre d'état inférieure.

- **Clic gauche** (bascule) : activer ou désactiver les couleurs d'épreuve
- **Clic droit** : ouvrez la fenêtre contextuelle de vérification écran dans laquelle vous pouvez modifier :
  - profil actuel
  - sélecteur de profil
  - intention de rendu
  - compensation du point noir
  - marquage hors gamme

{{< callout type="warning" >}}
**Remarque importante sur la précision**
L'aperçu à l'épreuve écran n'est activé que pour les images **16 bits et 32 bits**.
Pour les images **8 bits**, la bascule est désactivée et Lumi vous demandera d'abord de convertir la précision en une profondeur plus élevée avant de prévisualiser les couleurs avec précision.
{{< /callout >}}

## Préférences et valeurs par défaut

Les défauts mondiaux résident dans :
- Edition > Préférences > Gestion des couleursRubriques concernées :
- **Profil de moniteur manuel**
- **Profil RVB préféré**
- **Profil en niveaux de gris préféré**
- **Soft-Proofing**

### Paramètres par défaut actuels de Lumi

#### Espaces de travail

ICC d'espace de travail groupés actuellement proposés à partir du dossier de données partagé :
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Pour le travail sRGB standard, Lumi fournit également un **profil de travail sRGB intégré en interne**.

#### Paramètres par défaut de la vérification logicielle

Profils soft-proof groupés actuellement installés :
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Lorsqu’il est disponible, `CoatedFOGRA39.icc` est utilisé comme profil de référence d’épreuve écran/CMYK fourni par défaut.

## Flux de travail pratiques

### Pour la peinture et le travail normal sur écran

- Conservez l'image dans le sRGB intégré ou dans un autre espace de travail RVB valide.
- Laissez Lumi utiliser le profil du moniteur système s'il est disponible.

### Pour un aperçu avant impression

- Conservez l'image dans son espace de travail RVB standard.
- Choisissez un profil d'épreuve écran qui correspond à la condition d'impression cible (par exemple FOGRA39).
- Activer l'aperçu à l'épreuve du logiciel.
- Activez éventuellement les avertissements de gamme pour voir les intentions de rendu tronquées.