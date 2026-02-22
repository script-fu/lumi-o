---
title: "Mélangeur à palettes"
type: docs
---
Le mélangeur de palettes dérive de nouvelles couleurs à partir de paires d'entrées de palette à l'aide d'un pipeline fixe en trois étapes. Étant donné que le mélange se produit dans le domaine spectral plutôt que dans le domaine RVB, les résultats se comportent comme des pigments physiques : le bleu et le jaune produisent du vert, les couleurs saturées virent au neutre à mesure qu'elles se mélangent.

## Le pipeline

Chaque couleur produite par le Mixer passe par trois étapes dans un ordre fixe :

1. **Mélange** : WGM spectral entre le parent A (CCW) et le parent B (CW).
2. **Chroma** : Mélangez vers le spectre neutre de la palette, réduisant ainsi la saturation.
3. **Ton** : Mélangez vers le mélange du blanc (teinte) ou le mélange du noir (nuance).

Le ton est toujours appliqué en dernier. Cela rend la légèreté dominante : un réglage de tonalité atterrit exactement au niveau de luminosité souhaité sans être dilué par le réglage de chrominance qui le précède.

## Sélection des parents

Parent A et Parent B sont les deux entrées entre lesquelles le curseur de fusion mélange. Ils sont chargés depuis la Palette Map :

- Maintenez **Shift** sur la carte de la palette et **cliquez avec le bouton gauche** pour définir le parent A (CCW).
- Maintenez **Shift** et **cliquez avec le bouton droit** pour définir le parent B (CW).

Seules les inscriptions **Classe A** (Primaires et mélanges personnalisés avec provenance intacte) sont acceptées comme parents. Les tertiaires et les entrées avec ascendance perdue sont exclus.

Les positions Parent A et Parent B du mixeur sont affichées sur la carte sous la forme d'un **anneau en diamant** mis en surbrillance afin que vous puissiez toujours voir quelles entrées sont chargées.

## Les curseurs

| Curseur | Effet |
| :--- | :--- |
| **Mélanger** | Se déplace entre le parent A (extrémité CCW) et le parent B (extrémité CW). À 0,0, le résultat correspond au parent A ; à 1,0, il correspond au parent B. |
| **Chrome** | Désature le mélange vers le neutre de la palette. Des valeurs plus élevées produisent des résultats plus sourds et plus terreux. |
| **Ton** | Déplace la luminosité vers un mélange de blanc (direction de la teinte) ou un mélange de noir (direction de la teinte). |

## Contrôles de valeur

**Value Lock** gèle la luminosité perceptuelle (CIE L\*) à son niveau actuel pendant que les autres curseurs se déplacent. Utilisez-le pour explorer la variation de chrominance ou de teinte sans modifier la valeur d'un mélange.

**Band Clamp** limite le résultat pour qu'il reste dans les limites de sa bande de valeur actuelle (par exemple, dans Lower Mid). Le curseur de tonalité est toujours déplaçable mais la luminosité de sortie est limitée.

Le curseur Tonalité reflète également les écarts de valeur configurés dans l'éditeur de palette. Les plages de luminosité qui se situent à l’intérieur d’un espace sont affichées sous forme de bandes grises semi-transparentes sur le creux du curseur. La poignée du curseur saute automatiquement par-dessus ces espaces : en faisant glisser le curseur dans une région grise, vous accédez à la limite de bande valide la plus proche de l'autre côté.

## Points de terminaison de mélange (blanc, noir, neutre)

Les étapes de tonalité et de chrominance nécessitent des points finaux de référence : un blanc mélangé, un noir mélangé et un neutre. Lumi les découvre automatiquement en recherchant dans la palette active les meilleurs candidats :

- **Mixing White** : le primaire à chroma le plus élevé le plus proche du blanc pur.
- **Mixing Black** : le primaire le plus faible en termes de luminosité.
- **Neutre** : le primaire le plus proche de l'achromatique (chroma le plus bas).

Ceux-ci peuvent être remplacés manuellement en cliquant avec le bouton droit sur une entrée dans l'éditeur de palette.

## Sauvegarder un mixCliquez sur **Ajouter à la palette** pour enregistrer le résultat actuel du mixeur en tant que **Mélange enregistré** (entrée personnalisée). Avant d'enregistrer, le système applique la **Best-Match Relocation** : il recherche dans la palette la recette optimale qui produit la même couleur finale avec le meilleur ajustement spatial sur la carte de la palette. Si une recette plus proche est trouvée, les curseurs du mixeur sauteront pour la refléter, confirmant que le système a trouvé une meilleure origine et la position de l'entrée enregistrée s'alignera sur son point visuel sur la carte.

Les mélanges enregistrés stockent leur recette complète (UID parent A/B, facteur de mélange, tonalité, chrominance) afin qu'ils puissent être reproduits exactement.

## Récupération de recette

Un simple clic sur une entrée personnalisée dans l'éditeur de palette restaure la recette de cette entrée dans le mélangeur :

- Parent A et Parent B sont rechargés.
- Les curseurs de mélange, de tonalité et de chrominance reviennent à leurs positions d'origine.
- Tout verrouillage de valeur ou serrage de bande qui était actif lors de la création est réactivé.

Cela permet de revenir facilement à une couleur et de l'ajuster davantage, ou de l'utiliser comme point de départ pour un nouveau mélange.