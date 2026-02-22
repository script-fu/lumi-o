---
title: "Mélange de couleurs spectrales"
type: docs
---
Le système de palette de Lumi utilise un modèle de couleur spectrale pour simuler la manière dont les vrais pigments se mélangent. L’objectif est de faire en sorte que l’expérience de création et de sélection de couleurs à partir d’une palette numérique se comporte comme un mélange de peintures physiques. Une fois qu’une couleur est appliquée à la toile, elle est RVB standard.

## Que signifie le mélange spectral

Le mélange RVB traditionnel est additif : le mélange de deux valeurs RVB les fait atteindre un point médian. Le mélange de pigments est soustractif : chaque pigment absorbe certaines longueurs d'onde et leur effet combiné est plus sombre et change souvent de teinte.

Lumi modélise cela en utilisant une représentation de réflectance spectrale à 10 bandes pour les couleurs de la palette, plutôt que RVB.

Cela produit des résultats semblables à ceux de la peinture : le mélange du bleu et du jaune produit du vert et non du gris. Le mélange de deux couleurs saturées produit une couleur qui vire au neutre comme le font les pigments physiques.

Le calcul spectral s'exécute pendant la construction de la palette, lors de la génération des entrées de palette secondaire et tertiaire et lorsque le mélangeur de palette mélange deux couleurs parentes. La couleur résultante est convertie en RVB linéaire pour l'affichage et la peinture.

## Profils de pigments

Les entrées de palette peuvent être basées sur des données réelles sur les pigments à l'aide des **codes Colour Index (CI)**. Chaque famille de pigments CI présente un biais spectral caractéristique qui influence la manière dont elle se mélange.

| Rôle des pigments | Comportement de mélange | Exemple |
| :--- | :--- | :--- |
| **Primaire** | Chroma élevé, secondaires propres | PY3 (jaune citron), PR122 (magenta) |
| **Corps** | Ton de masse opaque et fort, vire à l'olive dans les mélanges verts | PY35 (jaune de cadmium), PR108 (rouge de cadmium) |
| **Neutraliseur** | Désature rapidement et met en sourdine | PBk11 (Mars Black), PBr7 (Sienne) |
| **Ancre Chroma** | Haut pouvoir colorant, domine les mélanges | PB29 (bleu outremer), PG7 (vert phtalo) |

L'ajout de primaires avec des codes CI à une palette donne au moteur de mélange un biais spectral précis pour ces couleurs, de sorte que les mélanges secondaires et tertiaires générés reflètent le comportement de mélange réel.

## LumiPigments

La palette Master est livrée avec les pigments suivants. Les échantillons montrent l’apparence masstone typique de chaque pigment (pleine résistance, non diluée).

### Oranges et jaunes

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Orange Pyrrole | PO73 | Rouge (écarlate) |
| {{< swatch "243,114,64" >}} | Orange de cadmium | PO20 | Jaune (Corps) |
| {{< swatch "240,180,80" >}} | Jaune de cadmium | PY35 | Jaune (Corps) |
| {{< swatch "245,210,25" >}} | Jaune de cadmium pâle | PY35 : Pâle | Jaune (cadmium pâle) |
| {{< swatch "250,230,5" >}} | Jaune citron | PY3 | Jaune (Citron) |
| {{< swatch "225,155,10" >}} | Nickel Azo Jaune | PY150 | Jaune (milieu) |
| {{< swatch "180,175,45" >}} | Or vert | PY129 | Jaune-Vert (Or) |

### Couleurs de la Terre

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Sienne brûlée | PBr7 : Brûlé | Terre (Brun Rouge) |
| {{< swatch "117,66,0" >}} | Ombre brûlée | PBr7:Ombre | Terre (Neutre) |
| {{< swatch "205,68,35" >}} | Sienne crue | PBr7:Brut | Terre (jaune brun) |
| {{< swatch "187,124,25" >}} | Ocre jaune | PY42 | Terre (Jaune) |

### Verts

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Vert de phtalo (YS) | PG36 | Vert (teinte jaune phtalo) |
| {{< swatch "64,130,109" >}} | Viridien | PG18 | Vert (Viridien) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Vert (Terre Cool) |
| {{< swatch "0,110,100" >}} | Winsor Vert (BS) | PG7 | Vert (nuance bleu phtalo) |

### Bleus et Cyans

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Lumière turquoise cobalt | PG50 | Cyan (minéral) |
| {{< swatch "0,148,214" >}} | Bleu céruléen | PB35 | Cyan (minéral) |
| {{< swatch "0,100,110" >}} | Phthalo Turquoise | PB16 | Bleu (Phthalo) |
| {{< swatch "0,123,194" >}} | Bleu cobalt | PB28 | Bleu (Violet-Maigre) |
| {{< swatch "0,75,115" >}} | Bleu Winsor | PB15 | Bleu (Phthalo) |
| {{< swatch "27,63,148" >}} | Outremer | PB29 | Bleu (Violet-Maigre) |

### Violettes, Magentas et Rouges

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Violette Brillante | PV23 | Violette (Dioxazine) |
| {{< swatch "230,90,180" >}} | Roses permanentes | PV19:Rose | Magenta (Quinacridone) |
| {{< swatch "190,40,120" >}} | Quinacridone Magenta | PV19:Magenta | Magenta (Quinacridone) |
| {{< swatch "160,30,65" >}} | Alizarine permanente pourpre | PV19 : Pourpre | Magenta (Quinacridone) |
| {{< swatch "120,35,65" >}} | Violette de Pérylène | PV29 | Magenta (Quinacridone) |
| {{< swatch "135,10,45" >}} | Pérylène Marron | PR179 | Rouge (cramoisi) |
| {{< swatch "215,30,60" >}} | Rouge Pyrrole | PR254 | Rouge (écarlate) |
| {{< swatch "225,55,65" >}} | Feu Rouge Pyrrole | PR255 | Rouge (lumière pyrrole) |

### Noirs et Blancs

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Mars Noir (Chaud) | PBk11 | Noir (Mars) |
| {{< swatch "18,28,12" >}} | Vert pérylène | PBk31 | Noir (vert pérylène) |
| {{< swatch "10,18,19" >}} | Noir ivoire (Froid) | PBk9 | Noir (ivoire) |
| {{< swatch "18,18,18" >}} | Lampe noire (neutre) | PBk7 | Noir (Lampe) |
| {{< swatch "255,249,235" >}} | Blanc de titane (chaud) | PW6 : Chaud | Blanc (Titane Chaud) |
| {{< swatch "255,255,255" >}} | Blanc de titane (neutre) | PW6 | Blanc (Titane Neutre) |
| {{< swatch "245,250,255" >}} | Blanc de zinc (froid) | PW4 | Blanc (Zinc Cool) |

### Contrôler les gris

Les gris de contrôle sont des neutralisants standardisés utilisés pour désaturer les mélanges de manière prévisible.

| Échantillon | Nom | Code CI |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Gris chaud | N_CHAUD |
| {{< swatch "128,128,128" >}} | Gris neutre | N_NEUTRE |
| {{< swatch "120,128,135" >}} | Gris froid | N_COOL |

## La carte des palettes

La carte de palette visualise la palette active sous la forme d'une roue de teinte : 36 secteurs de teinte (pas de 10°) × 15 cellules de luminosité. Lorsque des primaires sont ajoutées, le système génère des mélanges secondaires et tertiaires et les place aux positions appropriées sur la carte.

Cliquer sur une cellule sélectionne une couleur comme premier plan. Maj-clic l'affecte en tant que point de terminaison parent dans le mélangeur de palettes.

## Le mélangeur de palettes

Le Palette Mixer dérive de nouvelles couleurs à partir de deux entrées parentes à l'aide d'un pipeline fixe en trois étapes :

1. **Mélange** : WGM spectral entre le parent A (CCW) et le parent B (CW).
2. **Chroma** : Mélangez vers le spectre neutre de la palette, réduisant ainsi la saturation.
3. **Ton** : Mélangez vers le mélange du blanc ou le mélange du noir, en ajustant la luminosité.

Le ton est appliqué en dernier afin que les ajustements de luminosité ne soient pas dilués par les changements de chrominance. Les commandes Value Lock et Band Clamp contraignent les résultats à un niveau de luminosité ou une bande de valeurs spécifique.

Les couleurs mélangées peuvent être enregistrées dans la palette en tant qu'entrées **Personnalisées**, stockant la recette complète (UID parents, facteur de mélange, tonalité, valeurs de chrominance) pour une récupération ultérieure.

## Les pixels du canevas sont RVB

Le système spectral fonctionne entièrement dans la construction de palettes et la sélection de couleurs. Lorsqu'un coup de pinceau est appliqué, la couleur de premier plan (déjà convertie en RVB linéaire) est ce qui est peint. Le canevas stocke les données de pixels RVB standard.Le mélange spectral améliore l'expérience de création d'une palette et de choix des couleurs d'une manière cohérente avec le comportement physique des pigments, sans modifier la façon dont les données d'image sont stockées ou composées.