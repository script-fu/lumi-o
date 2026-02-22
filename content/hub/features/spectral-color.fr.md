---
title: "Mélange de couleurs spectrales"
type: docs
---
Le système de palette de Lumi utilise un modèle de couleur spectrale pour simuler la manière dont les vrais pigments se mélangent. L’objectif est de faire en sorte que l’expérience de création et de sélection de couleurs à partir d’une palette numérique se comporte comme un mélange de peintures physiques. Une fois qu’une couleur est appliquée à la toile, elle est RVB standard.

## Que signifie le mélange spectral

Le mélange RVB traditionnel est additif : le mélange de deux valeurs RVB les fait atteindre un point médian. Le mélange de pigments est soustractif : chaque pigment absorbe certaines longueurs d'onde et leur effet combiné est plus sombre et change souvent de teinte.

Lumi modélise cela en utilisant une représentation de réflectance spectrale à 10 bandes pour les couleurs de la palette, plutôt que RVB.

Cela produit des résultats semblables à ceux de la peinture : le mélange du bleu et du jaune produit du vert et non du gris. Le mélange de deux couleurs saturées produit une couleur qui vire au neutre comme le font les pigments physiques.

Le calcul spectral s'exécute pendant la construction de la palette, lors de la génération d'entrées de palette secondaire et tertiaire et lorsque le mélangeur de palette mélange deux couleurs parentes. La couleur résultante est convertie en RVB linéaire pour l'affichage et la peinture.

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

### Oranges et jaunes| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Orange Pyrrole | PO73 | Rouge (écarlate) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Orange de cadmium | PO20 | Jaune (Corps) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Jaune de cadmium | PY35 | Jaune (Corps) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Jaune de cadmium pâle | PY35 : Pâle | Jaune (cadmium pâle) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Jaune citron | PY3 | Jaune (Citron) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Nickel Azo Jaune | PY150 | Jaune (milieu) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Or vert | PY129 | Jaune-Vert (Or) |

### Couleurs de la Terre

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Sienne brûlée | PBr7 : Brûlé | Terre (Brun Rouge) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ombre brûlée | PBr7:Ombre | Terre (Neutre) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Sienne crue | PBr7:Brut | Terre (jaune brun) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ocre jaune | PY42 | Terre (Jaune) |

### Verts

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Vert de phtalo (YS) | PG36 | Vert (teinte jaune phtalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridien | PG18 | Vert (Viridien) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Vert (Terre Cool) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Vert (BS) | PG7 | Vert (nuance bleu phtalo) |### Bleus et Cyans

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lumière turquoise cobalt | PG50 | Cyan (minéral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bleu céruléen | PB35 | Cyan (minéral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo Turquoise | PB16 | Bleu (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bleu cobalt | PB28 | Bleu (Violet-Maigre) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Bleu Winsor | PB15 | Bleu (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Outremer | PB29 | Bleu (Violet-Maigre) |

### Violettes, Magentas et Rouges

| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violette Brillante | PV23 | Violette (Dioxazine) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Roses permanentes | PV19:Rose | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinacridone Magenta | PV19:Magenta | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Alizarine permanente pourpre | PV19 : Pourpre | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violette de Pérylène | PV29 | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pérylène Marron | PR179 | Rouge (cramoisi) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rouge Pyrrole | PR254 | Rouge (écarlate) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Feu Rouge Pyrrole | PR255 | Rouge (lumière pyrrole) |

### Noirs et Blancs| Échantillon | Nom | Code CI | Famille |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Mars Noir (Chaud) | PBk11 | Noir (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Vert pérylène | PBk31 | Noir (vert pérylène) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Noir ivoire (Froid) | PBk9 | Noir (ivoire) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lampe noire (neutre) | PBk7 | Noir (Lampe) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanc de titane (chaud) | PW6 : Chaud | Blanc (Titane Chaud) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanc de titane (neutre) | PW6 | Blanc (Titane Neutre) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanc de zinc (froid) | PW4 | Blanc (Zinc Cool) |

### Contrôler les gris

Les gris de contrôle sont des neutralisants standardisés utilisés pour désaturer les mélanges de manière prévisible.

| Échantillon | Nom | Code CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris chaud | N_CHAUD |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris neutre | N_NEUTRE |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris froid | N_COOL |

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

## Les pixels du canevas sont RVBLe système spectral fonctionne entièrement dans la construction de palettes et la sélection de couleurs. Lorsqu’un coup de pinceau est appliqué, la couleur de premier plan – déjà convertie en RVB linéaire – est ce qui est peint. Le canevas stocke les données de pixels RVB standard.

Le mélange spectral améliore l'expérience de création d'une palette et de choix des couleurs d'une manière cohérente avec le comportement physique des pigments, sans modifier la façon dont les données d'image sont stockées ou composées.