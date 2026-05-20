---
title: "Filtres"
type: docs
---
Le menu Filtres de Lumi rassemble des ajustements correctifs, des effets de lentille stylisés, des générateurs de texture procédurale, des traitements inspirés de l'impression et des outils d'analyse en un seul endroit. L'ordre du menu est pratique plutôt qu'académique : les outils de flou et d'amélioration sont placés les uns à côté des autres, les effets de distorsion et d'éclairage sont regroupés par aspect, et les générateurs de textures ou de motifs sont conservés ensemble lorsque l'objectif est de créer du matériel source plutôt que de modifier une image existante.

Les boîtes de dialogue de filtre suivent le même flux de travail général. Les préréglages, l'aperçu, la vue fractionnée et les commandes d'opacité ou de fusion permettent d'ajuster rapidement un effet, et sur les calques, le résultat peut rester sous la forme d'un filtre non destructif modifiable au lieu d'être fusionné immédiatement. Lumi conserve également un historique récent de l'utilisation des filtres, donc répéter le dernier effet ou rouvrir la dernière boîte de dialogue fait partie du rythme normal de peinture plutôt qu'une tâche distincte.

## Flou

### Flou gaussien

Le flou gaussien est le filtre d'adoucissement standard de Lumi : un flou net et uniforme avec des contrôles de taille horizontaux et verticaux séparés, une gestion des bords et des options de noyau. Il s'agit du choix général pour la mise au point douce, les masques adoucis, la profondeur atmosphérique et tout flux de travail où le flou lui-même doit rester neutre.

### Pixeliser

Pixelize réduit les détails en structures de blocs délibérées au lieu d'un flou doux. Étant donné que la boîte de dialogue expose la largeur et la hauteur du bloc, les décalages, la forme des pixels et le comportement de remplissage, elle fonctionne à la fois comme un effet de censure grossière et comme une mosaïque contrôlable ou un traitement graphique basse résolution.

### Flou gaussien sélectif

Le flou gaussien sélectif adoucit les régions tout en essayant de préserver les bords plus forts. Ceci est utile lorsqu'une image a besoin d'une texture plus calme ou d'un broutage réduit sans perdre les limites de forme plus larges qui doivent encore être lues clairement.

### Flou de l'objectif

Lens Blur est l'un des filtres de flou les plus axés sur l'illustration de Lumi. Ses commandes sont construites autour de la forme de l'iris polygonal, de la courbure de la lame, de l'étirement anamorphique, de l'amélioration des hautes lumières et d'une région de mise au point configurable. Il se comporte donc moins comme un adoucisseur générique que comme un outil de profondeur de champ stylisé avec un bokeh façonné.

### Inclinaison-décalage

L'inclinaison et le décalage maintiennent une bande de mise au point contrôlable nette tout en rendant progressivement l'image floue au-dessus et en dessous. L'angle de bande, la plume, le biais de perspective, la forme de l'iris et l'accentuation miniature du dialogue le rendent bien adapté aux scènes miniatures, aux vues architecturales et à toute composition où la mise au point doit être interprétée comme une bande conçue plutôt que comme un indicateur de profondeur circulaire.

### Flou de mouvement circulaire

Le flou de mouvement circulaire étale les détails autour d'un point central, transformant les bords en traînées de rotation. C'est le choix naturel pour les sujets en rotation, l'énergie semblable à une turbine ou les illustrations nécessitant une sensation de mouvement orbital.

### Flou de mouvement linéaire

Le flou de mouvement linéaire étend les détails dans une direction, simulant un voyage, un mouvement de caméra ou un geste rapide sur le cadre. Ceci est particulièrement utile lorsque le mouvement doit être directionnel et graphique plutôt que diffus.

### Zoom sur le flou de mouvement

Zoom Motion Blur rayonne les détails vers l'extérieur à partir d'un centre, produisant la sensation d'une précipitation vers ou loin du spectateur. Cela fonctionne bien pour les moments d'impact, les lignes rapides et les compositions qui nécessitent une énergie de zoom de caméra sans repeindre l'image entière.

## Améliorer

### Passe-hautLe passe-haut isole un contraste local fin plutôt qu'un changement de tonalité large. Avec uniquement l'échelle et le contraste à gérer, il s'agit d'un outil simple pour extraire les détails des bords, créer des superpositions nettes ou préparer des passes de netteté qui doivent mettre davantage l'accent sur la structure que sur la couleur.

### Réduction du bruit

La réduction du bruit est la démarche inverse : elle supprime les fines variations indésirables afin que les formulaires plus grands soient lus plus clairement. Ceci est utile lorsque le matériel numérisé, les textures compressées ou les passages surchargés doivent être simplifiés avant de poursuivre la peinture ou le filtrage.

### Aiguiser

Sharpen utilise un modèle de masque flou, avec un rayon, une quantité et un seuil contrôlant la force avec laquelle le contraste local est poussé. En pratique, cela le rend adapté à la restauration de la clarté après un flou, un redimensionnement à l'exportation ou des passes de finition subtiles où les détails doivent apparaître sans transformer chaque pixel en bruit.

## Distorsion

### Aberration chromatique

L'aberration chromatique sépare les canaux de couleur vers l'extérieur d'un centre choisi, avec des contrôles pour la direction radiale ou tangentielle, la polarisation entre les paires de canaux, l'atténuation et la préservation de la luminance. Le code et la boîte de dialogue le traitent tous deux comme un outil bidirectionnel : il peut ajouter des franges de lentilles stylisées pour l'énergie, ou inverser le signe pour corriger une légère aberration dans le matériau source.

### Distorsion de l'objectif

La distorsion de l'objectif remodèle l'image grâce à la courbure en barillet ou en coussinet, aux termes de contour, à la compensation du zoom, aux décalages centraux et à l'éclaircissement des coins. Cela le rend utile à la fois pour corriger une image qui semble optiquement courbée et pour en pousser délibérément une vers un caractère grand angle ou rétro.

## Éclairage

### Floraison

Bloom transforme les zones lumineuses en lueur contrôlée, avec un seuil, une douceur, un rayon et une force définissant la distance dans laquelle la lumière se propage et la force avec laquelle elle soulève l'image. Le contrôle supplémentaire de limitation de l'exposition le rend utilisable comme effet de surbrillance plutôt que comme effet de délavage automatique.

### Ciel

Sky est plus qu'une superposition de teintes ou de dégradés : il restitue un ciel analytique à l'aide des modèles Preetham, Hosek/Wilkie ou Nishita. Étant donné que la boîte de dialogue expose la projection, l'angle du soleil, la turbidité, la densité atmosphérique, l'altitude, les commandes du disque solaire et l'exposition, elle peut créer n'importe quoi, depuis une simple toile de fond claire jusqu'à un coucher de soleil ou un ciel crépusculaire plus physiquement ancré.

### Vignettes

La vignette s'assombrit, colore ou même efface vers les bords de l'image, avec des commandes de forme, de rayon, de douceur, de gamma, de proportion, de compression, de rotation et de positionnement sur la toile. Il fonctionne comme un traitement de bord photographique classique, mais il est suffisamment flexible pour agir comme un masque de cadrage ou un projecteur de composition irrégulière.

## Bruit

### Bruit HSV

HSV Noise randomise la teinte, la saturation et la valeur indépendamment. Cela le rend utile lorsqu'une image a besoin de vivacité des couleurs ou d'instabilité analogique sans briser complètement la structure locale.

### Lancement

Hurl est la version extrême du bruit : il remplace les pixels par des couleurs complètement aléatoires. Il est préférable de le considérer comme une source de chaos destructeur pour les travaux de pépins, les textures en détresse ou les masques qui nécessitent une dissolution agressive.

### Choisir

Pick remplace chaque pixel par un voisin choisi au hasard, de sorte que l'image reste liée à sa source au lieu de devenir purement statique. Le résultat est une variation granulaire mélangée qui peut sembler plus organique qu’un bruit entièrement aléatoire.

### PropagéRépartissez les pixels en les déplaçant de manière aléatoire dans un rayon. C'est utile lorsque vous souhaitez une perturbation immobile : une surface brisée, un bord maculé ou une texture vieillie qui porte toujours les relations de couleurs de l'image source.

### Fractale

Fractal génère du bruit Perlin fractal carrelable, ce qui le rend particulièrement précieux en tant que source réutilisable pour les masques, les nuages, la texture du papier, la rupture de type terrain et les superpositions procédurales. Grâce à sa mosaïque, il peut alimenter des flux de travail plus importants sans créer de joints évidents.

### Grain de bruit bleu

Blue Noise Grain est le générateur de grains monochromes de style film et impression de Lumi. Les préréglages de taille de grain, le masquage du bruit bleu, la polarisation des tons moyens, la polarisation des ombres et les contrôles de graine de la boîte de dialogue montrent qu'elle est conçue pour placer le grain de manière uniforme et contrôlable, et pas seulement pour pulvériser des taches monochromes aléatoires sur l'image.

### Grain risographe

Risograph Grain s'appuie sur la même logique de grain mais la transforme en un effet d'impression sur deux plaques. Les couleurs d'encre séparées, l'équilibre des plaques, les erreurs de repérage délibérées et les variations prédéfinies en font un bon choix pour les travaux d'affiches, l'esthétique des impressions indépendantes et les illustrations qui devraient sembler physiquement surimprimées plutôt que numériquement parfaites.

### Demi-teintes (FM)

Halftone (FM) crée une demi-teinte stochastique modulée en fréquence à l'aide de méthodes de bruit bleu ou de seuillage associées. Avec des modes de couleur monochrome, bicolore et CMJN, ainsi que des commandes de gain de points et de décorrélation des plaques, il vise à obtenir une texture semblable à une impression qui reste irrégulière et vivante au lieu de tomber dans une grille rigide.

## Bords

### Différence des Gaussiennes

La différence de gaussiennes détecte les contours en soustrayant deux versions floues de l'image l'une de l'autre. Il s'agit d'un opérateur compact et utile pour les cartes de contours, l'extraction de lignes stylisées et la recherche de transitions structurelles sans s'engager dans un contour complet avec seuil.

## Morphologie

### Médiane

Median remplace chaque pixel par la valeur médiane de son voisinage, ce qui tend à supprimer le bruit isolé tout en préservant mieux les limites plus fortes qu'un simple flou. Il s'agit d'un filtre de nettoyage pratique permettant d'aplatir les petits bavardages visuels sans adoucir immédiatement l'ensemble de l'image.

### Dilater

Dilate agrandit les régions plus claires vers l’extérieur en utilisant la même logique de voisinage sensible aux formes. En termes de création d’images, il peut épaissir les marques lumineuses, agrandir les formes claires ou combler les petits espaces sombres.

### Éroder

Erode effectue le mouvement complémentaire, en agrandissant les régions les plus sombres et en retirant les plus claires. Il est utile pour affiner les détails clairs, agrandir les masses sombres ou resserrer les masques et les formes graphiques.

## Modèle

### Damier

Le damier génère un motif de tuiles alterné régulier. C'est simple, mais cette simplicité le rend utile pour tester la transparence, créer des masques, bloquer des arrière-plans graphiques ou créer un matériau source géométrique propre.

### Grille

Grid dessine des divisions horizontales et verticales répétées, ce qui le rend utile pour les guides de mise en page, les arrière-plans de conception, les illustrations techniques et le masquage procédural. Puisqu'il est généré sous forme de filtre, l'espacement et l'apparence peuvent être ajustés sans créer manuellement le motif.

### Voronej

Voronoi génère une texture cellulaire carrelable à partir de points prédéfinis, avec des contrôles pour le type de caractéristique, la métrique de distance, le caractère aléatoire, les détails fractals et l'habillage transparent. En pratique, il peut passer de structures de cellules fissurées propres à des motifs de pierre, de peau, de carte ou de réseau plus organiques.

### VagueWave produit des motifs en bandes ou en anneaux façonnés par le profil de forme d'onde, la disposition géométrique, la distorsion, les détails fractals et le décalage de phase. Cela en fait plus qu'un simple outil de bande : il peut générer des ondulations contrôlées, des bandes topographiques, des graphiques de type moiré ou des champs de motifs concentriques bruyants.

### Demi-teintes (AM)

Halftone (AM) applique un écran de points classique à modulation d'amplitude, avec des commandes de fréquence, de forme de point, de netteté, de mode couleur et d'angle CMJN pour une structure d'impression de style rosette. Par rapport aux demi-teintes FM, il s'agit de l'option mécanique la plus ordonnée et reconnaissable lorsque l'aspect souhaité est du papier journal, de la lithographie offset ou une géométrie d'écran délibérément visible.