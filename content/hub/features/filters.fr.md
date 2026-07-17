---
title: "Filtres"
type: docs
url: "hub/features/filters"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
---

Le menu Filtres de Lumi rassemble ajustements correctifs, effets de lentille stylisés, générateurs de texture procédurale, traitements inspirés de l'impression et outils d'analyse au même endroit. L'ordre du menu est pratique plutôt qu'académique : outils de flou et d'amélioration côte à côte, effets de distorsion et d'éclairage regroupés par rendu, et générateurs de texture ou de motif réunis lorsque l'objectif est de créer du matériau source plutôt que de modifier une image existante.

Les boîtes de dialogue de filtre suivent le même flux de travail général. Préréglages, aperçu, vue fractionnée et contrôles d'opacité ou de fusion permettent d'ajuster rapidement un effet, et sur les calques le résultat peut rester un filtre non destructif modifiable au lieu d'être fusionné immédiatement. Lumi conserve aussi un historique récent d'utilisation des filtres, de sorte que répéter le dernier effet ou rouvrir la dernière boîte de dialogue fait partie du rythme normal de peinture plutôt que d'une tâche à part.

## Flou

### Gaussian Blur

Gaussian Blur est le filtre d'adoucissement standard de Lumi : un flou net et uniforme avec des contrôles de taille horizontale et verticale séparés, une gestion des bords et des options de noyau. C'est le choix généraliste pour la mise au point douce, les masques adoucis, la profondeur atmosphérique et tout flux de travail où le flou lui-même doit rester neutre.

### Pixelize

Pixelize réduit le détail en structures de blocs délibérées plutôt qu'en flou doux. Comme la boîte de dialogue expose largeur et hauteur de bloc, décalages, forme des pixels et comportement de remplissage, il sert aussi bien d'effet de censure grossière que de mosaïque contrôlable ou de traitement graphique basse résolution.

### Selective Gaussian Blur

Selective Gaussian Blur adoucit certaines régions tout en préservant autant que possible les bords marqués. Il est utile lorsqu'une image a besoin d'une texture plus calme ou d'un grain visuel réduit sans perdre les grands contours de forme qui doivent rester lisibles.

### Lens Blur

Lens Blur est l'un des filtres de flou les plus orientés illustration de Lumi. Ses contrôles tournent autour de la forme d'iris polygonale, de la courbure des lames, de l'étirement anamorphique, du renforcement des hautes lumières et d'une zone de mise au point configurable. Il se comporte moins comme un adoucisseur générique que comme un outil de profondeur de champ stylisé avec bokeh façonné.

### Tilt-shift

Tilt-shift conserve une bande de mise au point nette et contrôlable tout en floutant progressivement l'image au-dessus et en dessous. L'angle de bande, le fondu, le biais de perspective, la forme d'iris et l'accentuation miniature de la boîte de dialogue le rendent adapté aux scènes en miniature, aux vues architecturales et à toute composition où la mise au point doit se lire comme une bande conçue plutôt que comme un repère de profondeur circulaire.

### Circular Motion Blur

Circular Motion Blur étire le détail autour d'un point central, transformant les bords en traînées de rotation. C'est le choix naturel pour les sujets en rotation, l'énergie de type turbine ou les illustrations qui ont besoin d'un mouvement orbital.

### Linear Motion Blur

Linear Motion Blur étire le détail dans une direction, simulant déplacement, mouvement de caméra ou geste rapide à travers le cadre. Il est particulièrement utile lorsque le mouvement doit paraître directionnel et graphique plutôt que diffus.

### Zoom Motion Blur

Zoom Motion Blur rayonne le détail vers l'extérieur à partir d'un centre, produisant la sensation d'une course vers ou loin du spectateur. Il convient aux moments d'impact, aux lignes de vitesse et aux compositions qui demandent une énergie de zoom sans repeindre l'image entière.

## Amélioration

### High Pass

High Pass isole le contraste local fin plutôt que le changement tonal large. Avec seulement échelle et contraste à gérer, c'est un outil direct pour extraire le détail des bords, construire des superpositions nettes ou préparer des passes de netteté qui mettent l'accent sur la structure plutôt que sur la couleur.

### Noise Reduction

Noise Reduction fait l'inverse : il supprime les fines variations indésirables pour que les formes plus larges se lisent plus clairement. Il est utile lorsque du matériel numérisé, des textures compressées ou des passages surchargés doivent être simplifiés avant de poursuivre la peinture ou le filtrage.

### Sharpen

Sharpen utilise un modèle de masque flou, avec rayon, intensité et seuil qui contrôlent la force du contraste local. En pratique, il convient à la restauration de la clarté après flou, redimensionnement à l'export ou passes de finition subtiles où le détail doit ressortir sans transformer chaque pixel en bruit.

## Couleur

### Tonal Grading

Tonal Grading remappe la couleur par plage tonale plutôt qu'en remodelant le contraste ou en traçant une courbe. La luminance de chaque pixel choisit un mélange fluide de trois couleurs définies par l'utilisateur pour ombres, tons moyens et hautes lumières ; l'image conserve ainsi sa structure clair-sombre tandis que la palette se déplace. La force par région, un biais d'équilibre de type Lightroom (à gauche favorise la correction des ombres, à droite celle des hautes lumières) et la douceur des transitions contrôlent la portée de chaque couleur et la façon dont les corrections se chevauchent. Il vise l'illustration, la bande dessinée, l'art conceptuel et la photographie lorsque l'objectif est une correction ou un rendu cohérent.

## Distorsion

### Chromatic Aberration

Chromatic Aberration sépare les canaux couleur vers l'extérieur à partir d'un centre choisi, avec des contrôles pour direction radiale ou tangentielle, biais entre paires de canaux, atténuation et préservation de la luminance. Le code et la boîte de dialogue le traitent comme un outil bidirectionnel : il peut ajouter des franges de lentille stylisées pour l'énergie, ou inverser le signe pour corriger une légère aberration dans le matériau source.

### Lens Distortion

Lens Distortion remodèle l'image par une courbure en barillet ou en coussinet, des termes de bord, une compensation de zoom, des décalages centraux et un éclaircissement des coins. Il sert autant à corriger une image qui paraît optiquement courbée qu'à pousser délibérément une image vers un caractère grand-angle ou rétro.

## Éclairage

### Bloom

Bloom transforme les zones lumineuses en lueur contrôlée, avec seuil, douceur, rayon et intensité qui définissent la distance de propagation et la force avec laquelle la lumière soulève l'image. Le contrôle supplémentaire de limitation de l'exposition le rend utilisable comme effet de surbrillance plutôt que comme délavage automatique.

### Sky

Sky va au-delà d'une simple teinte ou d'un dégradé superposé : il restitue un ciel analytique à l'aide des modèles Preetham, Hosek/Wilkie ou Nishita. Comme la boîte de dialogue expose projection, angle du soleil, turbidité, densité atmosphérique, altitude, contrôles du disque solaire et exposition, elle peut produire aussi bien un simple fond clair qu'un coucher de soleil ou un crépuscule plus physiquement ancré.

### Vignette

Vignette assombrit, colore ou même efface vers les bords de l'image, avec des contrôles de forme, rayon, douceur, gamma, proportion, compression, rotation et positionnement sur le canevas. Elle sert de traitement de bord photographique classique, mais reste assez souple pour agir comme masque de cadrage ou projecteur de composition irrégulier.

## Bruit

### HSV Noise

HSV Noise randomise teinte, saturation et valeur indépendamment. Il est utile lorsqu'une image a besoin de vivacité couleur ou d'instabilité analogique sans briser complètement la structure locale.

### Hurl

Hurl est la version extrême du bruit : il remplace les pixels par des couleurs entièrement aléatoires. Il vaut mieux le voir comme une source de chaos destructeur pour le glitch, les textures usées ou les masques qui demandent une rupture agressive.

### Pick

Pick remplace chaque pixel par un voisin choisi au hasard, de sorte que l'image reste liée à sa source au lieu de devenir du pur bruit. Le résultat est une variation granulaire mélangée qui peut paraître plus organique qu'un bruit entièrement aléatoire.

### Spread

Spread disperse les pixels en les déplaçant aléatoirement dans un rayon. Il est utile lorsque l'on veut une perturbation immobile : surface brisée, bord estompé ou texture usée qui conserve encore les relations couleur de l'image source.

### Fractal

Fractal génère du bruit Perlin fractal carrelable, ce qui le rend particulièrement précieux comme source réutilisable pour masques, nuages, texture de papier, ruptures de type terrain et superpositions procédurales. Comme il carre, il peut alimenter des flux plus larges sans créer de joints visibles.

### Blue Noise Grain

Blue Noise Grain est le générateur de grain monochrome de style film et impression de Lumi. Les préréglages de taille de grain, le masquage blue-noise, le biais des tons moyens, le biais des ombres et les contrôles de graine montrent qu'il est conçu pour placer le grain de façon uniforme et contrôlable, et pas seulement pour pulvériser des taches monochromes aléatoires sur l'image.

### Risograph Grain

Risograph Grain repose sur la même logique de grain mais la transforme en effet d'impression à deux plaques. Couleurs d'encre séparées, équilibre des plaques, décalage volontaire et variation par graine en font un bon choix pour affiches, esthétique d'impression indépendante et illustrations qui doivent paraître physiquement surimprimées plutôt que numériquement parfaites.

### Halftone (FM)

Halftone (FM) crée une demi-teinte stochastique modulée en fréquence à l'aide de blue-noise ou de méthodes de seuillage associées. Avec des modes couleur monochrome, duotone et CMJN, ainsi que des contrôles de gain de points et de décorrélation des plaques, il vise une texture proche de l'impression, irrégulière et vivante, plutôt qu'une grille rigide.

## Bords

### Difference of Gaussians

Difference of Gaussians détecte les contours en soustrayant deux versions floutées de l'image. C'est un opérateur compact et utile pour cartes de contours, extraction de lignes stylisées et repérage de transitions structurelles sans s'engager dans un contour complet avec seuil.

## Morphologie

### Median

Median remplace chaque pixel par la valeur médiane de son voisinage, ce qui tend à supprimer le bruit isolé tout en préservant mieux les contours marqués qu'un simple flou. C'est un filtre de nettoyage pratique pour aplanir les petites agitations visuelles sans adoucir immédiatement toute l'image.

### Dilate

Dilate agrandit les régions claires vers l'extérieur en utilisant la même logique de voisinage sensible à la forme. En termes de création d'image, il peut épaissir les marques claires, agrandir les formes lumineuses ou combler de petits intervalles sombres.

### Erode

Erode fait le mouvement complémentaire, en agrandissant les régions sombres et en réduisant les zones claires. Il est utile pour affiner les détails clairs, agrandir les masses sombres ou resserrer masques et formes graphiques.

## Motifs

### Checkerboard

Checkerboard génère un motif régulier de tuiles alternées. Sa simplicité le rend utile pour tester la transparence, construire des masques, poser des arrière-plans graphiques ou créer du matériau source géométrique propre.

### Grid

Grid trace des divisions horizontales et verticales répétées, utile pour guides de mise en page, fonds de conception, illustration technique et masquage procédural. Comme il est généré comme filtre, l'espacement et l'apparence peuvent être ajustés sans construire le motif à la main.

### Voronoi

Voronoi génère une texture cellulaire carrelable à partir de points de graine, avec des contrôles pour type de caractéristique, métrique de distance, caractère aléatoire, détail fractal et habillage sans couture. En pratique, il peut aller de structures de cellules nettes et fissurées à des motifs plus organiques de pierre, peau, carte ou réseau abstrait.

### Wave

Wave produit des motifs en bandes ou en anneaux façonnés par profil de forme d'onde, disposition géométrique, distorsion, détail fractal et décalage de phase. C'est plus qu'un simple outil de rayures : il peut générer des ondulations contrôlées, des bandes topographiques, des graphiques de type moiré ou des champs de motifs concentriques bruyants.

### Halftone (AM)

Halftone (AM) applique un trame classique à modulation d'amplitude, avec contrôles de fréquence, forme de point, netteté, mode couleur et angles CMJN pour une structure d'impression de type rosette. Comparé à la demi-teinte FM, c'est l'option la plus ordonnée et reconnaissablement mécanique lorsque le rendu visé est journal, lithographie offset ou géométrie de trame volontairement visible.
