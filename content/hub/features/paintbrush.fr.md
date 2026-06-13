---
title: "Outil Pinceau"
type: docs
---
L'outil Pinceau est l'instrument de peinture principal de Lumi : une manière réactive et expressive de dessiner, peindre, ombrer, texturer et créer des marques directement sur la toile. Il est conçu pour être immédiat tout en donnant aux artistes la possibilité de façonner le comportement d'un trait.

Plutôt que d’être un simple pinceau fixe, il agit comme un système de peinture. La forme, la texture, le mouvement, la pression, le timing et la couleur du pinceau peuvent tous contribuer à la marque finale, ce qui la rend adaptée aux travaux de lignes épurées, à la peinture douce, aux effets sur supports secs, aux traits calligraphiques, aux textures dispersées et aux formations de pinceaux à plusieurs têtes.

![brush-tool](/images/screens/brush-tool.jpg)

## Marques de pinceau expressives

Les pinceaux peuvent être basés sur des tampons bitmap, des formes procédurales ou des sources animées basées sur des images. Cela permet à un trait de s'étendre d'une simple marque ronde et douce à une tête de brosse richement texturée ou évolutive. Le même moteur de peinture peut prendre en charge un dessin précis, une accumulation picturale, des marques décoratives et une rupture de style naturel.

Lorsqu'un pinceau devient visuellement complexe, l'aperçu peut rester simplifié afin que la peinture reste réactive et facile à lire.

![tool-setup](/images/screens/tool-setup.jpg)


## Dynamique et réponse d'entrée

L'outil Pinceau répond aux entrées en direct telles que la pression du stylet, la vitesse, la direction, l'inclinaison et d'autres valeurs du contrôleur. Ces signaux peuvent influencer le trait visible de plusieurs manières : l'épaisseur, l'opacité, l'angle, la réponse de la texture, le comportement des couleurs, l'espacement et d'autres qualités peuvent tous changer à mesure que la main bouge.

Cela fait que le pinceau ressemble moins à un motif estampé qu'à un instrument de dessin physique. Un toucher léger peut produire des marques délicates, un mouvement plus rapide peut ouvrir une texture ou une forme, et un comportement sensible à la direction peut aider les traits à suivre le geste de la main.

![dynamics](/images/screens/dynamics.jpg)

## Comportement de l'AVC

Les mouvements peuvent être directs et immédiats, ou ils peuvent être assistés par un lissage et une stabilisation. Ces fonctionnalités aident à réduire les vibrations indésirables, à adoucir les changements brusques et à donner l'impression que les mouvements plus longs sont plus contrôlés sans supprimer le caractère de l'entrée de l'artiste.

Le pinceau prend également en charge différentes approches d’accumulation de peinture. Il peut se comporter comme un trait continu, accumuler des touches répétées ou émettre des marques au fil du temps pendant que le pointeur est maintenu en place. Cette flexibilité le rend utile à la fois pour le travail en ligne délibéré et pour la construction tonale plus lente.

Pour les marques calligraphiques ou semblables à de l'encre, le pinceau peut générer un trait de forme plus continu plutôt que de s'appuyer uniquement sur des tampons répétés. Cela produit des formes fluides, semblables à des rubans, qui répondent naturellement au geste et à la vitesse.

![stroke](/images/screens/stroke.jpg)

## Capture de trait et rendu simulé

Le pinceau peut capturer un petit échantillon de la façon dont un préréglage est normalement dessiné à la main, puis utiliser ce profil lors du rendu de traits définis par la géométrie plutôt que par le mouvement en direct. Les lignes droites Maj-clic, les tracés tracés et les sélections tracées peuvent tous utiliser le modèle de pression et de vitesse capturé par le préréglage d'outil actif au lieu de se comporter comme une ligne mécanique plate.

Cela permet de garder les traits construits plus proches du caractère du pinceau. Une ligne tracée à partir d'un chemin peut commencer doucement, augmenter la pression, diminuer ou varier la réponse en vitesse de la même manière que le coup de main échantillonné, tout en suivant la forme exacte du chemin, du bord de sélection ou du geste en ligne droite.

## Post-traitementLe pinceau peut enregistrer un trait pendant que vous le dessinez, puis rejouer ce geste capturé une fois que vous décollez, affinant ainsi le tracé avant que la marque finale ne soit tracée. Vous pouvez dessiner librement tout en obtenant une direction plus claire, des angles plus nets ou une structure plus délibérée sans avoir à dessiner avec une précision mécanique.

Cela ouvre des hachures et des marques de construction lignées qui s'alignent sur des angles nets tout en conservant la longueur et le caractère dessinés à la main, des traits de ruban stables à l'inclinaison et une relecture tenant compte des coins qui traite différemment les virages et les lignes droites. Les pinceaux multi-têtes peuvent partager un tracé corrigé tandis que chaque tête conserve sa propre variation, et la dynamique peut toujours façonner le trait le long de sa courbe finale pendant la relecture. Le post-traitement s'applique aux traits dessinés plutôt qu'à l'émission continue de l'aérographe.

## Couleur et texture

Les coups de pinceau peuvent utiliser la couleur de peinture active, réagir aux dégradés ou faire varier la couleur grâce à la dynamique. La gestion de la texture permet au pinceau de passer d'une couverture solide à des marques brisées qui effleurent la surface, ce qui est utile pour les effets de pinceau sec, le grain et les ombrages expressifs.

Étant donné que la couleur et la texture peuvent faire partie du même système dynamique que la forme et l’opacité, un seul trait peut évoluer à mesure qu’il se déplace sur la toile au lieu de rester visuellement uniforme.

## Têtes et formations de brosse

L'outil Pinceau peut peindre avec plusieurs têtes à la fois. Plusieurs têtes peuvent être disposées autour du tracé du trait pour créer des marques de plume, des traits en éventail, un comportement semblable à des poils, des motifs de pulvérisation, des formations texturées ou des hachures structurées.

Ces têtes peuvent suivre la direction du déplacement, varier les unes des autres et se disperser de manière à donner au trait une sensation organique plutôt que mécaniquement répétée. Ceci est particulièrement utile pour les pinceaux naturels, les traits décoratifs, le feuillage, la fourrure, les hachures et autres marques bénéficiant d’une irrégularité contrôlée.

![brush-heads](/images/screens/brush-heads.jpg)

## Chargement des pinceaux et ramassage de la peinture

Le pinceau peut également simuler la quantité de peinture ou de matériau actuellement transportée sur le pinceau. Au fur et à mesure qu'un trait se poursuit, cette charge peut diminuer progressivement, laissant les marques devenir plus claires, plus sèches, plus fines, plus rugueuses ou autrement plus fragmentées en fonction de la manière dont la dynamique du pinceau est définie.

La charge peut être réintroduite entre les coups, maintenue à un niveau choisi ou utilisée comme signal de contrôle en direct pour d'autres comportements de brosse. Cela permet de créer des pinceaux qui ressemblent davantage à de vrais supports : mouillés au début d'un trait, progressivement épuisés au fil de la distance, puis plongés à nouveau pour le passage suivant.

![material-state](/images/screens/material-state.jpg)

## Contact avec la surface du pinceau

Le pinceau peut également simuler une perte intermittente de contact avec la surface à peindre – les marques cassées qui apparaissent lorsqu'un crayon, un fusain, un pinceau sec ou un marqueur partiellement épuisé n'entre que partiellement en contact avec le papier.

Lorsque la simulation de contact est activée, la brosse est soit en contact, soit levée. Au contact, les marques se déposent normalement. Lors du levage, aucun matériau ne se dépose et la course laisse un espace dont la longueur est choisie aléatoirement entre une distance minimale et maximale. La transition est binaire : l'effet ne modifie pas l'opacité, la taille, la dureté, l'espacement ou le flux, mais uniquement si la peinture est déposée.La facilité avec laquelle le contact est perdu est déterminée par un seuil de contact, la pression du stylet et éventuellement la charge de la brosse. Des valeurs de seuil plus élevées rendent les pauses plus fréquentes. La pression agit comme une force stabilisatrice : une pression légère augmente le risque de perte de contact, tandis qu'une pression ferme rend le coup plus susceptible de rester vers le bas. Lorsque la charge de brosse est activée, une faible charge peut rendre la marque plus brisée et une charge élevée peut aider à maintenir le contact, comme un outil qui transporte encore suffisamment de matériau pour adhérer à la surface.

La perte est évaluée à partir de la distance parcourue par le trait plutôt que du nombre de touches, de sorte que les pinceaux avec un espacement dense ou clairsemé se comportent de manière cohérente. La fonctionnalité fonctionne à la fois avec un rendu basé sur des tampons et avec un rendu calligraphique, produisant des espaces cohérents le long du trait plutôt que des touches sautées isolées.

## Animation et variation

Les sources de pinceaux animées peuvent changer de cadre à mesure que le trait progresse, donnant aux pinceaux une impression de mouvement et de variété. La randomisation et la variation par trait peuvent empêcher les marques répétées de paraître identiques, tandis qu'un ensemencement stable peut préserver un caractère cohérent lorsque la répétabilité est nécessaire.

Ces comportements sont utiles pour les pinceaux qui doivent paraître vivants : les poils se déplacent d'un trait, les tampons texturés changent subtilement au fil du temps ou les outils à plusieurs têtes où chaque tête a sa propre personnalité.

## Flux de travail axé sur l'artiste

L'outil Pinceau est organisé de manière à ce que les décisions de peinture courantes restent à portée de main, tandis que les choix de configuration moins fréquents restent à l'écart. L'intention est de garder l'outil accessible pendant la peinture tout en prenant en charge une personnalisation approfondie de la conception des pinceaux.

Dans l'ensemble, le pinceau est conçu pour couvrir à la fois la peinture quotidienne et la création de marques spécialisées : des croquis rapides, des illustrations soignées, un rendu texturé, un travail à l'encre expressif et des effets de pinceau procéduraux complexes partagent tous la même base flexible.