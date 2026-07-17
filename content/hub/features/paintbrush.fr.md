---
title: "Outil Pinceau"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
---

L'outil Pinceau est l'instrument de peinture principal de Lumi : une façon réactive et expressive de dessiner, peindre, ombrer, texturer et poser des marques directement sur le canevas. Il est conçu pour rester immédiat tout en laissant aux artistes la possibilité de façonner le comportement d'un trait.

Plutôt qu'un pinceau fixe unique, il agit comme un système de peinture. Forme, texture, mouvement, pression, timing et couleur peuvent tous contribuer à la marque finale, ce qui le rend adapté aux lignes nettes, à la peinture douce, aux effets de médium sec, aux traits calligraphiques, aux textures dispersées et aux formations multi-têtes.

![brush-tool](/images/screens/brush-tool.jpg)

## Marques expressives

Les pinceaux peuvent reposer sur des tampons bitmap, des formes procédurales ou des sources animées image par image. Un trait peut aller d'une simple marque ronde et douce à une tête de pinceau richement texturée ou évolutive. Le même moteur de peinture peut prendre en charge un dessin précis, une accumulation picturale, des marques décoratives et une rupture de type médium naturel.

Lorsqu'un pinceau devient visuellement complexe, l'aperçu peut rester simplifié afin que la peinture reste réactive et lisible.

![tool-setup](/images/screens/tool-setup.jpg)


## Dynamique et réponse à la saisie

L'outil Pinceau répond à la saisie en direct : pression du stylet, vitesse, direction, inclinaison et autres valeurs du contrôleur. Ces signaux peuvent influencer le trait visible de nombreuses façons : épaisseur, opacité, angle, réponse de texture, comportement couleur, espacement et autres qualités peuvent toutes évoluer au fil du geste.

Le pinceau ressemble moins à un motif estampé qu'à un instrument de dessin physique. Un toucher léger produit des marques délicates, un mouvement plus rapide ouvre texture ou forme, et un comportement sensible à la direction aide les traits à suivre le geste de la main.

![dynamics](/images/screens/dynamics.jpg)

## Comportement du trait

Les traits peuvent être directs et immédiats, ou assistés par lissage et stabilisation. Ces fonctions aident à réduire les tremblements indésirables, adoucir les changements brusques et rendre les mouvements longs plus contrôlés sans effacer le caractère de la saisie de l'artiste.

Le pinceau prend aussi en charge différentes approches d'accumulation de peinture. Il peut se comporter comme un trait continu, accumuler des touches répétées ou émettre des marques au fil du temps pendant que le pointeur reste en place. Cette souplesse le rend utile aussi bien pour le travail de ligne réfléchi que pour une construction tonale plus lente.

Pour les marques calligraphiques ou proches de l'encre, le pinceau peut générer un trait continu de forme plutôt que de s'appuyer uniquement sur des tampons répétés. Il produit alors des formes fluides, proches du ruban, qui répondent naturellement au geste et à la vitesse.

![stroke](/images/screens/stroke.jpg)

## Capture de trait et rendu simulé

Le pinceau peut capturer un court échantillon de la façon dont un préréglage est normalement dessiné à la main, puis utiliser ce profil pour rendre des traits définis par la géométrie plutôt que par le mouvement en direct. Les lignes droites Maj-clic, les tracés vectoriels et les sélections tracées peuvent tous reprendre le profil de pression et de vitesse capturé du préréglage d'outil actif, au lieu de se comporter comme une ligne mécanique plate.

Les traits construits restent ainsi plus proches du caractère du pinceau. Une ligne tracée à partir d'un chemin peut commencer en douceur, prendre de la pression, s'amenuiser ou varier en vitesse de la même manière que le geste manuel échantillonné, tout en suivant exactement la forme du chemin, du bord de sélection ou du geste en ligne droite.

## Post-traitement

Le pinceau peut enregistrer un trait pendant que vous le dessinez, puis rejouer ce geste capturé une fois le stylet relevé, en affinant le tracé avant que la marque finale ne soit posée. Vous pouvez croquer librement et obtenir malgré tout une direction plus nette, des angles plus précis ou une structure plus délibérée, sans dessiner avec une précision mécanique.

Cela ouvre des hachures et des marques de construction alignées sur des angles propres tout en conservant longueur et caractère dessinés à la main, des traits ruban stables malgré l'inclinaison, et une relecture consciente des angles qui traite différemment virages et lignes droites. Les pinceaux multi-têtes peuvent partager un tracé corrigé tandis que chaque tête conserve sa propre variation, et la dynamique peut encore façonner le trait le long de sa courbe finale pendant la relecture. Le post-traitement s'applique aux traits dessinés plutôt qu'à l'émission continue de type aérographe.

## Couleur et texture

Les coups de pinceau peuvent utiliser la couleur active, réagir aux dégradés ou varier la couleur via la dynamique. La gestion de texture permet au pinceau de passer d'une couverture pleine à des marques brisées qui effleurent la surface, utile pour les effets de pinceau sec, le grain et les ombrages expressifs.

Comme couleur et texture peuvent faire partie du même système dynamique que forme et opacité, un seul trait peut évoluer à mesure qu'il traverse le canevas au lieu de rester visuellement uniforme.

## Têtes de pinceau et formations

L'outil Pinceau peut peindre avec plusieurs têtes à la fois. Plusieurs têtes peuvent être disposées autour du tracé pour créer des marques de plume, des traits en éventail, un comportement proche des poils, des motifs de pulvérisation, des formations texturées ou des hachures structurées.

Ces têtes peuvent suivre la direction du déplacement, varier les unes des autres et se disperser de manière à donner au trait un aspect organique plutôt que mécaniquement répété. C'est particulièrement utile pour les pinceaux de type médium naturel, les traits décoratifs, le feuillage, la fourrure, les hachures et autres marques qui gagnent à une irrégularité maîtrisée.

![brush-heads](/images/screens/brush-heads.jpg)

## Charge de pinceau et reprise de matière

Le pinceau peut aussi simuler la quantité de peinture ou de matière actuellement portée. Au fil d'un trait, cette charge peut diminuer progressivement, laissant les marques devenir plus claires, plus sèches, plus fines, plus rugueuses ou autrement plus fragmentées selon la dynamique du pinceau.

La charge peut être réintroduite entre les traits, maintenue à un niveau choisi ou utilisée comme signal de contrôle en direct pour d'autres comportements. Il devient possible de créer des pinceaux proches des médiums réels : humides au début d'un trait, progressivement épuisés sur la distance, puis trempés à nouveau pour le passage suivant.

![material-state](/images/screens/material-state.jpg)

## Contact avec la surface

Le pinceau peut aussi simuler une perte intermittente de contact avec la surface à peindre — les marques brisées qui apparaissent lorsqu'un crayon, un fusain, un pinceau sec ou un marqueur partiellement épuisé n'engage le papier que partiellement.

Lorsque la simulation de contact est activée, le pinceau est soit en contact, soit levé. En contact, les marques se déposent normalement. Levé, aucune matière n'est déposée et le trait laisse un intervalle dont la longueur est choisie aléatoirement entre une distance minimale et maximale. La transition est binaire : l'effet ne modifie pas opacité, taille, dureté, espacement ou débit — seulement le fait que la peinture soit posée ou non.

La facilité avec laquelle le contact se perd dépend d'un seuil de contact, de la pression du stylet et, éventuellement, de la charge du pinceau. Des seuils plus élevés rendent les ruptures plus fréquentes. La pression agit comme une force stabilisatrice : une pression légère augmente le risque de perdre le contact, tandis qu'une pression ferme favorise un trait continu. Lorsque la charge de pinceau est activée, une faible charge peut rendre la marque plus brisée et une charge élevée peut aider à maintenir le contact, comme un outil qui transporte encore assez de matière pour accrocher la surface.

La perte est évaluée à partir de la distance parcourue par le trait plutôt que du nombre de touches, de sorte que les pinceaux à espacement dense ou clairsemé se comportent de façon cohérente. La fonctionnalité fonctionne avec le rendu par tampons et le rendu calligraphique, produisant des intervalles cohérents le long du trait plutôt que des touches isolées sautées.

## Animation et variation

Les sources de pinceau animées peuvent changer d'image au fil du trait, donnant aux pinceaux une impression de mouvement et de variété. La randomisation et la variation par trait empêchent les marques répétées de paraître identiques, tandis qu'une graine stable peut préserver un caractère cohérent lorsque la répétabilité est nécessaire.

Ces comportements conviennent aux pinceaux qui doivent paraître vivants : poils qui se déplacent au fil du trait, tampons texturés qui évoluent subtilement, ou outils multi-têtes où chaque tête a sa propre personnalité.

## Flux de travail centré sur l'artiste

L'outil Pinceau est organisé pour que les décisions de peinture courantes restent à portée de main, tandis que les choix de configuration moins fréquents restent en retrait. L'objectif est de garder l'outil accessible pendant la peinture tout en permettant une personnalisation profonde de la conception des pinceaux.

Dans l'ensemble, le pinceau couvre aussi bien la peinture quotidienne que la création de marques spécialisées : croquis rapides, illustration soignée, rendu texturé, travail à l'encre expressif et effets procéduraux complexes partagent la même base flexible.
