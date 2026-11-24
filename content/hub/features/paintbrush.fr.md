---
title: "Outil Pinceau"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b11c0248addcd834613763c198cf5d00356b8d31e99d3dad18a1cea96e6b7e3f
---

L'outil Pinceau est l'instrument de peinture principal de Lumi : une façon réactive et expressive de dessiner, peindre, ombrer, texturer et poser des marques directement sur le canevas. Il est conçu pour rester immédiat tout en laissant aux artistes la possibilité de façonner le comportement d'un trait.

Plutôt qu'un pinceau fixe unique, il agit comme un système de peinture. La forme du pinceau, la texture, le grain de surface, le mouvement, la pression, le timing, la couleur et la façon dont le pinceau rencontre la surface peuvent tous contribuer à la marque finale, ce qui le rend adapté aux lignes nettes, à la peinture douce, aux effets de médium sec, aux traits calligraphiques, aux textures dispersées, aux touffes de poils et aux formations multi-têtes.

## Marques expressives

Les pinceaux peuvent reposer sur des tampons bitmap, des formes procédurales ou des sources animées image par image. Un trait peut aller d'une simple marque ronde et douce à une tête de pinceau richement texturée ou évolutive. Le même moteur de peinture peut prendre en charge un dessin précis, une accumulation picturale, des marques décoratives et une rupture de type médium naturel.

Lorsqu'un pinceau devient visuellement complexe, l'aperçu peut rester simplifié afin que la peinture reste réactive et lisible.

## Dynamique et réponse à la saisie

L'outil Pinceau répond à la saisie en direct : pression du stylet, vitesse, direction, inclinaison et autres valeurs du contrôleur. Ces signaux peuvent influencer le trait visible de nombreuses façons : épaisseur, opacité, angle, réponse de texture, comportement couleur, espacement et autres qualités peuvent toutes évoluer au fil du geste.

Le pinceau ressemble moins à un motif estampé qu'à un instrument de dessin physique. Un toucher léger produit des marques délicates, un mouvement plus rapide ouvre texture ou forme, et un comportement sensible à la direction aide les traits à suivre le geste de la main.

## Comportement du trait

Les traits peuvent être directs et immédiats, ou assistés par lissage et stabilisation. Ces fonctions aident à réduire les tremblements indésirables, adoucir les changements brusques et rendre les mouvements longs plus contrôlés sans effacer le caractère de la saisie de l'artiste.

Le pinceau prend aussi en charge différentes approches d'accumulation de peinture. Il peut se comporter comme un trait continu, accumuler des touches répétées ou émettre des marques au fil du temps pendant que le pointeur reste en place. Cette souplesse le rend utile aussi bien pour le travail de ligne réfléchi que pour une construction tonale plus lente.

Pour les marques calligraphiques ou proches de l'encre, le pinceau peut générer un trait continu de forme plutôt que de s'appuyer uniquement sur des tampons répétés. Il produit alors des formes fluides, proches du ruban, qui répondent naturellement au geste et à la vitesse.

## Capture de trait et rendu simulé

Le pinceau peut capturer un court échantillon de la façon dont un préréglage est normalement dessiné à la main, puis utiliser ce profil pour rendre des traits définis par la géométrie plutôt que par le mouvement en direct. Les lignes droites Maj-clic, les tracés vectoriels et les sélections tracées peuvent tous reprendre le profil de pression et de vitesse capturé du préréglage d'outil actif, au lieu de se comporter comme une ligne mécanique plate.

Les traits construits restent ainsi plus proches du caractère du pinceau. Une ligne tracée à partir d'un chemin peut commencer en douceur, prendre de la pression, s'amenuiser ou varier en vitesse de la même manière que le geste manuel échantillonné, tout en suivant exactement la forme du chemin, du bord de sélection ou du geste en ligne droite.

## Post-traitement

Le pinceau peut enregistrer un trait pendant que vous le dessinez, puis rejouer ce geste capturé une fois le stylet relevé, en affinant le tracé avant que la marque finale ne soit posée. Vous pouvez croquer librement et obtenir malgré tout une direction plus nette, des angles plus précis ou une structure plus délibérée, sans dessiner avec une précision mécanique.

Cela ouvre des hachures et des marques de construction alignées sur des angles propres tout en conservant longueur et caractère dessinés à la main, des traits ruban stables malgré l'inclinaison, et une relecture consciente des angles qui traite différemment virages et lignes droites. Les pinceaux multi-têtes peuvent partager un tracé corrigé tandis que chaque tête conserve sa propre variation, et la dynamique peut encore façonner le trait le long de sa courbe finale pendant la relecture. Le post-traitement s'applique aux traits dessinés plutôt qu'à l'émission continue de type aérographe.

## Couleur et texture

Les coups de pinceau peuvent utiliser la couleur active, réagir aux dégradés ou varier la couleur via la dynamique. La gestion de texture permet au pinceau de passer d'une couverture pleine à des marques brisées qui effleurent la surface, utile pour les effets de pinceau sec, le grain et les ombrages expressifs.

Comme couleur et texture peuvent faire partie du même système dynamique que forme et opacité, un seul trait peut évoluer à mesure qu'il traverse le canevas au lieu de rester visuellement uniforme.

La texture d'une pointe de pinceau fragmente le tampon lui-même. La texture de surface, décrite ensuite, est le grain et l'accroche de la surface que le trait rencontre.

## Texture de surface

La texture de surface donne à un trait le grain et l'accroche d'un support : papier, toile ou un autre motif. Lumi fournit des grains prévus pour cela. Les zones claires du motif laissent passer la peinture. Les zones sombres la retiennent, si bien que la marque se brise comme un médium sec se brise sur l'accroche.

La place de ce grain peut changer avec le trait. Chaque trait peut commencer à son propre endroit dans le motif, et un virage peut faire rouler un nouveau décalage. C'est dans l'ombrage en aller-retour que cela se voit : chaque renversement prélève une phase nouvelle de l'accroche, et les passages qui se recouvrent construisent un ombrage brisé et réaliste. Une courbe progressive peut garder la phase déjà engagée sous le trait, de sorte que l'accroche reste cohérente le long du geste.

Les pinceaux estampés et les traits calligraphiques peuvent tous deux recevoir ce grain. Une rangée de touches et un ruban d'encre continu peuvent se briser sur la même sorte d'accroche. Sur un trait calligraphique, la ligne reste une seule marque façonnée, et le grain décide où ce ruban est ouvert et où il est retenu.

L'ouverture de l'accroche peut varier le long du trait. La pression et les autres saisies font passer l'accroche d'une lecture plus sèche à une lecture plus pleine. Dans la lecture plus sèche, les vides restent dégagés. À mesure que le trait s'ouvre, les îlots peints peuvent s'étendre dans ces vides, et le grain se remplit depuis ses bords. À la lecture pleine, le trait rejoint le motif plus ouvert.

Des touches qui se chevauchent dans un même trait peuvent monter l'opacité dans les îlots. Elles peuvent aussi poser une seule nappe de couverture qu'une boucle suivante de ce trait réécrit, de sorte que le grain ne s'empile pas sur lui-même.

## Têtes de pinceau et formations

L'outil Pinceau peut peindre avec plusieurs têtes à la fois. Plusieurs têtes peuvent être disposées autour du tracé pour créer des marques de plume, des traits en éventail, un comportement proche des poils, des motifs de pulvérisation, des formations texturées ou des hachures structurées.

Ces têtes peuvent suivre la direction du déplacement, varier les unes des autres et se disperser de manière à donner au trait un aspect organique plutôt que mécaniquement répété. C'est particulièrement utile pour les pinceaux de type médium naturel, les traits décoratifs, le feuillage, la fourrure, les hachures et autres marques qui gagnent à une irrégularité maîtrisée.

Les pinceaux générés peuvent aussi être construits comme des champs de poils. La forme de ces champs, et la façon dont ils bougent le long d'un trait, sont décrites plus bas.

## Forme des poils

Un pinceau à poils est un champ de marques distinctes, tenues dans une forme générée. Le contour de cette forme décide où les poils peuvent se placer : un cercle, un carré, un losange, un triangle, une forme hérissée, des coins adoucis ou un format étiré. Chaque poil garde sa propre marque à l'intérieur de ce contour.

L'espacement et la taille règlent le serrage des poils et la grandeur de chacun. La variation de taille laisse les poils voisins différer, d'une tête uniforme à une tête dont les poils vont de la pleine taille jusqu'au vide. La régularité fait passer le champ d'une grille régulière à une dispersion plus lâche. Le regroupement ramène les poils vers moins de centres, de sorte que la tête se lit comme des touffes. Le champ peut aussi s'éclaircir, et laisser un ensemble plus clairsemé des mêmes marques.

Les pointes peuvent rester rondes. Elles peuvent aussi devenir de petits polygones qui restent à l'intérieur de chaque poil. Elles peuvent être réparties régulièrement autour du cercle, ou laissées irrégulières, et se tenir sur le bord ou se retirer vers l'intérieur. Là où les pointes se chevauchent, c'est la marque la plus forte qui apparaît.

Chaque tête d'un pinceau multi-têtes peut porter sa propre disposition : un éventail de plumes est alors un ensemble de touffes apparentées.

La taille des poils peut suivre la pression du stylet et les autres saisies qui façonnent un trait. Quand cette taille change, la tête peut être redessinée, de sorte qu'un toucher léger et un appui ferme diffèrent dans les poils eux-mêmes.

## Poils le long du trait

Pendant qu'un trait se trace, les poils peuvent bouger comme une touffe vivante. Ils traînent derrière la main, basculent dans un virage et se posent dans la nouvelle direction, repoussent quand le trait s'inverse, se tordent avec la rotation du pinceau, s'ouvrent sous la pression et reviennent vers leur repos. Les poils qui traînent peuvent dépasser le contour au repos.

La distance qu'ils parcourent, la tenue des touffes et la vitesse à laquelle elles se posent se règlent librement. Une touffe raide peut rester sur une seule trace. Une plus souple peut se fendre, retarder une torsion et garder un peu de la courbure avant de revenir. Le même geste produit le même mouvement.

Un aérographe est un autre genre de mouvement. Il peut poser un arrangement de poils neuf à mesure qu'il émet, de sorte qu'un trait tenu renouvelle le jet sans cesse. Un trait calligraphique conserve sa plume continue, et cette plume peut encore recevoir une texture de surface. Ce mouvement vivant appartient au pinceau à poils estampé.

## Dégradés de bord

Un trait peut porter un côté net et un côté estompé, à la façon d'un bâton de fusain ou d'un pastel. Vers le côté doux, la peinture peut s'effacer, la pointe sécher et se fragmenter, et l'accroche du papier se montrer davantage. Les poils peuvent s'y éclaircir et disparaître, et une formation de têtes rester serrée et pleine du côté dur, tandis que le côté doux se relâche et se disperse en jet.

La chute de ce dégradé, et la direction vers laquelle il se tourne, se règlent librement. Il peut suivre le trait, ou rester sur l'inclinaison du stylet, de sorte qu'un ombrage en aller-retour garde le même bord net.

## Charge de pinceau et reprise de matière

Le pinceau peut aussi simuler la quantité de peinture ou de matière actuellement portée. Au fil d'un trait, cette charge peut diminuer progressivement, laissant les marques devenir plus claires, plus sèches, plus fines, plus rugueuses ou autrement plus fragmentées selon la dynamique du pinceau.

La charge peut être réintroduite entre les traits, maintenue à un niveau choisi ou utilisée comme signal de contrôle en direct pour d'autres comportements. Il devient possible de créer des pinceaux proches des médiums réels : humides au début d'un trait, progressivement épuisés sur la distance, puis trempés à nouveau pour le passage suivant.

## Contact du pinceau

Le contact du pinceau est la manière dont un trait rencontre le papier, puis le quitte. Au fil du trajet, la marque peut gonfler et se resserrer, se lever sur une longueur, puis se reposer. Un crayon, un fusain, un pinceau sec ou un marqueur fatigué peuvent s'épaissir, sauter et reprendre.

La facilité de ce mouvement peut suivre la main. Un toucher léger casse plus tôt. Un appui ferme reste posé plus longtemps. Les marques plus épaisses peuvent garder le contact pendant que les plus fines ouvrent des trous, et un rétrécissement peut être ce qui fait qu'un trait lourd commence à sauter. Le décollement peut être soudain, ou la marque peut se réduire dans le vide et en ressortir en grandissant.

Un pinceau pleinement chargé peut rencontrer le papier autrement. Le gonflement et le rétrécissement sont plus marqués, et la marque tient le contact sur de plus longues distances. À mesure que la charge s'épuise, cette façon de poser s'estompe, et le trait devient plus discret et plus enclin à sauter.

Une touffe de poils peut le faire poil par poil, si bien que le trait s'effiloche en brins séparés, puis se rassemble. Les têtes d'une formation peuvent rompre ensemble, ou chacune de son côté. Un trait calligraphique se lève d'un seul bec, et le ruban s'arrête là où le contact s'arrête. Chaque nouveau trait trouve son propre rythme.

## Animation et variation

Les sources de pinceau animées peuvent changer d'image au fil du trait, donnant aux pinceaux une impression de mouvement et de variété. La randomisation et la variation par trait empêchent les marques répétées de paraître identiques, tandis qu'une graine stable peut préserver un caractère cohérent lorsque la répétabilité est nécessaire.

Ces comportements conviennent aux pinceaux qui doivent paraître vivants : tampons texturés qui changent subtilement avec le temps, ou outils multi-têtes où chaque tête a sa propre personnalité. Les pinceaux à poils ont leur propre animation en cours de trait, décrite plus haut.

## Traits OpenGL

Un préréglage d'outil peut peindre ses traits avec OpenGL. La différence se voit surtout sur les grands aérographes et les grands tampons de texture, qui suivent la main à mesure que la marque grandit. La fenêtre d'image peut aussi dessiner la pile de calques avec OpenGL, ce qui garde fluide la navigation sur le canevas. Les deux sont décrits dans [OpenGL](/hub/features/opengl/).

## Flux de travail centré sur l'artiste

L'outil Pinceau est organisé pour que les décisions de peinture courantes restent à portée de main, tandis que les choix de configuration moins fréquents restent en retrait. L'objectif est de garder l'outil accessible pendant la peinture tout en permettant une personnalisation profonde de la conception des pinceaux.

Dans l'ensemble, le pinceau couvre aussi bien la peinture quotidienne que la création de marques spécialisées : croquis rapides, illustration soignée, rendu texturé, travail à l'encre expressif et effets procéduraux complexes partagent la même base flexible.
