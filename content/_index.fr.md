---
title: "Peinture numérique open source pour Linux"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o est conçu pour la peinture raster par calques : un outil volontairement léger et stable. La prise en charge des tablettes Wacom offre la saisie fluide et réactive exigée par l’illustration professionnelle.

Le système de couleurs de Lumi, fondé sur les palettes, mélange de façon spectrale des profils de pigments réels. Créez des palettes à partir de relations chromatiques visualisées, maîtrisez les valeurs tonales et organisez vos couleurs sous forme d’entrées nommées. La Palette Map et le Mixer vous aident à maintenir la cohérence de vos choix chromatiques au sein d’une illustration ou d’un projet.

Bâti sur l’architecture centrale de GIMP, Lumi-o est conçu et affiné par un artiste. Ses outils restent accessibles tout en offrant la personnalisation nécessaire aux flux de travail avancés. Des chargements et enregistrements rapides, ainsi qu’un système de sauvegarde automatique entièrement intégré, protègent votre travail sans interrompre votre élan.

Les traits de pinceau peuvent prendre une texture de surface, avec le grain et l’accroche d’un support, y compris les traits calligraphiques. Un nouveau trait, ou un virage dans le trait, peut décaler ce grain, de sorte qu’un ombrage en aller-retour se construit sur une phase nouvelle à chaque renversement. Les pinceaux à poils se façonnent dans le détail, et leurs poils peuvent se déplacer et se renouveler le long du trait. OpenGL dessine la pile de calques dans la fenêtre d’image et peut peindre les traits de chaque préréglage d’outil. Les grands aérographes et les grands tampons de texture sont bien plus rapides, et la navigation sur le canevas reste fluide.

Le chargement différé permet à Lumi d’ouvrir des projets volumineux et complexes en utilisant beaucoup moins de mémoire vive. Lorsqu’un groupe est replié, Lumi charge l’image composite enregistrée, tandis que ses calques, masques et groupes imbriqués restent de légers espaces réservés jusqu’à leur ouverture.

Lumi-o est développé sous Debian 13 avec Cinnamon sur X11. GNOME sur Wayland a été testé pour l’apparence générale de l’application et pour le rendu OpenGL. Si vous venez de Windows, installer Debian est la première étape. Consultez [Comment installer Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="À propos" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Démarrage rapide" icon="play" >}}
  {{< card link="hub/features/" title="Fonctionnalités" icon="book-open" >}}
{{< /cards >}}