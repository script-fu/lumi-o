---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi peut dessiner la pile de calques avec OpenGL, et peindre les traits de pinceau de la même manière. La navigation sur le canevas reste fluide. Les grands aérographes et les grands tampons de texture sont bien plus rapides. Le fait qu'un outil donné peigne ainsi est enregistré dans son préréglage.

## La pile de calques

La fenêtre d'image montre la pile de calques composée. Avec OpenGL, cette image est stockée en tuiles sur le processeur graphique, déjà prête pour l'affichage. Le déplacement, le zoom, la rotation et le retournement changent la vue de ces tuiles. Sur un grand canevas, les gestes restent fluides, parce que la navigation est un changement de vue sur une image déjà là.

Les modifications mettent à jour les tuiles qu'elles touchent. Les guides, le curseur du pinceau et le contour de la sélection sont dessinés par-dessus cette image.

Tant que le stylet est posé, un trait qui utilise OpenGL peut s'afficher en place parmi les calques qui l'entourent, y compris les calques au-dessus de celui qui est peint. La marque est écrite dans le calque lorsque le stylet se relève.

## Traits par préréglage d'outil

Peindre en OpenGL est un choix du préréglage d'outil, au même titre que le pinceau et la dynamique. Un préréglage peut peindre sur le processeur graphique. Un autre peut garder la voie de trait ordinaire. Revenir à un outil ramène ce choix avec lui.

Le gain est le plus net là où les tampons sont grands. Sur la voie ordinaire, un aérographe large se fait distancer par le stylet à mesure que la marque grandit : les touches s'accumulent en file, et le trait se creuse de vides, y compris pendant que le stylet reste posé et que le pinceau continue d'émettre. Avec OpenGL, cet aérographe continue de poser ses marques au rythme de la main.

Un grand tampon de texture montre le même changement. La voie ordinaire passe plus de temps sur chaque touche à mesure que le tampon grandit, jusqu'à ce qu'un trait cale. Le trait OpenGL suit le geste. Les tampons qui portent un grain de surface peuvent eux aussi être dessinés avec OpenGL.

Les marques de poils peuvent aussi être dessinées ainsi. Un tampon de poils à pointe ronde peut être peint avec OpenGL pendant que les poils changent le long du trait. Un trait calligraphique peut être dessiné comme un seul ruban continu, et la texture de surface peut masquer ce ruban. La [texture de surface](/hub/features/paintbrush/#texture-de-surface) et les [poils le long du trait](/hub/features/paintbrush/#poils-le-long-du-trait) appartiennent au pinceau lui-même.

Certains traits restent sur la voie ordinaire. Le mélange spectral, un filtre sur le calque peint et certains modes de fusion en font partie. Ces marques sont tout de même peintes.

## Quand le canevas ordinaire demeure

OpenGL sert lorsque le matériel graphique peut l'exécuter et que l'image tient dans la mémoire réservée au canevas. Sinon, Lumi dessine la fenêtre comme auparavant. L'AppImage publiée embarque ses propres bibliothèques graphiques. Sur un pilote que ces bibliothèques ne peuvent pas utiliser, y compris le pilote propriétaire de NVIDIA, le canevas reste sur cette voie ordinaire.

## Cinnamon sur X11, et GNOME sur Wayland

Lumi est développé sur Debian avec Cinnamon et X11. Cela reste l'environnement de développement. GNOME sur Wayland a été testé pour l'apparence générale de l'application et pour ce rendu OpenGL. La navigation sur le canevas y est fluide aussi.
