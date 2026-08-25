---
title: "Format de fichier (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Le format natif `.lum` de Lumi est un répertoire de projet, pas un fichier unique fermé. Il est conçu pour l'illustration en calques : arbres de calques profonds, grandes toiles, masques, effets non destructifs, et des points de contrôle qui n'ont pas à dupliquer toute la peinture.

Le rôle du format est de préserver cette structure de travail — pour rouvrir un projet à l'identique, l'inspecter quand quelque chose tourne mal, et le récupérer depuis un point de contrôle récent, sans traiter l'œuvre comme une masse opaque.

## Une séparation volontaire

Un projet `.lum` est un dossier. L'arbre des calques et les propriétés de l'image tiennent dans un XML lisible. Chaque calque et chaque masque conserve son propre tampon de pixels, nommé d'après l'œuvre plutôt que d'après un identifiant interne. Les tracés vectoriels sont stockés en SVG ordinaire. Les réglages de filtres volumineux occupent leurs propres fichiers, à côté de l'image. Les profils ICC sont stockés une seule fois à la racine du projet : les instantanés de récupération s'y réfèrent au lieu de les recopier.

C'est cette séparation qui rend le reste du format possible. Les calques inchangés peuvent rester tels quels sur le disque. Un tampon endommagé pose problème tout seul, sans emporter le fichier entier. Des pixels de calque manquants deviennent des calques vides qui gardent encore nom, position et réglages de fusion ; un aperçu de groupe manquant se reconstruit à partir des enfants. Le projet reste le plan de construction de la peinture.

Les palettes de pigments relèvent des outils couleur de Lumi. Un projet peut mémoriser la palette associée à l'image, mais la bibliothèque de palettes elle-même se trouve hors du `.lum`.

## Un état modifiable, pas un aplatissement

Le fichier conserve la peinture en cours. Les calques restent des calques, les groupes restent des groupes, les masques restent des masques — y compris décalages, verrous, comportement de fusion et piles de filtres. Les filtres non destructifs s'enregistrent comme opérations et paramètres, non comme pixels déjà appliqués. Un calque d'une seule couleur unie n'a pas besoin de fichier de pixels.

Les groupes réduits conservent aussi une vue composée d'eux-mêmes. C'est cet aperçu enregistré qui s'affiche sur la toile lorsqu'un groupe est fermé : inutile de reconstruire les enfants juste pour regarder l'image. Les modes d'inspection réservés à l'affichage restent hors de ce cache : afficher un masque ou l'alpha pour l'édition se restaure comme métadonnée, sans être gravé dans le groupe enregistré.

## Les fichiers volumineux peuvent rester en partie sur le disque

Ouvrir un `.lum` n'oblige pas à charger tous les pixels. Le contenu des groupes réduits peut rester sur le disque, tandis que l'aperçu enregistré du groupe s'affiche immédiatement. C'est en développant un groupe que ces calques, masques et groupes imbriqués passent en mémoire. Les groupes qui restent fermés restent légers.

Le fichier consigne aussi quels groupes étaient réellement utilisés. Ceux qui se trouvent sur le chemin de la sélection active peuvent se rouvrir développés ; les autres dossiers sont stockés réduits, même s'ils étaient ouverts lors de la dernière session. Un fichier profond n'a ainsi pas à charger en mémoire chaque branche inutilisée dès l'ouverture.

Regrouper est donc un choix de performance autant que d'organisation. Les grands fonds, les essais archivés et les variantes inutilisées peuvent rester dans des groupes fermés, sans occuper la même mémoire que les calques en cours de peinture. L'enregistrement suit la même règle : les tampons encore cachés sont copiés ou omis en tant que fichiers, sans être renvoyés en mémoire juste pour être réécrits.

## Des points de contrôle qui n'écrivent que ce qui a changé

Fichier → Enregistrer met à jour le projet de travail. Les enregistrements incrémentiels et la sauvegarde automatique écrivent dans un arbre de récupération, et n'écrivent que les données modifiées — les tampons de calques changés, pas une seconde copie de l'image entière. Chaque point de contrôle porte néanmoins une description complète de l'arbre des calques : n'importe quelle étape de cet historique peut s'ouvrir en complétant les pixels inchangés depuis des points de contrôle plus anciens et, au besoin, depuis le fichier de travail lui-même.

La sauvegarde automatique reprend le même schéma dans un cache séparé, pour que la protection automatique n'ait pas à réécrire le fichier sur le disque. Si l'on ouvre un projet alors qu'il existe des points de contrôle plus récents que le dernier enregistrement complet, Lumi peut les proposer au lieu d'écarter silencieusement le travail plus récent. Les images récupérées s'ouvrent sous un nom distinct, afin qu'un enregistrement rapide ne puisse pas écraser l'original.

## Un format de travail

`.lum` sert à poursuivre une peinture dans Lumi. Les formats aplatis ou de compatibilité servent à la publication, à la livraison et aux autres applications. Comme un projet est un répertoire de nombreux fichiers, il convient de l'archiver s'il doit voyager.

Le fichier de travail reste riche et modifiable. L'export est la façon dont une image terminée ou partagée quitte cette structure.
