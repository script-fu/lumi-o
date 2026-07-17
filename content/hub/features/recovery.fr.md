---
title: "Récupération de fichiers"
type: docs
url: "hub/features/recovery"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
---

Le système de récupération de Lumi est conçu pour protéger le travail de peinture contre les plantages, les erreurs et les sessions interrompues. Il offre aux projets un filet de sécurité sans obliger les artistes à dupliquer constamment les fichiers à la main.

La récupération repose sur deux idées : la protection automatique en arrière-plan et les points de contrôle intentionnels. Ensemble, elles aident à préserver le travail récent tout en permettant de revenir à des moments antérieurs d'un projet.

![recover](/images/screens/recover.jpg)

## Protection automatique

Pendant qu'une image est en cours de modification, Lumi peut conserver les données de récupération séparées du fichier de travail principal. Le projet lui-même n'a donc pas besoin d'être réécrit à chaque instantané de sécurité.

En cas de problème, l'état de récupération automatique peut fournir une version récente de l'illustration, parfois plus récente que la dernière sauvegarde volontaire. L'objectif est simple : réduire la quantité de travail perdu lorsqu'une session se termine de façon inattendue.

## Points de contrôle intentionnels

Certaines étapes d'une illustration méritent d'être préservées délibérément : avant un changement de couleur majeur, après une esquisse réussie, avant d'aplatir des calques ou lorsqu'on tente une direction risquée.

Lumi prend en charge des points de contrôle au niveau du projet pour ces moments. Ils sont plus légers que de conserver une copie complète pour chaque essai, tout en offrant un moyen de revenir à des étapes significatives de l'historique du travail.

## Récupération contextualisée

Les états de récupération sont présentés comme des versions de l'illustration plutôt que comme des fichiers bruts à parcourir manuellement. L'artiste peut comparer les sauvegardes automatiques récentes et les points de contrôle volontaires, puis ouvrir l'état qui correspond le mieux au travail qu'il souhaite poursuivre.

Les images récupérées s'ouvrent comme des documents de travail, ce qui permet de les examiner avant de décider comment les enregistrer ou les continuer.

## Garder la récupération pratique

Un système de récupération utile doit aussi rester gérable. Lumi est conçu pour organiser les données de récupération et permettre de supprimer les anciens états lorsqu'ils ne sont plus nécessaires.

La sécurité ne devient ainsi pas un encombrement. La récupération peut rester active en arrière-plan, tandis que l'artiste garde le contrôle sur la quantité d'historique conservée.

## Travailler en confiance

La récupération de fichiers ne remplace pas l'enregistrement, mais rend le travail créatif moins fragile. Les artistes peuvent peindre, expérimenter et prendre des risques en sachant que Lumi maintient des moyens supplémentaires de revenir en arrière lorsqu'une session, un fichier ou une décision tourne mal.
