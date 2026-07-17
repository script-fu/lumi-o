---
title: "Développement assisté par l'IA"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
---
Les outils d'IA modernes peuvent accélérer considérablement le développement de plug-ins Lumi en jouant le rôle d'un partenaire de codage collaboratif.

## VS Code en mode Agent

Utiliser Visual Studio Code avec un assistant IA en **mode Agent** (comme le mode Agent de GitHub Copilot ou d'autres assistants dotés d'outils) permet d'effectuer des tâches complexes en plusieurs étapes en langage naturel.

Au lieu de se contenter de compléter une seule ligne de code, un agent peut :
- lire l'intégralité de l'espace de travail pour comprendre le contexte
- créer de nouveaux fichiers et répertoires
- exécuter des commandes de terminal pour tester ou valider des scripts
- rechercher des modèles existants dans la base de code

## Accès au dépôt

L'assistance IA est plus efficace lorsque l'agent a accès à **lumi-dev** ou au dépôt de votre projet. Avec une visibilité sur le code existant, l'agent peut :
- utiliser les **[bibliothèques utilitaires]({{< ref "/hub/scripting/reference/utility-browser" >}})** comme référence pour les fonctions d'aide
- suivre les modèles existants pour les opérations GEGL et la gestion des calques
- réutiliser le code répétitif des plug-ins établis

## Exemple de flux de travail

Vous pouvez demander directement à l'agent de générer un plug-in complet en décrivant le résultat fonctionnel souhaité :

> « À l'aide des utilitaires Scheme et des exemples disponibles dans l'espace de travail, écrivez un nouveau plug-in qui crée un guide horizontal à 50 % sur l'image active et le nomme 'Center Guide'. »

L'agent recherchera comment créer des guides, identifiera la bonne fonction utilitaire (comme `lumi-image-add-hguide-percent` dans `common.scm`) et générera le fichier `.scm` complet avec le boilerplate d'enregistrement correct.

## Bonnes pratiques

- **Soyez précis** : décrivez exactement ce que le plug-in doit faire.
- **Référencer les utilitaires** : encouragez l'agent à consulter le répertoire `share/lumi/scripts/` pour trouver des helpers de haut niveau.
- **Relire et tester** : testez toujours le plug-in généré par l'IA — c'est souvent un processus itératif et créatif.
