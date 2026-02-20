---
title: "Développement assisté par l'IA"
type: docs
---
Les outils d'IA modernes peuvent accélérer considérablement le développement du plug-in Lumi en agissant en tant que partenaire de codage collaboratif.

## VS Code en mode Agent

L'utilisation de Visual Studio Code avec un assistant IA en **mode Agent** (tel que le mode Agent de GitHub Copilot ou d'autres assistants activés par des outils) vous permet d'effectuer des tâches complexes en plusieurs étapes en utilisant le langage naturel.

Au lieu de simplement compléter une seule ligne de code, un agent peut :
- Lisez l'intégralité de votre espace de travail pour comprendre le contexte.
- Créez de nouveaux fichiers et répertoires.
- Exécutez des commandes de terminal pour tester ou valider des scripts.
- Recherchez des modèles existants dans votre base de code.

## Accès au référentiel

L'assistance de l'IA est plus efficace lorsque l'agent a accès au **lumi-dev** ou au référentiel de votre projet spécifique. Avec une visibilité sur la base de code existante, l'agent peut :
- Utilisez le **[Utility Libraries](@@LUMI_TOKEN_4@@)** comme référence pour les fonctions d'assistance.
- Suivez les modèles existants pour les opérations GEGL et la gestion des couches.
- Réutilisez le code passe-partout des plug-ins établis.

## Exemple de flux de travail

Vous pouvez directement demander à l'Agent de générer un plug-in complet en décrivant le résultat fonctionnel souhaité :

> "À l'aide des utilitaires Scheme et des exemples disponibles dans l'espace de travail, écrivez un nouveau plug-in qui crée un guide horizontal à 50 % sur l'image active et le nomme 'Guide central'."

L'agent recherchera comment créer des guides, identifiera la fonction utilitaire correcte (comme `lumi-image-add-hguide-percent` de `common.scm`) et générera le fichier `.scm` complet avec le passe-partout d'enregistrement correct.

## meilleures pratiques

- **Soyez précis** : décrivez exactement ce que vous souhaitez que le plug-in fasse.
- **Utilitaires de référence** : encouragez l'agent à consulter le répertoire `share/lumi/scripts/` pour trouver des assistants de haut niveau.
- **Review and Test** : Testez toujours le plug-in généré par l'IA, c'est souvent un processus itératif et créatif.