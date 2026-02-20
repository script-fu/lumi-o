---
title: "Navigateur de procédures"
type: docs
---
Le navigateur de procédures est le principal outil de référence pour découvrir les centaines de fonctions disponibles dans la base de données procédurale (PDB) de Lumi. Étant donné que chaque outil, filtre et script de Lumi doit être enregistré dans le PDB pour pouvoir être appelé, ce navigateur est en fait un explorateur PDB complet.

## Ouverture du navigateur de procédures

Accédez à **Aide → Programmation → Navigateur de procédures**.

Vous pouvez également y accéder depuis la console Scheme via **Parcourir**.

## Ce que ça montre

Le navigateur de procédures peut répertorier toutes les procédures actuellement enregistrées dans le PDB, quelle que soit leur origine. Par défaut, la recherche "interne" est effectuée pour afficher les procédures principales enregistrées en interne.

- **Procédures internes** : fonctions de base pour la manipulation d'images, la gestion des calques et le contrôle des outils.
- **Plug-ins externes** : procédures fournies par des plug-ins C/C++ compilés ou des extensions persistantes.

## Recherche et filtrage

- **Champ de recherche** : filtre les procédures par nom, description ou auteur. Effacer le champ de recherche affiche toutes les procédures disponibles.
- **Type de recherche** : la liste déroulante de recherche vous permet de filtrer par champs spécifiques. Si vous le définissez sur **par type** et recherchez « interne », la liste se rétrécira pour afficher uniquement les procédures principales enregistrées en interne.
- **Vue détaillée** : cliquer sur une procédure affiche ses paramètres, ses valeurs de retour, son auteur, sa date et une description de ce qu'elle fait.

Ceci est essentiel pour trouver le nom exact et la signature d'argument d'une fonction que vous souhaitez appeler à partir de votre script.