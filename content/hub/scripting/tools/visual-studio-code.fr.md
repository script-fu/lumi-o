---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## Qu'est-ce que Cursor ?

[Cursor](https://cursor.com/) est un éditeur de code avec une aide par IA dans la même fenêtre. Il repose sur Visual Studio Code : la coloration syntaxique, les extensions, les espaces de travail et Git sont donc conservés. Cursor est l'éditeur utilisé ici pour les scripts Lumi, parce que cette aide par IA est bien plus économique.

- **Coloration syntaxique** : La coloration attribue des couleurs au code. Les erreurs se repèrent plus vite, la structure se lit mieux, et un code complexe devient plus clair.
- **Extensions** : Cursor exécute les extensions de Visual Studio Code. Langue, thème ou outil : une extension convient.
- **Intégration du contrôle de version** : Git est intégré. Il devient simple de suivre les modifications et de gérer les branches, avec des services comme [GitHub](https://github.com/) ou [GitLab](https://gitlab.com/).
- **L'IA dans l'éditeur** : Vous pouvez demander une recherche, un remaniement ou une petite modification à côté du fichier déjà ouvert. Le coût de cette aide est assez bas pour en faire un geste ordinaire d'écriture de scripts.

### Principales fonctionnalités pour les débutants

- **Espaces de travail** : Cursor range plusieurs projets dans des « espaces de travail », qui gardent ensemble les fichiers et dossiers liés. La navigation et la gestion du projet en sont plus simples.

- **Profils** : Les profils permettent des configurations différentes selon les projets ou les tâches. Un profil pour le web, un autre pour Scheme, chacun avec ses extensions, ses thèmes et ses réglages.

### Premiers pas avec Cursor

Pour commencer avec Cursor :

- **Télécharger et installer** : Allez sur le [site de Cursor](https://cursor.com/) et téléchargez la version pour Linux.

- **Explorer l'interface** : En ouvrant Cursor, vous verrez plusieurs panneaux. À gauche se trouve la **barre d'activité**, d'où vous passez des fichiers à la recherche, à Git et aux extensions.

- **Ouvrir un fichier ou un dossier** : Préparez l'espace de travail en ajoutant le dossier du projet, puis enregistrez la configuration active dans un fichier d'espace de travail.

Un espace de travail n'est qu'une vue de dossier enregistrée, plus les réglages de l'éditeur pour le projet.

### Extensions

- **Installer des extensions** : Cliquez sur l'icône **Extensions** de la barre d'activité pour chercher et installer des outils utiles pour Scheme. La principale est **« vscode-scheme »** d'Allen Huang. Elle fournit la coloration syntaxique et une prise en charge de base de Scheme, le langage dans lequel les plug-ins Lumi sont écrits.

Vous pouvez aussi adapter l'extension en modifiant le fichier de langage, pour y ajouter d'autres mots-clés Scheme ou tout ce qui touche aux scripts Lumi.

Si vous tenez votre propre bibliothèque d'aide, ajouter les noms de fonctions au surligneur rend la navigation et le remaniement plus rapides.

### Conclusion

Choisissez un éditeur qui rend faciles la recherche, le remaniement et les petites corrections, avec l'IA à portée de main. Cursor plus une extension Scheme est la base utilisée ici.
