---
title: "Git"
type: docs
---
Utilisez Git pour suivre les modifications apportées à vos plug-ins, annuler les erreurs et partager du code entre les machines.

## Pourquoi organiser votre code ?

Une fois que vous disposez de plusieurs scripts, une structure de dossiers cohérente permet de gagner du temps et simplifie le contrôle de version.

## Configuration d'une structure de dossiers de codes

L'un des moyens les plus simples d'organiser vos projets consiste à créer un **dossier de code** dédié sur votre ordinateur local. Dans ce dossier, vous pouvez créer des sous-dossiers pour chaque projet ou référentiel. Voici une structure de dossiers recommandée :

```plaintext
/home/your-username/code/
  ├── project1/
  ├── project2/
  └── project3/
```

Chaque sous-dossier (par exemple, `project1`) représente un **dépôt**, dans lequel vous stockerez les fichiers et le code de ce projet.

## Qu'est-ce qu'un référentiel ?

Un **dépôt** (ou **repo**) est essentiellement un dossier dont le contenu est suivi par Git. Lorsque vous créez un dépôt localement, vous initialisez Git dans ce dossier, vous permettant d'enregistrer toutes les modifications apportées à un clone en ligne.

### Dépôts locaux et distants

- **Local Repo** : Il s'agit du référentiel stocké sur votre ordinateur, dans l'un de vos dossiers de projet.
- **Remote Repo** : Une version du référentiel stockée en ligne (par exemple, sur GitLab ou GitHub).

## Utilisation de Git et GitHub

Une fois votre structure de dossiers en place, vous pouvez initialiser Git et connecter vos projets locaux à GitHub. Suivez ces étapes pour commencer :

### Étapes de base pour utiliser Git et GitHub

1. **Installer Git**
2. **Créez un compte GitHub**
3. **Créez un référentiel vierge sur GitHub**
4. **Initialisez Git dans votre projet local**
5. **Connectez votre dépôt local à GitHub**
6. **Mettez en scène vos fichiers**
7. ** Validez vos modifications **
8. **Poussez vos modifications vers GitHub**
9. **Consultez votre référentiel en ligne**

### 1. Installez Git

Si vous n'avez pas encore installé Git, vous pouvez le faire sous Linux en utilisant :

```sh
sudo apt install git
```

### 2. Créez un compte GitHub

Si vous n'avez pas déjà de compte, visitez [GitHub](https://github.com/) pour vous inscrire. Une fois inscrit, vous pouvez créer des référentiels sur GitHub pour stocker votre code en ligne.

### 3. Créez un référentiel vierge sur GitHub

1. **Connectez-vous à GitHub** : accédez à [GitHub](https://github.com/) et connectez-vous à votre compte.
2. **Créer un nouveau référentiel** :
   - Cliquez sur l'icône **** dans le coin supérieur droit et sélectionnez **Nouveau référentiel**.
   - Entrez un nom de référentiel (par exemple, `your-repository`).
   - Ajoutez une description si vous le souhaitez.
   - Choisissez une visibilité **Public** ou **Privée**.
   - **Ne pas** initialiser le référentiel avec un README, `.gitignore` ou une licence (pour éviter les conflits).
   - Cliquez sur **Créer un référentiel**.

### 4. Initialisez Git dans votre projet local

Pour commencer à suivre un dossier de projet avec Git, ouvrez votre terminal, accédez au dossier du projet et exécutez :

```sh
cd code/your/project/folder
git init
```

Cette commande initialise un référentiel Git vide dans votre dossier de projet.

### 5. Connectez votre dépôt local à GitHub

Ensuite, vous souhaiterez connecter votre référentiel local à GitHub. Après avoir créé un dépôt vierge sur GitHub, ajoutez-le en tant que dépôt distant à votre projet local :

```sh
cd code/your/project/folder
git remote add origin https://github.com/your-username/your-repository.git
```

Remplacez `your-username` et `your-repository` par votre nom d'utilisateur GitHub réel et le nom du référentiel. Cette commande relie votre projet local au référentiel distant sur GitHub.

### 6. Organisez vos fichiers

Avant de pouvoir enregistrer vos modifications dans Git, vous devez indiquer à Git quels fichiers vous avez modifiés et souhaitez enregistrer. C'est ce qu'on appelle la « mise en scène » de vos fichiers. Utilisez la commande suivante pour transférer tous les fichiers modifiés ou nouveaux :

```sh
git add .
```Cela indique à Git de suivre les modifications que vous avez apportées à tous les fichiers de votre projet. Vous pouvez également préparer des fichiers spécifiques en remplaçant `.` par le nom du fichier.

### 7. Validez vos modifications

Après la préparation, l'étape suivante consiste à enregistrer (ou « valider ») les modifications dans votre référentiel Git local. Lors de la validation, vous devez toujours inclure un message décrivant les modifications que vous avez apportées. Par exemple :

```sh
git commit -m "Add new feature"
```

Le drapeau `-m` vous permet d'écrire un message résumant les modifications que vous avez apportées. Ce message vous aide, ainsi que les autres, à comprendre ce qui a été modifié dans ce commit.

### 8. Transférez vos modifications vers GitHub

Une fois que vous avez validé les modifications localement, vous pouvez maintenant les « pousser » vers GitHub afin que votre référentiel distant soit mis à jour. Exécutez la commande suivante pour importer vos modifications :

```sh
git push -u origin main
```

La branche `main` est la branche par défaut de GitHub où le code est stocké, et cette commande télécharge vos modifications locales dans le référentiel distant, les rendant accessibles en ligne.

### 9. Affichez votre code sur GitHub

Une fois que vous avez transféré votre code vers GitHub, vous pouvez afficher votre référentiel dans l'interface Web de GitHub. Vous devriez voir les fichiers de votre dépôt local, ainsi qu'un historique de validation montrant les modifications que vous avez apportées.

## Conclusion

En organisant votre code dans des dossiers dédiés et en utilisant GitHub pour gérer et sauvegarder vos référentiels, vous garderez vos projets bien structurés et facilement accessibles. Une fois que vous disposez d’une version fonctionnelle de votre code, transférez-la vers GitHub. Vous pouvez ensuite facilement suivre toutes les modifications à l'aide de l'interface Web GitHub ou de Visual Studio Code, qui met en évidence les lignes modifiées. Cette approche vous permet de continuer à affiner et à développer votre code sans perdre la trace des progrès ou des modifications.

Git et les plateformes comme GitHub et GitLab sont des outils puissants et, même s'ils peuvent être complexes, de nombreuses ressources sont disponibles en ligne pour vous aider à mieux les comprendre. L'une des ressources les plus précieuses que j'ai trouvées sont les aides à l'IA comme ChatGPT. Vous pouvez décrire ce que vous devez accomplir et ces outils vous guideront patiemment tout au long du processus, étape par étape.

## Glossaire

Voici quelques termes courants que vous rencontrerez lorsque vous travaillerez avec Git et GitHub :- **Commit** : un instantané de vos modifications dans le référentiel. Chaque validation comprend un message décrivant ce qui a été modifié et crée un enregistrement historique auquel vous pouvez vous référer ou revenir ultérieurement.
- **Dépôt (Repo)** : Une collection de fichiers et leur historique suivis par Git. Les référentiels peuvent exister localement sur votre ordinateur ou à distance sur des plateformes comme GitHub. Chaque projet est généralement stocké dans son propre référentiel.
- **Remote** : Un référentiel distant est une version de votre projet hébergée sur une plateforme comme GitHub. La version locale de votre projet sur votre ordinateur est liée à cette télécommande afin que vous puissiez télécharger (push) et télécharger (pull) les modifications.
- **Staging** : processus de préparation des fichiers pour une validation. Lorsque vous préparez un fichier, vous indiquez à Git que vous souhaitez l'inclure dans la prochaine validation. La mise en scène vous permet de choisir les modifications à inclure dans une validation.
- **Push** : action d'envoyer vos modifications validées depuis votre référentiel local vers un référentiel distant (par exemple, GitHub), afin que d'autres puissent accéder à la version mise à jour de votre code.
- **Pull** : action de récupérer les modifications depuis un référentiel distant pour mettre à jour votre copie locale. Vous extrayez les modifications lorsque vous souhaitez synchroniser votre référentiel local avec la dernière version de la télécommande.
- **Origin** : Le nom par défaut d'un référentiel distant lorsque vous connectez pour la première fois votre référentiel local à un référentiel distant. Fait généralement référence à l'URL principale de votre projet sur GitHub.