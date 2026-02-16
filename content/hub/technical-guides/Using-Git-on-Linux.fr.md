---
title: "Utiliser Git sous Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
---
Bienvenue dans ce guide du débutant sur l'utilisation de Git sous Linux ! Ce guide est conçu pour vous aider à démarrer avec Git et GitLab, et pour vous fournir une compréhension de base de la façon d'utiliser ces outils.

## Présentation de Git

Le code utilisé pour créer des applications est conservé dans une collection de dossiers et de fichiers sur votre système. Git est une application qui nous permet de sauvegarder, partager et copier cette collection. Git est connu comme un système de contrôle de version qui vous permet de suivre les modifications apportées à votre code et de collaborer avec d'autres. C'est un outil puissant largement utilisé dans la communauté open source. GitLab est une plate-forme Web qui vous permet d'héberger et de gérer vos référentiels Git en ligne, facilitant ainsi la collaboration avec d'autres et le suivi des modifications apportées à votre code.

## Qu'est-ce qu'un référentiel ?

Un _repo_, abréviation de dépôt, est un dossier local géré par Git avec une copie en ligne. Un dépôt Git Lab est une collection de fichiers et de dossiers qui composent un projet. Il peut avoir des _branches_ qui sont des copies indépendantes du même projet. Une branche est une version distincte de votre projet qui vous permet d'apporter des modifications sans affecter la version principale. Ceci est utile pour tester de nouvelles fonctionnalités ou corriger des bugs sans perturber le projet principal. Il existe votre dépôt local, stocké sur votre disque dur, et le dépôt distant, stocké en ligne à l'aide de Git et GitLab.

## Utilisation de Git

Vous devrez installer Git sur votre système. Sur les systèmes basés sur Debian, vous pouvez utiliser la commande apt pour installer des packages logiciels. Dans ce cas, nous l'utilisons pour installer Git, qui est un package qui fournit le système de contrôle de version Git. La commande sudo donne au programme d'installation l'autorisation d'installer sur votre système.

```bash
 sudo apt install git
```

## Accéder à GitLab

Avant de pouvoir utiliser [GitLab](https://gitlab.com/users/sign_up), vous devrez créer un compte en visitant le site Web de GitLab et en complétant le processus d'inscription.

GitLab nécessite _SSH_ pour une communication sécurisée et authentifiée entre un client (vous, par exemple) et le serveur GitLab lors de l'exécution d'opérations Git telles que les référentiels _cloning_, _pushing_ et _fetching_. Le clonage consiste à créer une copie locale du dépôt, la récupération consiste à apporter toutes les modifications apportées dans le dépôt à votre copie locale et la transmission consiste à envoyer les modifications et le contenu au référentiel du serveur. SSH (Secure Shell) est un protocole réseau qui permet un accès à distance sécurisé et utilise des _paires de clés_ pour authentifier et établir des connexions sécurisées. Pour générer une paire de clés SSH, vous pouvez utiliser la commande ssh-keygen dans votre terminal.

```bash
 ssh-keygen
```

Spécifiez un nom de fichier ou utilisez le nom par défaut en appuyant sur Entrée, et éventuellement un mot de passe. Dans votre répertoire personnel, dans un dossier caché appelé .ssh, il y a maintenant deux fichiers id_rsa, si vous avez choisi les noms par défaut. Le fichier .pub est la clé publique et vous pouvez voir son contenu avec un éditeur de texte.

Connectez-vous à votre compte GitLab et accédez à vos paramètres utilisateur. Cliquez sur « Clés SSH » dans le menu de navigation de gauche. Copiez et collez votre clé publique dans le champ Clé et donnez à la clé un titre pertinent, comme PC@Home. Cliquez sur le bouton « Ajouter une clé » pour enregistrer la clé. Votre clé publique SSH est désormais ajoutée à votre compte GitLab et vous pouvez l'utiliser pour vous authentifier auprès des référentiels GitLab. Testez si vos clés et votre connexion fonctionnent avec la commande ssh -T pour voir un message de bienvenue de GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Commandes Git de baseMaintenant que Git est installé et que vous avez configuré votre clé SSH avec GitLab, passons en revue quelques commandes Git essentielles pour gérer les référentiels. Ces commandes vous aideront à travailler avec des projets existants, à les maintenir à jour et à apporter des modifications en toute sécurité.

### 1. **Clonage d'un référentiel**

Le clonage est le processus de création d'une copie locale d'un référentiel distant. Ceci est utile lorsque vous souhaitez travailler sur un projet qui existe déjà sur GitLab. Pour cloner un référentiel, utilisez la commande `git clone` suivie de l'URL du référentiel :

```sh
git clone https://gitlab.com/username/repository.git
```

Remplacez `https://gitlab.com/username/repository.git` par l'URL du référentiel que vous souhaitez cloner. Cette commande créera une copie locale du référentiel dans un nouveau répertoire.

### 2. **Vérification de l'état du référentiel**

Pour voir si votre référentiel local a des modifications ou pour afficher son état actuel, utilisez :

```sh
git status
```

Cette commande vous montrera quels fichiers ont été modifiés, ajoutés ou supprimés dans votre copie locale du référentiel.

### 3. **Dépôts distants**

Les référentiels distants sont des versions de votre projet hébergées en ligne, par exemple sur GitLab. Ils servent d’emplacement central où votre code est stocké et peut être consulté par d’autres. Le référentiel distant par défaut créé par Git lorsque vous clonez un projet s'appelle `origin`. Vous pouvez ajouter, supprimer ou répertorier des référentiels distants à l'aide des commandes suivantes :

- **Liste des télécommandes :**

  Pour voir quels référentiels distants sont liés à votre projet local, utilisez :

  ```sh
  git remote -v
  ```

  Cette commande répertorie toutes les télécommandes et leurs URL. En règle générale, vous verrez `origin` répertorié ici.

- **Ajout d'une télécommande :**

  Si vous devez ajouter un nouveau référentiel distant, vous pouvez le faire avec :

  ```sh
  git remote add <name> <url>
  ```

  Remplacez `<name>` par le nom de la télécommande et `<url>` par l'URL du référentiel.

- **Suppression d'une télécommande :**

  Pour supprimer un dépôt distant, utilisez :

  ```sh
  git remote remove <name>
  ```

  Remplacez `<name>` par le nom de la télécommande que vous souhaitez supprimer.

### 4. **Récupération des modifications depuis le référentiel distant**

Si vous souhaitez voir quelles modifications ont été apportées au référentiel distant sans les appliquer à votre copie locale, utilisez :

```sh
git fetch origin
```

Cette commande récupère les dernières modifications du référentiel distant mais ne les fusionne pas dans votre branche locale. C'est un moyen de vérifier les mises à jour avant de décider de les intégrer.

### 5. **Réinitialisation de votre référentiel local**

Si vous souhaitez réinitialiser votre référentiel local pour qu'il corresponde exactement au référentiel distant, vous pouvez utiliser une réinitialisation « matérielle ». **Avertissement :** Toutes les modifications locales que vous avez apportées seront écrasées.

```sh
git reset --hard origin/branch-name
```

Remplacez `branch-name` par le nom de la branche que vous souhaitez réinitialiser. Cette commande supprimera toutes les modifications locales et rendra votre référentiel local identique au référentiel distant.

### 6. **Affichage de l'historique des validations**

Pour voir une liste des modifications apportées au référentiel au fil du temps, utilisez :

```sh
git log
```

Cette commande affiche un historique des validations, y compris l'auteur, la date et le message pour chaque modification. C'est utile pour comprendre quels changements ont été apportés et quand.

### Résumé

Ces commandes Git de base vous aideront à travailler avec des référentiels, en gardant vos copies locales à jour et en garantissant que vous pouvez gérer en toute sécurité les référentiels distants. Le clonage de référentiels, la vérification de l'état de votre copie locale et la gestion de référentiels distants sont des compétences clés pour gérer des projets à l'aide de Git.