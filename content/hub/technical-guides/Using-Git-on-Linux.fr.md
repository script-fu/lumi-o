---
title: "Utiliser Git sous Linux"
type: docs
url: "hub/technical-guides/Using-Git-on-Linux"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7054a9ff9efeb93b4f494197e09ed1fe34d5d6bde7bc305480693c3982d375ae
---

Bienvenue dans ce guide pour débutants sur l'utilisation de Git sous Linux ! Ce guide vous aide à démarrer avec Git et GitLab et vous donne une compréhension de base de ces outils.

## Présentation de Git

Le code utilisé pour créer des applications est conservé dans une collection de dossiers et de fichiers sur votre système. Git est une application qui permet de sauvegarder, partager et copier cette collection. Git est un système de contrôle de version qui vous permet de suivre les modifications apportées à votre code et de collaborer avec d'autres. C'est un outil puissant, largement utilisé dans la communauté open source. GitLab est une plateforme web qui vous permet d'héberger et de gérer vos dépôts Git en ligne, facilitant la collaboration et le suivi des modifications.

## Qu'est-ce qu'un dépôt ?

Un _repo_, abréviation de repository (dépôt), est un dossier local géré par Git avec une copie en ligne. Un dépôt GitLab est une collection de fichiers et de dossiers qui composent un projet. Il peut avoir des _branches_, des copies indépendantes du même projet. Une branche est une version distincte de votre projet qui vous permet d'apporter des modifications sans affecter la version principale. C'est utile pour tester de nouvelles fonctionnalités ou corriger des bugs sans perturber le projet principal. Il y a votre dépôt local, stocké sur votre disque dur, et le dépôt distant, stocké en ligne via Git et GitLab.

## Utiliser Git

Vous devez installer Git sur votre système. Sur les systèmes basés sur Debian, vous pouvez utiliser la commande apt pour installer des paquets logiciels. Ici, nous l'utilisons pour installer Git, un paquet qui fournit le système de contrôle de version Git. La commande sudo donne au programme d'installation l'autorisation d'installer sur votre système.

```bash
 sudo apt install git
```

## Accéder à GitLab

Avant de pouvoir utiliser [GitLab](https://gitlab.com/users/sign_up), vous devez créer un compte en visitant le site web de GitLab et en complétant le processus d'inscription.

GitLab requiert _SSH_ pour une communication sécurisée et authentifiée entre un client (vous, par exemple) et le serveur GitLab lors d'opérations Git telles que le _clonage_, le _push_ et le _fetch_ de dépôts. Le clonage consiste à créer une copie locale du dépôt, le fetch consiste à récupérer les modifications du dépôt distant vers votre copie locale, et le push consiste à envoyer vos modifications vers le dépôt sur le serveur. SSH (Secure Shell) est un protocole réseau qui permet un accès distant sécurisé et utilise des _paires de clés_ pour authentifier et établir des connexions sécurisées. Pour générer une paire de clés SSH, utilisez la commande ssh-keygen dans votre terminal.

```bash
 ssh-keygen
```

Spécifiez un nom de fichier, ou utilisez le nom par défaut en appuyant sur Entrée, et éventuellement un mot de passe. Dans votre répertoire personnel, dans un dossier caché appelé .ssh, se trouvent maintenant deux fichiers id_rsa si vous avez conservé les noms par défaut. Le fichier .pub est la clé publique ; vous pouvez en consulter le contenu avec un éditeur de texte.

Connectez-vous à votre compte GitLab et accédez à vos paramètres utilisateur. Cliquez sur « SSH Keys » dans le menu de navigation de gauche. Copiez et collez votre clé publique dans le champ Key et donnez à la clé un titre pertinent, comme PC@Home. Cliquez sur le bouton « Add Key » pour enregistrer la clé. Votre clé publique SSH est maintenant ajoutée à votre compte GitLab et vous pouvez l'utiliser pour vous authentifier auprès des dépôts GitLab. Testez vos clés et votre connexion avec la commande ssh -T pour voir un message de bienvenue de GitLab.

```bash
 $ ssh -T git@ssh.gitlab.gnome.org
 Welcome to GitLab, @username!
```

## Commandes Git de base

Maintenant que Git est installé et que vous avez configuré votre clé SSH avec GitLab, passons en revue quelques commandes Git essentielles pour gérer les dépôts. Ces commandes vous aideront à travailler sur des projets existants, à les maintenir à jour et à apporter des modifications en toute sécurité.

### 1. **Cloner un dépôt**

Le clonage consiste à créer une copie locale d'un dépôt distant. C'est utile lorsque vous souhaitez travailler sur un projet qui existe déjà sur GitLab. Pour cloner un dépôt, utilisez la commande `git clone` suivie de l'URL du dépôt :

```sh
git clone https://gitlab.com/username/repository.git
```

Remplacez `https://gitlab.com/username/repository.git` par l'URL du dépôt que vous souhaitez cloner. Cette commande crée une copie locale du dépôt dans un nouveau répertoire.

### 2. **Vérifier l'état du dépôt**

Pour voir si votre dépôt local a des modifications ou pour afficher son état actuel, utilisez :

```sh
git status
```

Cette commande indique quels fichiers ont été modifiés, ajoutés ou supprimés dans votre copie locale du dépôt.

### 3. **Dépôts distants**

Les dépôts distants sont des versions de votre projet hébergées en ligne, par exemple sur GitLab. Ils servent d'emplacement central où votre code est stocké et accessible aux autres. Le dépôt distant par défaut créé par Git lorsque vous clonez un projet s'appelle `origin`. Vous pouvez ajouter, supprimer ou lister des dépôts distants avec les commandes suivantes :

- **Lister les remotes :**

  Pour voir quels dépôts distants sont liés à votre projet local, utilisez :

  ```sh
  git remote -v
  ```

  Cette commande liste tous les remotes et leurs URL. En règle générale, vous verrez `origin` listé ici.

- **Ajouter un remote :**

  Si vous devez ajouter un nouveau dépôt distant, utilisez :

  ```sh
  git remote add <name> <url>
  ```

  Remplacez `<name>` par un nom pour le remote et `<url>` par l'URL du dépôt.

- **Supprimer un remote :**

  Pour supprimer un dépôt distant, utilisez :

  ```sh
  git remote remove <name>
  ```

  Remplacez `<name>` par le nom du remote que vous souhaitez supprimer.

### 4. **Récupérer les modifications depuis le dépôt distant**

Si vous souhaitez voir quelles modifications ont été apportées au dépôt distant sans les appliquer à votre copie locale, utilisez :

```sh
git fetch origin
```

Cette commande récupère les dernières modifications du dépôt distant mais ne les fusionne pas dans votre branche locale. C'est un moyen de vérifier les mises à jour avant de décider de les intégrer.

### 5. **Réinitialiser votre dépôt local**

Si vous souhaitez réinitialiser votre dépôt local pour qu'il corresponde exactement au dépôt distant, vous pouvez utiliser un reset « hard ». **Avertissement :** cela écrase toutes les modifications locales que vous avez apportées.

```sh
git reset --hard origin/branch-name
```

Remplacez `branch-name` par le nom de la branche que vous souhaitez réinitialiser. Cette commande supprime toutes les modifications locales et rend votre dépôt local identique au dépôt distant.

### 6. **Consulter l'historique des commits**

Pour voir la liste des modifications apportées au dépôt au fil du temps, utilisez :

```sh
git log
```

Cette commande affiche un historique des commits, y compris l'auteur, la date et le message pour chaque modification. C'est utile pour comprendre quels changements ont été apportés et quand.

### Résumé

Ces commandes Git de base vous aideront à travailler avec des dépôts, à maintenir vos copies locales à jour et à gérer les dépôts distants en toute sécurité. Cloner des dépôts, vérifier l'état de votre copie locale et gérer les remotes sont des compétences essentielles pour gérer des projets avec Git.
