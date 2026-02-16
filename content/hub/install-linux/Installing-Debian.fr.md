---
title: "Installer Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
---
Ce document décrit le processus utilisé pour installer Debian Stable comme système d'exploitation de développement Lumi·o. Cela peut être utile à d’autres créant un environnement similaire.

Debian Stable a été sélectionnée parce que Lumi vise à construire de manière fiable sur une plate-forme prévisible à long terme. Le développement de GIMP cible Debian Testing, faisant de Debian Stable un système de base étroitement aligné.

Si vous venez de Windows, le principal changement conceptuel est que la plupart des installations et configurations de logiciels s'effectuent via des gestionnaires de packages et de simples commandes de terminal plutôt que via des programmes d'installation téléchargeables.

## À qui s'adresse ce guide

Ce guide documente une configuration Debian Stable fonctionnelle utilisée pour le développement de Lumi. Il ne s'agit pas d'un didacticiel général d'installation de Linux.

Il est particulièrement utile pour :

- les artistes qui quittent Windows et qui souhaitent une configuration Linux prévisible
- les développeurs construisent Lumi à partir des sources
- les utilisateurs qui préfèrent reproduire un environnement de travail connu plutôt que de concevoir leur propre configuration système

Une connaissance de base du partitionnement de disque et de l'utilisation simple de la ligne de commande est supposée.

## Sauvegardez vos données

Avant d'installer Debian, créez une sauvegarde complète de votre répertoire personnel sur un disque externe. Incluez tous les dossiers de données supplémentaires que vous souhaitez conserver.

Remarque : Sous Linux, `~` représente votre répertoire personnel.

Si vous utilisez des référentiels Git, transférez toutes les modifications importantes à leurs origines afin qu'elles puissent être facilement restaurées après l'installation. Cette étape n'est pertinente que si vous utilisez déjà Git.

## Créer une partition

Créez de l'espace sur votre disque principal pour Debian. De nombreux guides et outils existent pour cette étape, dont GParted. En fonction de votre configuration, vous pouvez :

- réduire une partition Windows existante pour le double démarrage
- réutiliser une partition Linux existante
- préparer de nouvelles partitions Linux et swap

En cas de doute, consultez les guides spécifiques au matériel avant d'apporter des modifications, car les étapes de partitionnement varient considérablement d'un système à l'autre.


## Créer une clé USB d'installation Debian

En supposant qu'une partition cible et un espace de swap existent déjà :

1. Téléchargez l'ISO Debian depuis le site officiel : https://www.debian.org/
2. Sous Windows, utilisez BalenaEtcher pour écrire l'ISO sur une clé USB.
3. Sous Linux, utilisez un outil de ligne de commande tel que `dd` pour créer une clé USB amorçable.

## Installer Debian

1. Insérez la clé USB.
2. Redémarrez et appuyez sur la touche du menu de démarrage (généralement `F2`, `F12`, `Esc` ou `Del`) pendant le démarrage.
3. Sélectionnez le périphérique USB.
4. Choisissez un programme d'installation non graphique.
5. Laissez le mot de passe root vide lorsque vous y êtes invité afin que le programme d'installation accorde à sudo l'accès à votre compte utilisateur.
6. Partitionner manuellement :

   - Système de fichiers : ext4 (journalisation)
   - Swap : partition de swap existante
   - Point de montage : `/`
   - Libellé : `linux`
   - Nom d'hôte : nom du système affiché sous la forme `user@hostname`
   - Compte utilisateur : votre nom complet
   - Nom d'utilisateur : nom de connexion au terminal

7. Sélectionnez **Cinnamon** comme environnement de bureau.
8. Terminez l'installation et redémarrez dans Debian Stable.

## Configuration du système

### Mise à l'échelle de l'affichage

Debian Stable gère actuellement la mise à l'échelle fractionnaire de manière incohérente, en particulier sur les écrans 4K. Au lieu de réduire la résolution d’affichage, ajustez directement les éléments de l’interface.

Ajustements recommandés :- Évitez la mise à l'échelle de l'affichage fractionnaire.
- Menu → Sélection de police → Paramètres de police → Facteur de mise à l'échelle du texte : `2.5`
- Police de bureau : `14`
- Panneau → Personnaliser → Hauteur du panneau : `60`
- Apparence du panneau → Taille de l'icône symbolique de la zone droite : `48px`
- Souris et pavé tactile → Ajustement de la taille du pointeur
- Bureau (clic droit) → Personnaliser → Taille d'icône plus grande

Ajustement de Firefox :

- Barre d'adresse → `about:config`
- Définissez `layout.css.devPixelsPerPx` sur `1`

### Borne

Configurez les préférences du terminal :

1. Menu → Terminal → Modifier → Préférences
2. Texte → Taille initiale : `140 columns`, `40 rows`
3. Texte → Police personnalisée : `Monospace 10`
4. Couleurs → Schémas intégrés → Solarized Dark

## Restaurer les données

Restaurez les fichiers sauvegardés dans le répertoire d'accueil si nécessaire, par exemple :

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Remarque : les dossiers commençant par `.` sont des répertoires de configuration masqués sous Linux.

## Facultatif : configuration de Git

Requis uniquement si vous envisagez de créer Lumi ou de restaurer des référentiels.

### Installer Git

```bash
sudo apt install git
```

Configurez votre identité :

```bash
git config --global --edit
```

#### Accès à GitLab

Restaurez l'accès au référentiel à GitLab ou GitHub :

1. Modifiez les autorisations sur le fichier de clé SSH : `chmod 600 ~/.ssh/id_rsa`
2. Ajoutez l'utilisateur à la nouvelle installation Git : `ssh-add ~/.ssh/id_rsa`
3. Testez la connexion : `ssh -T git@ssh.gitlab.gnome.org` ou `ssh -T git@github.com`

Pour chaque référentiel, récupérez les origines et réinitialisez la branche locale pour qu'elle corresponde :

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Exécutez `git status` pour confirmer que les référentiels sont propres.

Nous avons maintenant un nouveau système d'exploitation avec toutes les données et référentiels restaurés. Cette configuration reflète un environnement de travail connu utilisé pour le développement de Lumi et peut être adaptée aux flux de travail individuels selon les besoins.

## Construire Lumi après la configuration du système d'exploitation

Les scripts de build Lumi se trouvent dans :

`~/code/lumi-dev/build/lumi/scripts`.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Install dependencies once
sudo bash lumi-install-packages.sh

# First full setup build
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Regular rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile path
bash lumi-build-script.sh --scope compile --dir lumi-dev

# Launch Lumi
bash lumi-launch-active.sh lumi-dev
```