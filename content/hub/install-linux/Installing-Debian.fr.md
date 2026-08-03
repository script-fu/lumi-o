---
title: "Installer Debian"
type: docs
url: "hub/install-linux/Installing-Debian"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e79ae25c72fd6b2a9d31e1efe3019289f4b44d9230990f6874c0332de6c5f19
---

Ce document décrit le processus utilisé pour installer Debian Stable comme système d'exploitation de développement Lumi-o. Il peut aussi servir à d'autres personnes qui mettent en place un environnement similaire.

Debian Stable a été choisie parce que Lumi vise à se compiler de manière fiable sur une plateforme prévisible et durable. Le développement de GIMP cible Debian Testing, ce qui fait de Debian Stable une base étroitement alignée.

Lumi est à son meilleur sur Debian avec Cinnamon (X11) et y est développé et testé. Cinnamon offre un flux de travail de bureau proche de Windows, tandis que X11 a fourni l'environnement le plus stable pour le développement de Lumi.

Si vous venez de Windows, le principal changement conceptuel est que la plupart des installations et configurations de logiciels passent par des gestionnaires de paquets et de simples commandes en terminal, plutôt que par des installateurs téléchargeables.

## À qui s'adresse ce guide

Ce guide documente une configuration Debian Stable éprouvée, utilisée pour le développement de Lumi. Il ne s'agit pas d'un tutoriel général d'installation Linux.

Il est particulièrement utile pour :

- les artistes qui quittent Windows et souhaitent une configuration Linux prévisible
- les développeurs qui compilent Lumi à partir des sources
- les utilisateurs qui préfèrent reproduire un environnement connu plutôt que de concevoir leur propre configuration système

Une connaissance de base du partitionnement de disque et de l'utilisation simple de la ligne de commande est supposée.

## Sauvegardez vos données

Avant d'installer Debian, créez une sauvegarde complète de votre répertoire personnel sur un disque externe. Incluez tous les dossiers de données supplémentaires que vous souhaitez conserver.

Remarque : sous Linux, `~` représente votre répertoire personnel.

Si vous utilisez des dépôts Git, poussez les modifications importantes vers leurs origines afin de pouvoir les restaurer facilement après l'installation. Cette étape ne concerne que les personnes qui utilisent déjà Git.

## Créer une partition

Libérez de l'espace sur votre disque principal pour Debian. De nombreux guides et outils existent pour cette étape, dont GParted. Selon votre configuration, vous pouvez :

- réduire une partition Windows existante pour un double démarrage
- réutiliser une partition Linux existante
- préparer de nouvelles partitions Linux et swap

En cas de doute, consultez des guides spécifiques à votre matériel avant toute modification : les étapes de partitionnement varient beaucoup d'un système à l'autre.


## Créer une clé USB d'installation Debian

En supposant qu'une partition cible et un espace swap existent déjà :

1. Téléchargez l'ISO Debian depuis le site officiel : https://www.debian.org/
2. Sous Windows, utilisez BalenaEtcher pour écrire l'ISO sur une clé USB.
3. Sous Linux, utilisez un outil en ligne de commande tel que `dd` pour créer une clé USB amorçable.

## Installer Debian

1. Insérez la clé USB.
2. Redémarrez et appuyez sur la touche du menu de démarrage (généralement `F2`, `F12`, `Esc` ou `Del`) pendant le démarrage.
3. Sélectionnez le périphérique USB.
4. Choisissez un installateur non graphique.
5. Laissez le mot de passe root vide lorsque vous y êtes invité, afin que l'installateur accorde l'accès sudo à votre compte utilisateur.
6. Partitionnez manuellement :

   - Système de fichiers : ext4 (journalisation)
   - Swap : partition swap existante
   - Point de montage : `/`
   - Libellé : `linux`
   - Nom d'hôte : nom du système affiché sous la forme `user@hostname`
   - Compte utilisateur : votre nom complet
   - Nom d'utilisateur : identifiant de connexion au terminal

7. L'installateur Debian propose un choix d'environnement de bureau à ce stade ; sélectionnez **Cinnamon** pour la configuration recommandée par Lumi.
8. Terminez l'installation et redémarrez dans Debian Stable.

## Configuration du système

### Mise à l'échelle de l'affichage

Debian Stable gère actuellement la mise à l'échelle fractionnaire de façon inégale, surtout sur les écrans 4K. Plutôt que de réduire la résolution d'affichage, ajustez directement les éléments de l'interface.

Ajustements recommandés :

- Évitez la mise à l'échelle fractionnaire de l'affichage.
- Menu → Sélection de police → Paramètres de police → Facteur de mise à l'échelle du texte : `2.5`
- Police du bureau : `14`
- Panneau → Personnaliser → Hauteur du panneau : `60`
- Apparence du panneau → Taille des icônes symboliques de la zone droite : `48px`
- Souris et pavé tactile → Taille du pointeur
- Bureau (clic droit) → Personnaliser → Taille d'icône plus grande

Ajustement de Firefox :

- Barre d'adresse → `about:config`
- Définissez `layout.css.devPixelsPerPx` sur `1`

### Terminal

Configurez les préférences du terminal :

1. Menu → Terminal → Édition → Préférences
2. Texte → Taille initiale : `140 columns`, `40 rows`
3. Texte → Police personnalisée : `Monospace 10`
4. Couleurs → Schémas intégrés → Solarized Dark

### Touche Alt pour redimensionner les outils

Si `Alt` + clic droit et glisser ne redimensionne pas les pinceaux dans Lumi, le bureau utilise Alt pour la gestion des fenêtres.

1. Recherchez **Fenêtres** dans le menu système.
2. Fenêtres → Comportement → Touche spéciale pour déplacer et redimensionner les fenêtres → **Désactivé**

Après cette modification, `Alt` + clic droit et glisser devrait fonctionner dans Lumi pour redimensionner les outils.

## Restaurer les données

Restaurez les fichiers sauvegardés dans le répertoire personnel selon vos besoins, par exemple :

- `Backup/Home/Artwork` → `~/Artwork`
- `Backup/Home/code` → `~/code`
- `Backup/Home/Desktop` → `~/Desktop`
- `Backup/Home/.ssh` → `~/.ssh`
- `Backup/Home/.config/lumi` → `~/.config/lumi`

Remarque : les dossiers commençant par `.` sont des répertoires de configuration masqués sous Linux.

## Facultatif : configuration de Git

Requis uniquement si vous prévoyez de compiler Lumi ou de restaurer des dépôts.

### Installer Git

```bash
sudo apt install git
```

Configurez votre identité :

```bash
git config --global --edit
```

#### Accès GitLab

Restaurez l'accès aux dépôts sur GitLab ou GitHub :

1. Modifiez les permissions du fichier de clé SSH : `chmod 600 ~/.ssh/id_rsa`
2. Ajoutez la clé à la nouvelle installation Git : `ssh-add ~/.ssh/id_rsa`
3. Testez la connexion : `ssh -T git@ssh.gitlab.gnome.org` ou `ssh -T git@github.com`

Pour chaque dépôt, récupérez les origines et réinitialisez la branche locale pour qu'elle corresponde :

```bash
git reset --hard remote-name/branch-name
git clean -df
```

Exécutez `git status` pour confirmer que les dépôts sont propres.

Vous disposez maintenant d'un nouveau système d'exploitation, avec vos données et dépôts restaurés. Cette configuration reflète un environnement éprouvé, utilisé pour le développement de Lumi, et peut être adaptée à votre propre flux de travail.

## Compiler Lumi après la configuration du système

Les scripts de compilation de Lumi se trouvent dans :

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
