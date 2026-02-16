---
title: "Sauvegarde du système à l'aide de Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
---
Il est courant de sauvegarder vos fichiers importants pour revenir à des versions antérieures ou remplacer des données corrompues. Cependant, un autre type de sauvegarde essentiel est un **clone de disque**, une sauvegarde complète de l'état de votre système.

Une fois que votre système est configuré et fonctionne correctement, la création d’une sauvegarde complète est cruciale pour restaurer votre environnement en cas de catastrophe. Cette sauvegarde complète la sauvegarde régulière de vos données de travail.

[Clonezilla](https://clonezilla.org/) est un logiciel d'imagerie et de clonage de disque gratuit et open source. Il permet aux utilisateurs de créer et de restaurer des sauvegardes complètes du disque dur de leur ordinateur, ce qui en fait un outil populaire auprès des professionnels de l'informatique et des utilisateurs particuliers.

Il est toujours préférable d'avoir une sauvegarde et de ne pas en avoir besoin plutôt que d'avoir besoin d'une sauvegarde et de ne pas l'avoir.


## Principales caractéristiques de Clonezilla

- **Imagerie disque** : Clonezilla crée une copie exacte d'un disque dur, y compris le système d'exploitation, les applications et les données.
- **Sauvegarde et restauration** : Il vous permet de créer une image de sauvegarde d'un disque dur et de la restaurer en cas de panne ou de migration vers un nouveau disque.
- **Gratuit et Open Source** : Clonezilla est entièrement gratuit et le code source est disponible pour modification et personnalisation.


## Utiliser Clonezilla pour sauvegarder

### Étapes de préparation

Vous aurez besoin d'une clé USB pour Clonezilla et d'un disque dur externe plus grand que le disque interne que vous avez l'intention de cloner.

Ces étapes simplifient le processus basé sur le [official guide](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). C'est une bonne idée de consulter le guide complet, qui comprend des captures d'écran pour plus de clarté.

1. **Créez une clé USB ou un CD/DVD Clonezilla Live** : suivez les instructions détaillées sur le Clonezilla [website](https://clonezilla.org/liveusb.php) pour créer une clé USB ou un CD/DVD amorçable.

2. **Connectez votre disque de sauvegarde externe** : branchez votre disque externe et assurez-vous qu'il est reconnu par votre système. Ce sera la destination de votre sauvegarde.

3. **Vérifiez la disposition de votre partition** : utilisez la commande `lsblk` dans un terminal pour vérifier la disposition des partitions de votre disque dur principal. Notez le nom du périphérique principal.

4. **Démarrer à partir de la clé USB Clonezilla Live** : Redémarrez votre ordinateur et démarrez à partir du support Clonezilla que vous avez créé. Vous devrez peut-être accéder aux paramètres BIOS/UEFI (généralement en appuyant sur F2, F12, ESC ou DEL pendant le démarrage) et ajuster l'ordre de démarrage pour donner la priorité à la clé USB.



### Sauvegarde avec Clonezilla

1. **Sélectionnez le mode de sauvegarde** : Une fois Clonezilla démarré, choisissez le mode "périphérique-périphérique". Ce mode vous permet de cloner directement votre disque interne sur un périphérique externe.

2. **Sélectionnez le périphérique source** : Choisissez le lecteur interne principal.

3. **Sélectionnez le périphérique cible** : Choisissez votre lecteur de sauvegarde externe comme périphérique cible. Soyez prudent lors de la sélection de l'appareil pour éviter d'écraser des données importantes. Assurez-vous que le lecteur cible est de taille égale ou supérieure à celle du lecteur source.

4. **Démarrez le processus de sauvegarde** : Clonezilla démarrera le processus de sauvegarde. Selon la taille de votre partition et la vitesse de vos disques, cela peut prendre de plusieurs minutes à quelques heures.

5. **Étiquetez votre sauvegarde** : Une fois la sauvegarde terminée, étiquetez la clé USB et le disque dur externe avec la date et le système que vous avez sauvegardé. Conservez-les dans un endroit sûr.

---

### Restauration à partir d'une sauvegarde

Si vous devez restaurer votre système Debian à partir de la sauvegarde, procédez comme suit :

1. **Démarrer à partir de Clonezilla Media** : Insérez le Clonezilla USB et démarrez à partir de celui-ci, en suivant les mêmes étapes que lors du processus de sauvegarde.2. **Sélectionnez le mode de restauration** : choisissez à nouveau le mode "appareil-appareil", mais cette fois, vous restaurerez à partir de l'image de sauvegarde. Cela copiera toutes les données de votre disque externe sur votre disque interne.

3. **Sélectionnez le périphérique source** : Choisissez votre disque externe sur lequel la sauvegarde est stockée.

4. **Sélectionnez le périphérique cible** : sélectionnez le lecteur interne sur lequel vous souhaitez restaurer la sauvegarde.

5. **Démarrez le processus de restauration** : Clonezilla commencera le processus de restauration. Comme pour la sauvegarde, le temps requis dépendra de la taille du disque et de la vitesse de votre matériel.

---

## Notes finales

Les sauvegardes de disque avec Clonezilla garantissent que l'ensemble de votre système (système d'exploitation, paramètres et applications) est préservé. Avec un minimum d'effort, vous pouvez protéger votre système contre une panne catastrophique et minimiser les temps d'arrêt en cas de panne.

N'oubliez pas que **les sauvegardes sont essentielles**. Mettez régulièrement à jour vos sauvegardes et testez-les périodiquement pour vous assurer que vous pouvez restaurer votre système en cas de besoin.

Après le démarrage, vous pouvez brancher votre lecteur de sauvegarde externe et inspecter sa structure de partition à l'aide de l'utilitaire Disques sous Linux. Le disque de sauvegarde doit refléter la structure du disque interne, avec les mêmes partitions et un peu d'espace inutilisé si le disque externe est plus grand.