---
title: "Sauvegarde du système avec Clonezilla"
type: docs
url: "hub/install-linux/System-Backup-Clonezilla"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: b3453289d7da56bb4fc9039616edb73e537acd6a722f0eb8a000e4a398016863
---

Il est courant de sauvegarder vos fichiers importants pour revenir à des versions antérieures ou remplacer des données corrompues. Un autre type de sauvegarde essentiel est toutefois le **clone de disque** : une copie complète de l'état de votre système.

Une fois votre système configuré et fonctionnel, une sauvegarde complète est cruciale pour restaurer votre environnement en cas de sinistre. Elle complète la sauvegarde régulière de vos données de travail.

[Clonezilla](https://clonezilla.org/) est un logiciel libre d'imagerie et de clonage de disques. Il permet de créer et de restaurer des sauvegardes complètes du disque dur d'un ordinateur, ce qui en fait un outil populaire aussi bien chez les professionnels de l'informatique que chez les particuliers.

Mieux vaut avoir une sauvegarde dont on n'a pas besoin que d'en avoir besoin sans en avoir.


## Principales fonctionnalités de Clonezilla

- **Imagerie de disque** : Clonezilla crée une copie exacte d'un disque dur, y compris le système d'exploitation, les applications et les données.
- **Sauvegarde et restauration** : vous pouvez créer une image de sauvegarde d'un disque dur et la restaurer en cas de panne ou de migration vers un nouveau disque.
- **Libre et open source** : Clonezilla est entièrement gratuit et son code source est disponible pour modification et personnalisation.


## Sauvegarder avec Clonezilla

### Étapes de préparation

Vous aurez besoin d'une clé USB pour Clonezilla et d'un disque dur externe plus grand que le disque interne que vous souhaitez cloner.

Ces étapes simplifient le processus à partir du [guide officiel](https://clonezilla.org//fine-print-live-doc.php?path=./clonezilla-live/doc/01_Save_disk_image/00-boot-clonezilla-live-cd.doc#00-boot-clonezilla-live-cd.doc). Il est recommandé de consulter le guide complet, qui inclut des captures d'écran pour plus de clarté.

1. **Créer une clé USB ou un CD/DVD Clonezilla Live** : suivez les instructions détaillées sur le [site Web](https://clonezilla.org/liveusb.php) de Clonezilla pour créer un support amorçable.

2. **Connecter le disque de sauvegarde externe** : branchez votre disque externe et vérifiez qu'il est reconnu par le système. Il servira de destination pour la sauvegarde.

3. **Vérifier la disposition des partitions** : utilisez la commande `lsblk` dans un terminal pour vérifier la disposition des partitions de votre disque principal. Notez le nom du périphérique principal.

4. **Démarrer depuis la clé USB Clonezilla Live** : redémarrez l'ordinateur et amorcez depuis le support Clonezilla que vous avez créé. Vous devrez peut-être accéder aux paramètres BIOS/UEFI (généralement en appuyant sur F2, F12, ESC ou DEL au démarrage) et ajuster l'ordre de démarrage pour prioriser la clé USB.



### Sauvegarde avec Clonezilla

1. **Sélectionner le mode de sauvegarde** : une fois Clonezilla démarré, choisissez le mode « device-device ». Ce mode permet de cloner directement le disque interne vers un périphérique externe.

2. **Sélectionner le périphérique source** : choisissez le disque interne principal.

3. **Sélectionner le périphérique cible** : choisissez le disque externe de sauvegarde comme périphérique cible. Soyez prudent lors de la sélection pour éviter d'écraser des données importantes. Vérifiez que le disque cible est au moins aussi grand que le disque source.

4. **Lancer la sauvegarde** : Clonezilla démarre le processus de sauvegarde. Selon la taille de la partition et la vitesse des disques, cela peut prendre de quelques minutes à plusieurs heures.

5. **Étiqueter la sauvegarde** : une fois la sauvegarde terminée, étiquetez la clé USB et le disque dur externe avec la date et le système sauvegardé. Rangez-les en lieu sûr.

---

### Restauration depuis une sauvegarde

Si vous devez restaurer votre système Debian depuis la sauvegarde, procédez comme suit :

1. **Démarrer depuis le support Clonezilla** : insérez la clé USB Clonezilla et amorcez depuis celle-ci, en suivant les mêmes étapes que pour la sauvegarde.

2. **Sélectionner le mode de restauration** : choisissez à nouveau le mode « device-device », mais cette fois pour restaurer depuis l'image de sauvegarde. Toutes les données seront recopiées du disque externe vers le disque interne.

3. **Sélectionner le périphérique source** : choisissez le disque externe sur lequel la sauvegarde est stockée.

4. **Sélectionner le périphérique cible** : sélectionnez le disque interne sur lequel vous souhaitez restaurer la sauvegarde.

5. **Lancer la restauration** : Clonezilla démarre le processus de restauration. Comme pour la sauvegarde, la durée dépend de la taille du disque et de la vitesse du matériel.

---

## Notes finales

Les sauvegardes de disque avec Clonezilla préservent l'ensemble du système — système d'exploitation, paramètres et applications. Avec un effort minimal, vous protégez votre système contre une panne catastrophique et réduisez les temps d'arrêt en cas de crash.

N'oubliez pas : **les sauvegardes sont essentielles**. Mettez-les à jour régulièrement et testez-les périodiquement pour vous assurer de pouvoir restaurer le système quand vous en avez besoin.

Après le démarrage, vous pouvez brancher le disque de sauvegarde externe et inspecter sa structure de partitions avec l'utilitaire Disques sous Linux. Le disque de sauvegarde doit reproduire la structure du disque interne, avec les mêmes partitions et un espace inutilisé si le disque externe est plus grand.
