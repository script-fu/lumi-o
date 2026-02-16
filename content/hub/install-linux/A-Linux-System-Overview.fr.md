---
title: "Un aperçu du système Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux est un système d'exploitation puissant et polyvalent doté d'une vaste communauté de développeurs. À la base, un système Linux se compose de plusieurs composants clés qui fonctionnent ensemble pour offrir une expérience utilisateur transparente. Cet aperçu décrira les parties essentielles d'un système Linux, notamment le noyau, la distribution, le gestionnaire de packages, le gestionnaire d'affichage, l'environnement de bureau et le serveur d'affichage (X11 ou Wayland).

**Exemples de configurations de distribution Linux**

| **Distribution** | **Gestionnaire de paquets** | **Gestionnaire d'affichage** | **Environnement de bureau** | **Serveur d'affichage** |
|------------------------|----------------------|----------------------|-------------------------|----------------------------------|
| Debian | APT | GDM | GNOME | X11 |
| Debian | APT | LumièreDM | Cannelle | X11 |
| Tests Debian | APT | GDM | GNOME | Wayland |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arch Linux | Pacman | SDDM | KDE Plasma | X11 |

### Termes clés

#### Noyau
Le cœur du système d'exploitation qui s'interface directement avec le matériel, généralement Linux.

#### Répartition
La distribution Linux, qui regroupe le noyau ainsi que les outils, bibliothèques et logiciels de l'espace utilisateur. Les exemples incluent Debian, Arch Linux et Fedora.

#### Gestionnaire de paquets
Un outil utilisé pour installer, mettre à jour et supprimer des applications logicielles des référentiels. Les exemples incluent APT pour les distributions basées sur Debian, DNF pour Fedora et Pacman pour Arch Linux.

#### Gestionnaire d'affichage
Gère l'écran de connexion graphique et le lancement de la session. Les exemples incluent GDM (GNOME Display Manager), LightDM et SDDM (Simple Desktop Display Manager).

#### Environnement de bureau
Fournit l’interface utilisateur graphique (GUI) et gère l’apparence globale et l’expérience utilisateur. Les exemples incluent GNOME, Cinnamon et KDE Plasma.

#### Serveur d'affichage
Gère les événements de sortie et d’entrée d’affichage. Les exemples incluent X11 (X Window System) et Wayland. X11 est un serveur d'affichage traditionnel, tandis que Wayland est une alternative plus récente et plus sécurisée.