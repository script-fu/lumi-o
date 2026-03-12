---
title: "Un aperçu du système Linux"
type: docs
---
Linux est un système d'exploitation puissant et polyvalent doté d'une vaste communauté de développeurs. À la base, un système Linux se compose de plusieurs composants clés qui fonctionnent ensemble pour offrir une expérience utilisateur transparente. Cet aperçu décrira les parties essentielles d'un système Linux, notamment le noyau, la distribution, le gestionnaire de packages, le gestionnaire d'affichage, l'environnement de bureau et le serveur d'affichage (X11 ou Wayland).

Lumi est à son meilleur sur Debian avec Cinnamon (X11) et est développé et testé dans cet environnement.

**Paramètres par défaut courants de la distribution Linux actuelle**

| **Distribution** | **Gestionnaire de paquets** | **Gestionnaire d'affichage** | **Environnement de bureau** | **Serveur d'affichage** |
|------------------------|----------------------|----------------------|-------------------------|----------------------------------|
| Debian | APT | GDM | GNOME | Wayland |
| Ubuntu | APT | GDM | GNOME | Wayland |
| Debian | APT | GDM | Cannelle | X11 |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arch Linux | Pacman | Choix de l'utilisateur | Choix de l'utilisateur | Choix de l'utilisateur |

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