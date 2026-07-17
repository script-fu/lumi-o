---
title: "Un aperçu du système Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux est un système d'exploitation puissant et polyvalent, soutenu par une vaste communauté de développeurs. À la base, un système Linux regroupe plusieurs composants clés qui travaillent ensemble pour offrir une expérience utilisateur fluide. Cet aperçu présente les éléments essentiels d'un système Linux : le noyau, la distribution, le gestionnaire de paquets, le gestionnaire d'affichage, l'environnement de bureau et le serveur d'affichage (X11 ou Wayland).

Lumi est à son meilleur sur Debian avec Cinnamon (X11) et est développé et testé dans cet environnement.

**Réglages par défaut des principales distributions Linux**

| **Distribution**   | **Gestionnaire de paquets** | **Gestionnaire d'affichage** | **Environnement de bureau** | **Serveur d'affichage** |
|--------------------|-----------------------------|------------------------------|-----------------------------|-------------------------|
| Debian             | APT                         | GDM                          | GNOME                       | Wayland                 |
| Ubuntu             | APT                         | GDM                          | GNOME                       | Wayland                 |
| Debian             | APT                         | GDM                          | Cinnamon                    | X11                     |
| Fedora             | DNF                         | GDM                          | GNOME                       | Wayland                 |
| Arch Linux         | Pacman                      | Au choix                     | Au choix                    | Au choix                |

### Termes clés

#### Noyau

Le cœur du système d'exploitation, qui communique directement avec le matériel — en pratique, le noyau Linux.

#### Distribution

Une distribution Linux regroupe le noyau avec les outils, bibliothèques et logiciels de l'espace utilisateur. Exemples : Debian, Arch Linux et Fedora.

#### Gestionnaire de paquets

Outil utilisé pour installer, mettre à jour et supprimer des applications à partir de dépôts. Exemples : APT pour les distributions basées sur Debian, DNF pour Fedora et Pacman pour Arch Linux.

#### Gestionnaire d'affichage

Gère l'écran de connexion graphique et le démarrage de session. Exemples : GDM (GNOME Display Manager), LightDM et SDDM (Simple Desktop Display Manager).

#### Environnement de bureau

Fournit l'interface utilisateur graphique (GUI) et gère l'apparence générale ainsi que l'expérience utilisateur. Exemples : GNOME, Cinnamon et KDE Plasma.

#### Serveur d'affichage

Gère l'affichage et les événements de saisie. Exemples : X11 (X Window System) et Wayland. X11 est un serveur d'affichage traditionnel ; Wayland en est une alternative plus récente et plus sécurisée.
