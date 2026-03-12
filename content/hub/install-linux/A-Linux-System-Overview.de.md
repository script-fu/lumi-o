---
title: "Eine Linux-Systemübersicht"
type: docs
---
Linux ist ein leistungsstarkes und vielseitiges Betriebssystem mit einer großen Entwicklergemeinschaft. Im Kern besteht ein Linux-System aus mehreren Schlüsselkomponenten, die zusammenarbeiten, um ein nahtloses Benutzererlebnis zu bieten. In dieser Übersicht werden die wesentlichen Teile eines Linux-Systems beschrieben, einschließlich Kernel, Distribution, Paketmanager, Display-Manager, Desktop-Umgebung und Display-Server (X11 oder Wayland).

Lumi läuft unter Debian mit Cinnamon (X11) am besten und wurde in dieser Umgebung entwickelt und getestet.

**Allgemeine aktuelle Standardeinstellungen für die Linux-Distribution**

| **Verteilung** | **Paketmanager** | **Anzeigemanager** | **Desktop-Umgebung** | **Anzeigeserver** |
|------|-------|----------------------|-----------|------|
| Debian | APT | GDM | GNOME | Wayland |
| Ubuntu | APT | GDM | GNOME | Wayland |
| Debian | APT | GDM | Zimt | X11 |
| Fedora | DNF | GDM | GNOME | Wayland |
| Arch Linux | Pacman | Benutzerauswahl | Benutzerauswahl | Benutzerauswahl |

### Schlüsselbegriffe

#### Kernel

Der Kern des Betriebssystems, der direkt mit der Hardware, normalerweise Linux, verbunden ist.

#### Verteilung

Die Linux-Distribution, die den Kernel zusammen mit User-Space-Tools, Bibliotheken und Software paketiert. Beispiele hierfür sind Debian, Arch Linux und Fedora.

#### Paketmanager

Ein Tool zum Installieren, Aktualisieren und Entfernen von Softwareanwendungen aus Repositorys. Beispiele hierfür sind APT für Debian-basierte Distributionen, DNF für Fedora und Pacman für Arch Linux.

#### Display-Manager

Verwaltet den grafischen Anmeldebildschirm und die Sitzungsinitiierung. Beispiele hierfür sind GDM (GNOME Display Manager), LightDM und SDDM (Simple Desktop Display Manager).

#### Desktop-Umgebung

Stellt die grafische Benutzeroberfläche (GUI) bereit und verwaltet das Gesamterscheinungsbild und die Benutzererfahrung. Beispiele hierfür sind GNOME, Cinnamon und KDE Plasma.

#### Anzeigeserver

Verwaltet die Anzeigeausgabe- und Eingabeereignisse. Beispiele hierfür sind X11 (X Window System) und Wayland. X11 ist ein traditioneller Anzeigeserver, während Wayland eine neuere, sicherere Alternative ist.