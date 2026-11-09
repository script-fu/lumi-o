---
title: "Eine Linux-Systemübersicht"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux ist ein leistungsstarkes und vielseitiges Betriebssystem mit einer großen Entwicklergemeinschaft. Im Kern besteht ein Linux-System aus mehreren Schlüsselkomponenten, die zusammenarbeiten, um ein reibungsloses Benutzererlebnis zu bieten. Dieser Überblick stellt die wesentlichen Teile eines Linux-Systems vor: Kernel, Distribution, Paketmanager, Display-Manager, Desktop-Umgebung und Display-Server (X11 oder Wayland).

Lumi wird auf Debian mit Cinnamon (X11) entwickelt und getestet. Das bleibt die Entwicklungsumgebung. GNOME unter Wayland wurde daneben getestet, auf das allgemeine Erscheinungsbild der Anwendung und auf das OpenGL-Rendering.

**Typische Standardkonfigurationen gängiger Linux-Distributionen**

| **Distribution**   | **Paketmanager** | **Display-Manager** | **Desktop-Umgebung** | **Display-Server** |
|--------------------|------------------|---------------------|----------------------|--------------------|
| Debian             | APT              | GDM                 | GNOME                | Wayland            |
| Ubuntu             | APT              | GDM                 | GNOME                | Wayland            |
| Debian             | APT              | GDM                 | Cinnamon             | X11                |
| Fedora             | DNF              | GDM                 | GNOME                | Wayland            |
| Arch Linux         | Pacman           | Nach Wahl           | Nach Wahl            | Nach Wahl          |

### Schlüsselbegriffe

#### Kernel

Der Kern des Betriebssystems, der direkt mit der Hardware kommuniziert — in der Regel der Linux-Kernel.

#### Distribution

Eine Linux-Distribution bündelt den Kernel mit Werkzeugen, Bibliotheken und Software im User Space. Beispiele: Debian, Arch Linux und Fedora.

#### Paketmanager

Werkzeug zum Installieren, Aktualisieren und Entfernen von Anwendungen aus Repositorys. Beispiele: APT für Debian-basierte Distributionen, DNF für Fedora und Pacman für Arch Linux.

#### Display-Manager

Verwaltet den grafischen Anmeldebildschirm und den Sitzungsstart. Beispiele: GDM (GNOME Display Manager), LightDM und SDDM (Simple Desktop Display Manager).

#### Desktop-Umgebung

Stellt die grafische Benutzeroberfläche (GUI) bereit und steuert Erscheinungsbild sowie Benutzererfahrung. Beispiele: GNOME, Cinnamon und KDE Plasma.

#### Display-Server

Verwaltet Bildschirmausgabe und Eingabeereignisse. Beispiele: X11 (X Window System) und Wayland. X11 ist ein etablierter Display-Server; Wayland ist eine neuere, sicherere Alternative.
