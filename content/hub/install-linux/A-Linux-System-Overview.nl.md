---
title: "Een Linux-systeemoverzicht"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux is een krachtig en veelzijdig besturingssysteem met een enorme gemeenschap van ontwikkelaars. In de kern bestaat een Linux-systeem uit verschillende belangrijke componenten die samenwerken om een ​​naadloze gebruikerservaring te bieden. Dit overzicht schetst de essentiële onderdelen van een Linux-systeem, inclusief de kernel, distributie, pakketbeheerder, displaymanager, desktopomgeving en displayserver (X11 of Wayland).

**Voorbeelden van Linux-distributieconfiguraties**

| **Distributie** | **Pakketbeheer** | **Displaybeheer** | **Desktopomgeving** | **Weergaveserver** |
|------------------|---------------------|-------------------|----------------------|-----------------|
| Debian | APT | GDM | GNOME | X11 |
| Debian | APT | LichtDM | Kaneel | X11 |
| Debian-testen | APT | GDM | GNOME | Wayland |
| Fedora | DNF | GDM | GNOME | Wayland |
| Boog Linux | Pacman | SDDM | KDE-plasma | X11 |

### Sleutelbegrippen

#### Kernel
De kern van het besturingssysteem die rechtstreeks communiceert met de hardware, meestal Linux.

#### Distributie
De Linux-distributie, die de kernel samen met gebruikersruimtetools, bibliotheken en software verpakt. Voorbeelden hiervan zijn Debian, Arch Linux en Fedora.

#### Pakketbeheerder
Een tool die wordt gebruikt om softwareapplicaties uit repository's te installeren, bij te werken en te verwijderen. Voorbeelden hiervan zijn APT voor op Debian gebaseerde distributies, DNF voor Fedora en Pacman voor Arch Linux.

#### Weergavebeheer
Beheert het grafische inlogscherm en sessie-initiatie. Voorbeelden hiervan zijn GDM (GNOME Display Manager), LightDM en SDDM (Simple Desktop Display Manager).

#### Desktopomgeving
Biedt de grafische gebruikersinterface (GUI) en beheert het algehele uiterlijk en de gebruikerservaring. Voorbeelden hiervan zijn GNOME, Cinnamon en KDE Plasma.

#### Weergaveserver
Beheert de weergave-uitvoer en invoergebeurtenissen. Voorbeelden hiervan zijn X11 (X Window System) en Wayland. X11 is een traditionele weergaveserver, terwijl Wayland een nieuwer, veiliger alternatief is.