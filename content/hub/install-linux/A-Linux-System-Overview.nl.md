---
title: "Overzicht van een Linux-systeem"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux is een krachtig en veelzijdig besturingssysteem met een grote community van ontwikkelaars. In de kern bestaat een Linux-systeem uit verschillende belangrijke componenten die samenwerken voor een naadloze gebruikerservaring. Dit overzicht beschrijft de essentiële onderdelen van een Linux-systeem: de kernel, distributie, pakketbeheerder, display manager, desktopomgeving en display server (X11 of Wayland).

Lumi werkt het best op Debian met Cinnamon (X11) en wordt in die omgeving ontwikkeld en getest.

**Veelvoorkomende standaardinstellingen van Linux-distributies**

| **Distributie** | **Pakketbeheer** | **Display Manager** | **Desktopomgeving** | **Display Server** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | Keuze van gebruiker  | Keuze van gebruiker     | Keuze van gebruiker |

### Belangrijke termen

#### Kernel

De kern van het besturingssysteem die rechtstreeks met de hardware communiceert, meestal Linux.

#### Distributie

De Linux-distributie bundelt de kernel met gebruikersruimte-tools, bibliotheken en software. Voorbeelden zijn Debian, Arch Linux en Fedora.

#### Pakketbeheerder

Een tool om software uit repository's te installeren, bij te werken en te verwijderen. Voorbeelden zijn APT voor Debian-gebaseerde distributies, DNF voor Fedora en Pacman voor Arch Linux.

#### Display Manager

Beheert het grafische inlogscherm en het starten van sessies. Voorbeelden zijn GDM (GNOME Display Manager), LightDM en SDDM (Simple Desktop Display Manager).

#### Desktopomgeving

Biedt de grafische gebruikersinterface (GUI) en beheert het uiterlijk en de gebruikerservaring. Voorbeelden zijn GNOME, Cinnamon en KDE Plasma.

#### Display Server

Beheert schermuitvoer en invoergebeurtenissen. Voorbeelden zijn X11 (X Window System) en Wayland. X11 is een traditionele display server; Wayland is een nieuwer, veiliger alternatief.
