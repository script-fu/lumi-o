---
title: "Översikt över ett Linux-system"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux är ett kraftfullt och mångsidigt operativsystem med en stor utvecklargemenskap. I grunden består ett Linux-system av flera viktiga komponenter som samverkar för en smidig användarupplevelse. Den här översikten beskriver de väsentliga delarna: kärnan, distributionen, pakethanteraren, display manager, skrivbordsmiljö och display server (X11 eller Wayland).

Lumi fungerar bäst på Debian med Cinnamon (X11) och utvecklas och testas i den miljön.

**Vanliga standardval i Linux-distributioner**

| **Distribution** | **Pakethanterare** | **Display Manager** | **Skrivbordsmiljö** | **Display Server** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | Användarval          | Användarval             | Användarval        |

### Nyckeltermer

#### Kärna

Kärnan i operativsystemet som kommunicerar direkt med hårdvaran — vanligtvis Linux.

#### Distribution

Linux-distributionen paketerar kärnan tillsammans med användarutrymmesverktyg, bibliotek och programvara. Exempel: Debian, Arch Linux och Fedora.

#### Pakethanterare

Ett verktyg för att installera, uppdatera och ta bort program från repositoryer. Exempel: APT för Debian-baserade distributioner, DNF för Fedora och Pacman för Arch Linux.

#### Display Manager

Hanterar den grafiska inloggningsskärmen och sessionsstart. Exempel: GDM (GNOME Display Manager), LightDM och SDDM (Simple Desktop Display Manager).

#### Skrivbordsmiljö

Tillhandahåller det grafiska användargränssnittet (GUI) och hanterar utseende och användarupplevelse. Exempel: GNOME, Cinnamon och KDE Plasma.

#### Display Server

Hanterar skärmutdata och inmatningshändelser. Exempel: X11 (X Window System) och Wayland. X11 är en traditionell display server; Wayland är ett nyare, säkrare alternativ.
