---
title: "Översikt över ett Linux-system"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a38745c31470f2662ed8b220af2cc2065e981b8c1435fce2efae41886a2a7996
---

Linux är ett kraftfullt och mångsidigt operativsystem med en stor utvecklargemenskap. I grunden består ett Linux-system av flera viktiga komponenter som samverkar för en smidig användarupplevelse. Den här översikten beskriver de väsentliga delarna: kärnan, distributionen, pakethanteraren, display manager, skrivbordsmiljö och display server (X11 eller Wayland).

Lumi utvecklas och testas på Debian med Cinnamon (X11). Det förblir utvecklingsmiljön. GNOME på Wayland har testats vid sidan av den, för programmets allmänna utseende och för OpenGL-rendering.

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

#### Skärmhanterare

Hanterar den grafiska inloggningsskärmen och sessionsstart. Exempel: GDM (GNOME Display Manager), LightDM och SDDM (Simple Desktop Display Manager).

#### Skrivbordsmiljö

Tillhandahåller det grafiska användargränssnittet (GUI) och hanterar utseende och användarupplevelse. Exempel: GNOME, Cinnamon och KDE Plasma.

#### Skärmserver

Hanterar skärmutdata och inmatningshändelser. Exempel: X11 (X Window System) och Wayland. X11 är en traditionell display server; Wayland är ett nyare, säkrare alternativ.
