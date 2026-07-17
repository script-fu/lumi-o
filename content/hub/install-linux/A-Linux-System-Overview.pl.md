---
title: "Przegląd systemu Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 57573497133d7364dcc0acf023b2cb3b098b2a931ed245e846a539b96adb78b0
---

Linux to potężny i wszechstronny system operacyjny z ogromną społecznością programistów. W swej istocie składa się z kilku kluczowych komponentów, które współpracują, zapewniając płynną obsługę. Ten przegląd opisuje podstawowe elementy systemu Linux: jądro, dystrybucję, menedżer pakietów, menedżer wyświetlania, środowisko graficzne i serwer wyświetlania (X11 lub Wayland).

Lumi działa najlepiej na Debianie z Cinnamon (X11) i jest rozwijany oraz testowany w tym środowisku.

**Typowe domyślne ustawienia dystrybucji Linux**

| **Dystrybucja** | **Menedżer pakietów** | **Menedżer wyświetlania** | **Środowisko pulpitu** | **Serwer wyświetlania** |
|--------------------|----------------------|----------------------|-------------------------|--------------------|
| Debian             | APT                  | GDM                  | GNOME                   | Wayland            |
| Ubuntu             | APT                  | GDM                  | GNOME                   | Wayland            |
| Debian             | APT                  | GDM                  | Cinnamon                | X11                |
| Fedora             | DNF                  | GDM                  | GNOME                   | Wayland            |
| Arch Linux         | Pacman               | Wybór użytkownika    | Wybór użytkownika       | Wybór użytkownika  |

### Kluczowe terminy

#### Jądro

Rdzeń systemu operacyjnego, który łączy się bezpośrednio ze sprzętem — zwykle Linux.

#### Dystrybucja

Dystrybucja Linuksa pakuje jądro wraz z narzędziami, bibliotekami i oprogramowaniem przestrzeni użytkownika. Przykłady: Debian, Arch Linux i Fedora.

#### Menedżer pakietów

Narzędzie do instalowania, aktualizowania i usuwania aplikacji z repozytoriów. Przykłady: APT w dystrybucjach opartych na Debianie, DNF w Fedorze i Pacman w Arch Linux.

#### Menedżer wyświetlania

Zarządza graficznym ekranem logowania i uruchamianiem sesji. Przykłady: GDM (GNOME Display Manager), LightDM i SDDM (Simple Desktop Display Manager).

#### Środowisko pulpitu

Zapewnia graficzny interfejs użytkownika (GUI) i zarządza wyglądem oraz doświadczeniem użytkownika. Przykłady: GNOME, Cinnamon i KDE Plasma.

#### Serwer wyświetlania

Zarządza wyjściem obrazu i zdarzeniami wejściowymi. Przykłady: X11 (X Window System) i Wayland. X11 to tradycyjny serwer wyświetlania; Wayland to nowsza, bezpieczniejsza alternatywa.
