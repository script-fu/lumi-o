---
title: "Przegląd systemu Linux"
type: docs
url: "hub/install-linux/A-Linux-System-Overview"
---
Linux to potężny i wszechstronny system operacyjny z ogromną społecznością programistów. W swej istocie system Linux składa się z kilku kluczowych komponentów, które współpracują ze sobą, zapewniając płynną obsługę użytkownika. W tym przeglądzie zostaną przedstawione podstawowe części systemu Linux, w tym jądro, dystrybucja, menedżer pakietów, menedżer wyświetlania, środowisko graficzne i serwer wyświetlania (X11 lub Wayland).

**Przykłady konfiguracji dystrybucji Linuksa**

| **Dystrybucja** | **Menedżer pakietów** | **Menedżer wyświetlania** | **Środowisko pulpitu** | **Serwer wyświetlacza** |
|-----------------------|----------------------|----------------------|----------------------------------|----------------------------------|
| Debiana | APT | GDM | GNOME | X11 |
| Debiana | APT | LightDM | Cynamon | X11 |
| Testowanie Debiana | APT | GDM | GNOME | Waylanda |
| Fedora | DNF | GDM | GNOME | Waylanda |
| Arch Linux | Pacmana | SDDM | Plazma KDE | X11 |

### Kluczowe terminy

#### Jądro
Rdzeń systemu operacyjnego, który łączy się bezpośrednio ze sprzętem, zwykle Linux.

#### Dystrybucja
Dystrybucja Linuksa, która pakuje jądro wraz z narzędziami, bibliotekami i oprogramowaniem przestrzeni użytkownika. Przykładami są Debian, Arch Linux i Fedora.

#### Menedżer pakietów
Narzędzie używane do instalowania, aktualizowania i usuwania aplikacji z repozytoriów. Przykłady obejmują APT dla dystrybucji opartych na Debianie, DNF dla Fedory i Pacman dla Arch Linux.

#### Menedżer wyświetlania
Zarządza graficznym ekranem logowania i inicjowaniem sesji. Przykłady obejmują GDM (menedżer wyświetlania GNOME), LightDM i SDDM (prosty menedżer wyświetlania pulpitu).

#### Środowisko pulpitu
Zapewnia graficzny interfejs użytkownika (GUI) i zarządza ogólnym wyglądem i doświadczeniem użytkownika. Przykładami są GNOME, Cinnamon i KDE Plasma.

#### Serwer wyświetlania
Zarządza zdarzeniami wyjściowymi i wejściowymi wyświetlacza. Przykładami są X11 (system X Window) i Wayland. X11 to tradycyjny serwer wyświetlania, natomiast Wayland to nowsza, bezpieczniejsza alternatywa.