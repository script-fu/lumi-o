---
title: "Obszary robocze"
type: docs
---
Obszar roboczy to zapisana migawka całego środowiska interfejsu użytkownika: które panele są otwarte i gdzie, dekoracje płótna i dopełnienie zarówno w widoku normalnym, jak i pełnoekranowym, aktywny motyw i zestaw ikon, układ przybornika, aktywna paleta i ustawienia narzędzi. Lumi pozwala zapisać dowolną liczbę nazwanych obszarów roboczych i błyskawicznie się między nimi przełączać — wszystkie otwarte obrazy są aktualizowane na miejscu, bez konieczności ponownego uruchamiania.

## Co oszczędza obszar roboczy

Każdy nazwany obszar roboczy przechowuje niezależnie następujące informacje:

| Składnik | Co obejmuje |
| :--- | :--- |
| **Układ** | Położenie i rozmiar okna, układ doków (kolumny lewego i prawego panelu, które panele są otwarte i w jakiej kolejności), tryb jednego lub wielu okien, stan zmaksymalizowany, widoczność i położenie paska zakładek |
| **Opcje narzędzia** | Bieżące ustawienia dla każdego narzędzia (rozmiar pędzla, twardość, zachowanie przy wypaczaniu itp.) |
| **Urządzenia wejściowe** | Konfiguracja urządzenia wejściowego: krzywe ciśnienia, przypisania przycisków, mapowania osi dla rysika i innych urządzeń |
| **Dekoracje na płótnie** | Domyślne ustawienia linijek, pasków przewijania, linii pomocniczych, siatki, podświetlenia zaznaczenia, granicy warstwy i granicy obszaru roboczego dla każdego obszaru roboczego — ustawiane za pomocą **Preferencje → Okna obrazu → Wygląd domyślny** i **Wygląd pełnego ekranu**, niezależnie dla widoku normalnego i pełnego ekranu |
| **Wyściółka z płótna** | Tryb i kolor dopełniania obszaru roboczego dla widoku normalnego i pełnoekranowego — ustaw za pomocą **Preferencje → Okna obrazu → Wygląd domyślny** |
| **Motyw i ikony** | Aktywny motyw, ciemny/jasny wariant kolorów, zestaw ikon, zastąpienie rozmiaru ikon i skala czcionki |
| **Przybornik** | Pozycja widżetu FG/BG (góra/dół/lewo/prawo), skala FG/BG, widoczność maskotki Wilbera, nagłówki grup narzędzi |

Aktywna **paleta** i **ustawienie narzędzia** są również rejestrowane dla każdego obszaru roboczego i przywracane po przełączeniu.

> **Dekoracje płócienne i wyściółka** są kontrolowane przez
> **Preferencje → Okna obrazu → Zaawansowane opcje okna → Wygląd domyślny** (Widok normalny)
> i **Wygląd pełnoekranowy** (Widok pełnoekranowy). Dostosuj te ustawienia do swoich upodobań,
> następnie zapisz obszar roboczy. Elementy menu **Widok** (linijki, linie pomocnicze itp.) są dostępne lokalnie
> bieżącego okna obrazu i nie są zapisywane dla poszczególnych obszarów roboczych.

### Aktualizacje na żywo na przełączniku

Kiedy przełączasz obszary robocze, wszystkie otwarte okna obrazów są natychmiast aktualizowane — linijki, linie pomocnicze, paski przewijania, kolor wypełnienia i wszystkie inne ustawienia widoku zmieniają się na miejscu, bez konieczności zamykania i ponownego otwierania obrazów.

## Dostęp

**Edycja → Preferencje → Przestrzeń robocza**

Górna część strony preferencji obszaru roboczego zawiera listę wszystkich zapisanych obszarów roboczych i umożliwia zarządzanie nimi.

## Tworzenie obszaru roboczego

Skonfiguruj panele, narzędzia i paletę dokładnie tak, jak chcesz, a następnie:

1. Otwórz **Edycja → Preferencje → Przestrzeń robocza**.
2. Kliknij **Zapisz układ jako…**.
3. Wpisz nazwę i kliknij **Zapisz**.

Nowy obszar roboczy pojawi się na liście rozwijanej **Aktywny układ** i w menu **Windows**.

## Przełączanie obszarów roboczych

Istnieją dwa sposoby przełączania:

- **Menu Windows**: Nazwy układów pojawiają się w obszarze **Okna → Układ**, co umożliwia szybki dostęp z obszaru roboczego.
- **Preferencje → Przestrzeń robocza**: Wybierz układ z listy rozwijanej **Aktywny układ** i kliknij **Załaduj ponownie układ**.

Przełączanie jest natychmiastowe — Lumi przebudowuje układ panelu, przywraca opcje narzędzi, ponownie ładuje ustawienia urządzenia, aktualizuje dekoracje płótna, wyściółkę, motyw i układ skrzynki narzędziowej, a wszystko to bez ponownego uruchamiania.

## Zarządzanie obszarami roboczymi

Z **Edycja → Preferencje → Przestrzeń robocza**:| Akcja | Efekt |
| :--- | :--- |
| **Zapisz układ** | Zastępuje bieżący obszar roboczy bieżącymi ustawieniami. |
| **Zapisz układ jako…** | Tworzy nowy nazwany obszar roboczy na podstawie bieżących ustawień. |
| **Zmień nazwę układu…** | Zmienia nazwę wybranego obszaru roboczego. |
| **Załaduj ponownie układ** | Natychmiast stosuje wybrany obszar roboczy. |
| **Usuń układ…** | Trwale usuwa wybrany obszar roboczy i jego pliki. |

## Ustawienia trwałości

Dolna część strony preferencji Workspace kontroluje, co Lumi zapisuje automatycznie:

- **Zapisz pozycje okien przy wyjściu**: Gdy opcja ta jest włączona, pozycje doków i okien są zapisywane na dysku przy każdym wyjściu.
- **Otwórz okna na tym samym monitorze**: Ponownie otwiera każde okno na monitorze, na którym było włączone podczas ostatniej sesji.
- **Zapisz opcje narzędzia przy wyjściu**: Zapisuje bieżące ustawienia narzędzia przy wychodzeniu.
- **Zapisz ustawienia urządzenia wejściowego przy wyjściu**: zapisuje konfigurację rysika i urządzenia po wyjściu.

Te ustawienia dotyczą poszczególnych obszarów roboczych — każdy układ niezależnie zachowuje swój zapisany stan.

## Przykładowe przepływy pracy

Kilka sposobów, w jakie artyści mogą wykorzystywać wiele przestrzeni roboczych:

- **Malowanie** — duże doki pędzli, ciepły kolor wypełnienia (ustawiany w Preferencjach → Okna obrazu → Wygląd domyślny), preferowany wariant motywu
- **Inking** — linie pomocnicze i obramowanie obszaru roboczego włączone, paski przewijania włączone (ustawiane w Preferencjach → Wygląd domyślny), neutralny kolor wypełnienia
- **Roughs** — doki ukryte, bez linijek i siatki, ciemne wypełnienie, niewielki rozmiar ikon, aby zmaksymalizować przestrzeń płótna
- **Ostrość na pełnym ekranie** — różne ustawienia kolorów wypełnienia i dekoracji w przypadku wyglądu pełnoekranowego i wyglądu domyślnego, więc przełączanie trybu pełnoekranowego zapewnia naprawdę inne środowisko pracy
- **Skrypty** — panel skryptów otwarty, zwiększenie rozmiaru czcionki w celu zwiększenia czytelności, inny zestaw ikon