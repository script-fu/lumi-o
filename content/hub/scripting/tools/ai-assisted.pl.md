---
title: "Rozwój wspomagany sztuczną inteligencją"
type: docs
---
Nowoczesne narzędzia AI mogą znacznie przyspieszyć rozwój wtyczek Lumi, pełniąc rolę współpracującego partnera w zakresie kodowania.

## Kod VS w trybie agenta

Korzystanie z Visual Studio Code z asystentem AI w **trybie agenta** (takim jak tryb agenta GitHub Copilot lub inni asystenci z obsługą narzędzi) umożliwia wykonywanie złożonych, wieloetapowych zadań przy użyciu języka naturalnego.

Zamiast wypełniać tylko jedną linię kodu, Agent może:
- Przeczytaj całe miejsce pracy, aby zrozumieć kontekst.
- Twórz nowe pliki i katalogi.
- Uruchom polecenia terminala, aby przetestować lub sprawdzić poprawność skryptów.
- Wyszukaj istniejące wzorce w swojej bazie kodu.

## Dostęp do repozytorium

Pomoc AI jest najskuteczniejsza, gdy Agent ma dostęp do **lumi-dev** lub repozytorium konkretnego projektu. Dzięki wglądowi w istniejącą bazę kodu Agent może:
- Użyj **[Utility Libraries](@@LUMI_TOKEN_4@@)** jako odniesienia do funkcji pomocniczych.
- Postępuj zgodnie z istniejącymi wzorcami operacji GEGL i zarządzania warstwami.
- Ponownie użyj standardowego kodu z ustalonych wtyczek.

## Przykładowy przepływ pracy

Możesz bezpośrednio poprosić Agenta o wygenerowanie pełnej wtyczki, opisując pożądany wynik funkcjonalny:

> „Korzystając z dostępnych narzędzi Scheme i przykładów w obszarze roboczym, napisz nową wtyczkę, która utworzy poziomą prowadnicę o kącie 50% na aktywnym obrazie i nazwie ją „Przewodnik środkowy”.”

Agent wyszuka sposób tworzenia przewodników, zidentyfikuje właściwą funkcję narzędzia (np. `lumi-image-add-hguide-percent` z `common.scm`) i wygeneruje kompletny plik `.scm` z prawidłowym schematem rejestracyjnym.

## Najlepsze praktyki

- **Określ szczegółowo**: opisz dokładnie, co chcesz, aby wtyczka robiła.
- **Narzędzia referencyjne**: Zachęć agenta, aby zajrzał do katalogu `share/lumi/scripts/` w celu uzyskania pomocników wysokiego poziomu.
- **Przejrzyj i przetestuj**: Zawsze testuj wtyczkę wygenerowaną przez sztuczną inteligencję, często jest to proces iteracyjny i kreatywny.