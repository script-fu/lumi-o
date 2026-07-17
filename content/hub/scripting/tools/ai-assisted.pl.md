---
title: "Rozwój wspomagany sztuczną inteligencją"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Nowoczesne narzędzia AI mogą znacznie przyspieszyć rozwój wtyczek Lumi, pełniąc rolę współpracującego partnera w zakresie kodowania.

## VS Code w trybie Agent

Korzystanie z Visual Studio Code z asystentem AI w **trybie agenta** (takim jak tryb agenta GitHub Copilot lub inni asystenci z obsługą narzędzi) umożliwia wykonywanie złożonych, wieloetapowych zadań przy użyciu języka naturalnego.

Zamiast wypełniać tylko jedną linię kodu, Agent może:
- odczytać cały obszar roboczy, aby zrozumieć kontekst
- tworzyć nowe pliki i katalogi
- uruchamiać polecenia terminala, aby testować lub walidować skrypty
- wyszukiwać istniejące wzorce w bazie kodu

## Dostęp do repozytorium

Pomoc AI jest najskuteczniejsza, gdy Agent ma dostęp do **lumi-dev** lub repozytorium konkretnego projektu. Dzięki wglądowi w istniejącą bazę kodu Agent może:
- używać **[Bibliotek narzędzi]({{< ref "/hub/scripting/reference/utility-browser" >}})** jako odniesienia do funkcji pomocniczych
- stosować istniejące wzorce operacji GEGL i zarządzania warstwami
- ponownie wykorzystywać boilerplate z ustalonych wtyczek

## Przykładowy przepływ pracy

Możesz bezpośrednio poprosić Agenta o wygenerowanie pełnej wtyczki, opisując pożądany wynik funkcjonalny:

> „Korzystając z dostępnych narzędzi Scheme i przykładów w obszarze roboczym, napisz nową wtyczkę, która utworzy poziomą prowadnicę o kącie 50% na aktywnym obrazie i nazwie ją „Przewodnik środkowy”.”

Agent wyszuka sposób tworzenia przewodników, zidentyfikuje właściwą funkcję narzędzia (np. `lumi-image-add-hguide-percent` z `common.scm`) i wygeneruje kompletny plik `.scm` z prawidłowym szablonem rejestracji.

## Najlepsze praktyki

- **Określ szczegółowo**: opisz dokładnie, co chcesz, aby wtyczka robiła.
- **Narzędzia referencyjne**: Zachęć agenta, aby zajrzał do katalogu `share/lumi/scripts/` w celu uzyskania pomocników wysokiego poziomu.
- **Przejrzyj i przetestuj**: Zawsze testuj wtyczkę wygenerowaną przez sztuczną inteligencję, często jest to proces iteracyjny i kreatywny.
