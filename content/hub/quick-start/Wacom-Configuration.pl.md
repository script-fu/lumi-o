---
title: "Konfiguracja Wacoma"
type: docs
url: "hub/quick-start/Wacom-Configuration"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3af66b116d9f361052280ac9636ae4b23bf5fc30f10f7227fb42d2d9e654ea95
---
Do malowania cyfrowego w Lumi zalecana jest prosta **liniowa konfiguracja ciśnienia**.

- Utrzymuj liniową krzywą ciśnienia sterownika tabletu.
- Utrzymuj krzywe ciśnienia/wejściowe w Lumi w większości liniowe.
- Kształtuj wrażenie za pomocą samego pędzla, ponieważ dynamika pędzla może już być nieliniowa.

Zalecamy zachowanie domyślnej krzywej ciśnienia liniowego na poziomie sterownika systemu operacyjnego. Łączenie wielu nieliniowych krzywych często prowadzi do nieprzewidywalnych zachowań wejściowych; zachowując neutralność sterownika, masz pewność, że wszelkie regulacje dokonane w Lumi-o pozostaną intuicyjne i powtarzalne. W razie potrzeby niewielka korekta globalnej krzywej Lumi może być nadal uzasadniona.

## Globalna krzywa rysika w Lumi

W Lumi otwórz:

Edycja → Preferencje → Urządzenia wejściowe → Skonfiguruj tablet, rysik i inne urządzenia...

Tutaj możesz ustawić globalną krzywą nacisku dla swojego rysika.

## Pierścień dotykowy Wacom

Lumi obsługuje teraz bezpośrednio wejście Wacom Touch Ring, w tym wejścia pierścieniowe oparte na modyfikatorach.

W tym samym oknie dialogowym konfiguracji urządzenia możesz przypisać działania pierścienia do każdego wejścia, w tym:

- Rozmiar pędzla
- Rozmiar względny pędzla
- Kąt pędzla
- Kąt widzenia
- Powiększenie widoku

Uwaga: obraz musi być aktywny, aby pierścień Touch Ring miał wpływ na atrybuty. Pierścień domyślnie wskazuje względną zmianę rozmiaru pędzla. Aby zapobiec przypadkowym zmianom, do uruchomienia polecenia wymagane jest przesunięcie półkola (np. przesunięcie półkola w prawo podwaja rozmiar pędzla).