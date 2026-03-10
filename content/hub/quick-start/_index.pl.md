---
title: "Szybki start"
type: docs
---
Lumi nie został jeszcze wydany, jest dostępny w wersji rozwojowej.

Jeśli już korzystasz z Linuksa i chcesz szybko uruchomić Lumi, skorzystaj z najnowszego **rozwojowego AppImage** z artefaktów GitLab:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Pobierz najnowszy plik zip z artefaktem AppImage.
2. Wyciągnij zamek.
3. Kliknij dwukrotnie plik `Lumi*.AppImage`, aby go uruchomić.

AppImage powinien już działać. Jeśli tak nie jest, włącz opcję **Zezwalaj na wykonywanie pliku jako programu** w uprawnieniach pliku lub użyj poniższej metody terminala.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Konfiguracja firmy Wacom w systemie Linux

Do malowania cyfrowego w Lumi zazwyczaj najlepsza jest prosta **liniowa konfiguracja ciśnienia**:

- Utrzymuj liniową krzywą ciśnienia sterownika tabletu.
- Zachowaj liniowość krzywych ciśnienia/wejściowych w Lumi.
- Kształtuj wrażenie za pomocą samego pędzla, ponieważ dynamika pędzla może być nieliniowa.

Możesz sprawdzić i zresetować krzywą sterownika systemu Linux za pomocą:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Praktyczne wskazówki:

— Lumi obecnie blokuje problematyczne wejście panelu dotykowego/pierścienia dotykowego Wacom, aby uniknąć usterek X11. Zamiast tego przypisz przyciski tabletu do **względnego** rozmiaru pędzla w górę/w dół.
- Jeśli przeciąganie rozmiaru pędzla za pomocą `Alt` nie działa, Twój pulpit może używać `Alt` do przenoszenia okien. Zmień skrót menedżera okien na `Super` lub wyłącz go.

Jeśli chcesz pracować z kodu źródłowego, przejdź do [Technical Guides](/hub/technical-guides/) i [Installation](/hub/technical-guides/Installation/).