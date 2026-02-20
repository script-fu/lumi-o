---
title: "Przeglądarka narzędziowa"
type: docs
---
Przeglądarka narzędzi umożliwia korzystanie z wbudowanego narzędzia Scheme stdlib dostarczanego z Lumi – bez konieczności opuszczania aplikacji i przeglądania plików źródłowych.

## Otwieranie przeglądarki narzędzi

Przejdź do **Pomoc → Programowanie → Przeglądarka narzędzi**.

Okno otwiera się natychmiast; nie trzeba wcześniej ładować wtyczki.

## Co pokazuje

Przeglądarka wyświetla listę wszystkich procedur, zmiennych i formularzy składni wyeksportowanych przez siedem bibliotek narzędzi, które Lumi ładuje automatycznie przy uruchomieniu:

| Biblioteka | Co obejmuje |
|---|---|
| `common.scm` | Pomocnicy ogólnego przeznaczenia (narzędzia łańcuchowe, liczbowe, listowe) |
| `files.scm` | Pomocnicy plików i ścieżek |
| `gegl.scm` | Bufor GEGL i pomocnicy kolorów |
| `images.scm` | Pomocnicy na poziomie obrazu (`image-get-open-list` itp.) |
| `layers.scm` | Pomocnicy warstw i rysunków |
| `parasites.scm` | Pomocnicy do odczytu/zapisu pasożytów |
| `paths.scm` | Pomocnicy ścieżek i wektorów |

Wszystko to jest dostępne w dowolnej wtyczce Scheme lub w konsoli Scheme.

## Wyszukiwanie i filtrowanie

- **Pole wyszukiwania** — filtruje według nazwy podczas pisania (dopasowanie podciągu bez uwzględniania wielkości liter).
- **Filtr rodzajowy** — zawęź wyniki do `procedure`, `variable` lub `syntax`.

Kliknięcie wpisu powoduje wyświetlenie jego pełnej dokumentacji i biblioteki, z której pochodzi.

## Stdlib jako opakowania

Biblioteki narzędziowe stanowią praktyczne zastosowanie wzorca zawijania: każdy pomocnik nadaje jasną nazwę operacji niskiego poziomu, ukrywa szablon i zapewnia jedno miejsce do aktualizacji, jeśli zmieni się podstawowe polecenie. Jeśli chcesz zrozumieć podejście do projektowania, które się za nimi kryje, zapoznaj się z samouczkiem **[Wrapping](@@LUMI_TOKEN_11@@)**.

## Związek z przeglądarką procedur

Przeglądarka narzędziowa jest odrębna od **Filtry → Script-Fu → Konsola → Przeglądaj** (przeglądarka procedur). Przeglądarka procedur zawiera listę procedur zarejestrowanych w PDB. Przeglądarka narzędzi wyświetla definicje pomocnicze, które celowo znajdują się *poza* plikiem PDB — dotyczą tylko schematu i nie mają powiązania z językiem C.