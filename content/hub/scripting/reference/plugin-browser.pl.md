---
title: "Przeglądarka wtyczek"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ffbf087ea102e00b7057bf6bad9b6e2cb8f75ad05c7f26f0f2818d10f34392ce
url: "hub/scripting/reference/plugin-browser"
---
Przeglądarka wtyczek umożliwia przeglądanie systemu menu i sprawdzanie, gdzie zainstalowane są określone wtyczki.

## Otwieranie przeglądarki wtyczek

Przejdź do **Pomoc → Programowanie → Przeglądarka wtyczek**.

## Co pokazuje

Podczas gdy **Przeglądarka procedur** koncentruje się na surowych *funkcjach* w PDB, **Przeglądarka wtyczek** to widok podzbioru skupiający się na odkrywaniu interfejsu użytkownika. W szczególności filtruje plik PDB, aby pokazać „rzeczy wyglądające jak wtyczki zainstalowane w menu”.

Wewnętrznie używa zapytania, które zwraca tylko procedury, które mają zarówno powiązany plik na dysku, jak i zarejestrowaną ścieżkę menu.

- **Drzewo menu**: Pokazuje drzewiastą reprezentację struktury menu Lumi.
- **Lokalizacja wtyczek**: Pomaga znaleźć miejsce w menu, w którym znajduje się nowo zainstalowana wtyczka.
- **Metadane**: Pokazuje informacje o autorze, wersji i dacie wtyczki.

## Użycie

Użyj przeglądarki wtyczek, jeśli wiesz, że funkcja istnieje, ale nie możesz jej znaleźć w menu, lub gdy projektujesz własną wtyczkę i chcesz zobaczyć, gdzie znajdują się podobne narzędzia.
