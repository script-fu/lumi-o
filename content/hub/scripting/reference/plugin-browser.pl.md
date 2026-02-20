---
title: "Przeglądarka wtyczek"
type: docs
---
Przeglądarka wtyczek umożliwia przeglądanie systemu menu i sprawdzanie, gdzie zainstalowane są określone wtyczki.

## Otwieranie przeglądarki wtyczek

Przejdź do **Pomoc → Programowanie → Przeglądarka wtyczek**.

## Co pokazuje

Podczas gdy Przeglądarka Procedury koncentruje się na surowych *funkcjach* w PDB, Przeglądarka Wtyczek jest widokiem podzbioru skupiającym się na odkrywaniu interfejsu użytkownika. W szczególności filtruje plik PDB, aby pokazać „rzeczy wyglądające jak wtyczki zainstalowane w menu”.

Wewnętrznie używa zapytania, które zwraca tylko procedury, które mają zarówno powiązany plik na dysku, jak i zarejestrowaną ścieżkę menu.

- **Drzewo menu**: Pokazuje drzewiastą reprezentację struktury menu Lumi.
- **Lokalizacja wtyczek**: Pomaga znaleźć miejsce w menu, w którym znajduje się nowo zainstalowana wtyczka.
- **Metadane**: Pokazuje informacje o autorze, wersji i dacie wtyczki.

## Użycie

Użyj przeglądarki wtyczek, jeśli wiesz, że funkcja istnieje, ale nie możesz jej znaleźć w menu, lub gdy projektujesz własną wtyczkę i chcesz zobaczyć, gdzie znajdują się podobne narzędzia.