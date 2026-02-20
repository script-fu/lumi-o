---
title: "Przeglądarka procedur"
type: docs
---
Przeglądarka procedur jest głównym narzędziem referencyjnym umożliwiającym odkrywanie setek funkcji dostępnych w proceduralnej bazie danych Lumi (PDB). Ponieważ każde narzędzie, filtr i skrypt w Lumi musi być zarejestrowany w PDB, aby można było go wywołać, ta przeglądarka jest w rzeczywistości kompletnym eksploratorem PDB.

## Otwieranie przeglądarki procedur

Przejdź do **Pomoc → Programowanie → Przeglądarka procedur**.

Dostęp do niego można także uzyskać z konsoli schematów poprzez **Przeglądaj**.

## Co pokazuje

Przeglądarka procedur może wyświetlić listę wszystkich procedur aktualnie zarejestrowanych w PDB, niezależnie od ich pochodzenia. Domyślnie szukane jest słowo „wewnętrzne”, aby wyświetlić wewnętrznie zarejestrowane podstawowe procedury.

- **Procedury wewnętrzne**: Podstawowe funkcje manipulacji obrazem, zarządzania warstwami i kontroli narzędzi.
- **Wtyczki zewnętrzne**: Procedury udostępniane przez skompilowane wtyczki C/C++ lub trwałe rozszerzenia.

## Wyszukiwanie i filtrowanie

- **Pole wyszukiwania**: Filtruje procedury według nazwy, opisu lub autora. Wyczyszczenie pola wyszukiwania powoduje wyświetlenie wszystkich dostępnych procedur.
- **Typ wyszukiwania**: Lista rozwijana wyszukiwania umożliwia filtrowanie według określonych pól. Jeśli ustawisz opcję **według typu** i wyszukasz „wewnętrzne”, lista zawęzi się i wyświetli tylko wewnętrznie zarejestrowane podstawowe procedury.
- **Widok szczegółowy**: Kliknięcie procedury powoduje wyświetlenie jej parametrów, zwracanych wartości, autora, daty i opisu działania.

Jest to niezbędne do znalezienia dokładnej nazwy i podpisu argumentu funkcji, którą chcesz wywołać ze swojego skryptu.