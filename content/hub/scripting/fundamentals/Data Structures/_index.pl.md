---
title: "Struktury danych"
type: docs
weight: 3
---
W schemacie **struktury danych** są niezbędnymi narzędziami do organizowania, przechowywania i manipulowania danymi. Umożliwiają programistom tworzenie wydajnych, czytelnych i nadających się do ponownego użycia skryptów. Wybierając odpowiednią strukturę danych dla konkretnego problemu, możesz zoptymalizować zarówno wydajność, jak i przejrzystość swojego kodu.

## Kluczowe struktury danych w schemacie

Scheme udostępnia kilka potężnych i wszechstronnych struktur danych, z których każda jest dostosowana do określonych zadań. Podstawowe struktury danych obejmują:

### Listy
Listy to uporządkowane zbiory elementów, które mogą dynamicznie rosnąć lub kurczyć się. Idealnie nadają się do danych sekwencyjnych lub hierarchicznych i są szeroko stosowane w programowaniu funkcjonalnym.

Kluczowe cechy:
- Rozmiar dynamiczny.
- Elementy mogą być typów mieszanych.
- Powszechnie używane w algorytmach rekurencyjnych i reprezentujących struktury drzewiaste.

Przykłady użycia:
- Zarządzanie zbiorami przedmiotów.
- Reprezentowanie sekwencji lub hierarchii.

---

### Wektory
Wektory to zbiory elementów o stałym rozmiarze, indeksowane w celu szybkiego dostępu. Najlepiej nadają się do scenariuszy, w których wydajność i dostęp pozycyjny mają kluczowe znaczenie.

Kluczowe cechy:
- Naprawiono rozmiar przy tworzeniu.
- Dostęp do elementów uzyskuje się poprzez ich indeks.
- Szybciej niż listy w przypadku niektórych operacji, takich jak dostęp losowy.

Przykłady użycia:
- Przechowywanie konfiguracji lub danych o stałym rozmiarze.
- Szybkie wyszukiwania i aktualizacje na podstawie pozycji.

---

### Wybór właściwej struktury danych

Decyzja o użyciu **listy** lub **wektora** zależy od konkretnych potrzeb Twojego skryptu. Oto kilka wskazówek:

| Funkcja | Listy | Wektory |
|-------------------------|--------------------------------------------|--------------------------------|
| **Elastyczność rozmiaru** | Dynamiczny | Naprawiono |
| **Szybkość dostępu** | Wolniejsze (dostęp sekwencyjny) | Szybciej (dostęp indeksowany) |
| **Łatwość modyfikacji**| Łatwiej | Trudniejsze (wymaga realokacji)|
| **Przypadki użycia** | Dane dynamiczne, rekurencja | Dane statyczne, szybkie wyszukiwania |

---