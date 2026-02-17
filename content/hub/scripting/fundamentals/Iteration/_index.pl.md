---
title: "Iteracja"
type: docs
weight: 4
---
Iteracja jest podstawą programowania, umożliwiając skryptom powtarzanie działań i wydajne przetwarzanie zbiorów danych. W Scheme, opartym na języku programowania Scheme, iteracja zapewnia narzędzia do automatyzacji powtarzalnych zadań, manipulowania strukturami danych i tworzenia wyrafinowanych wzorców wykonania.

### Rola iteracji w schemacie

Iteracja spełnia w skryptach kilka podstawowych celów:
- **Automatyczne powtarzanie:** Umożliwia wielokrotne wykonanie tej samej akcji lub zestawu działań bez duplikowania kodu.
- **Większa wydajność:** Dzięki iteracyjnemu przetwarzaniu struktur danych skrypty mogą systematycznie obsługiwać operacje na dużą skalę.
- **Usprawnienie kodu:** Iteracja eliminuje nadmiarowość, czyniąc kod bardziej zwięzłym, czytelnym i łatwym w utrzymaniu.

### Dostępne typy iteracji

Scheme oferuje kilka konstrukcji do iteracji, każdy dostosowany do konkretnych potrzeb:
- **map:** Stosuje funkcję do każdego elementu listy, zwracając nową listę z wynikami.
- **for-each:** Podobny do `map`, ale używany do wykonywania funkcji na każdym elemencie bez zwracania wyniku.
- **do:** Konstrukcja pętli ogólnego przeznaczenia, która obsługuje szeroką gamę procesów iteracyjnych.
- **rekurencja:** potężna technika, w której funkcje wywołują same siebie w celu przyrostowego rozwiązywania problemów.

### Jak działa iteracja

Iteracja zazwyczaj obejmuje:
1. **Definiowanie powtórzenia:** Określenie akcji do powtórzenia oraz danych lub zakresu do przetworzenia.
2. **Wykonywanie sekwencyjne:** Powtarzanie akcji dla każdego elementu, kroku lub warunku aż do zakończenia.
3. **Zwracanie wyniku (opcjonalnie):** W zależności od konstrukcji iteracja może dać wynik lub zmodyfikować stan.

Konstrukcje te umożliwiają pisanie elastycznych, wydajnych i eleganckich skryptów, które z łatwością radzą sobie ze złożonymi zadaniami.