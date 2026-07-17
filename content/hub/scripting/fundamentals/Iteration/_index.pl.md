---
title: "Iteracja"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: df3e2118b9a580de4eed6ac56d9717aa3cbf555ab66bb49fabb4164b2994af91
url: "hub/scripting/fundamentals/Iteration/_index"
---
Iteracja to podstawa programowania — umożliwia skryptom powtarzanie działań i efektywne przetwarzanie zbiorów danych. W Scheme iteracja dostarcza narzędzi do automatyzacji powtarzalnych zadań, manipulacji strukturami danych i tworzenia zaawansowanych wzorców wykonania.

### Rola iteracji w Scheme

Iteracja pełni kilka istotnych funkcji w skryptach:
- **Automatyzacja powtórzeń:** Wykonywanie tej samej akcji lub zestawu akcji wielokrotnie bez duplikowania kodu.
- **Zwiększenie wydajności:** Iteracyjne przetwarzanie struktur danych pozwala systematycznie obsługiwać operacje na dużą skalę.
- **Usprawnienie kodu:** Iteracja eliminuje redundancję, czyniąc kod bardziej zwięzłym, czytelnym i łatwiejszym w utrzymaniu.

### Dostępne typy iteracji

Scheme oferuje kilka konstrukcji iteracyjnych, każdą dostosowaną do określonych potrzeb:
- **map:** Stosuje funkcję do każdego elementu listy, zwracając nową listę wyników.
- **for-each:** Podobnie jak `map`, ale służy do wykonywania funkcji na każdym elemencie bez zwracania wyniku.
- **do:** Ogólna konstrukcja pętli obsługująca szeroki zakres procesów iteracyjnych.
- **rekursja:** Potężna technika, w której funkcje wywołują same siebie, rozwiązując problemy krok po kroku.

### Jak działa iteracja

Iteracja zazwyczaj obejmuje:
1. **Określenie powtórzenia:** Wskazanie akcji do powtórzenia oraz danych lub zakresu do przetworzenia.
2. **Wykonywanie sekwencyjne:** Powtarzanie akcji dla każdego elementu, kroku lub warunku aż do zakończenia.
3. **Zwrócenie wyniku (opcjonalnie):** W zależności od konstrukcji iteracja może zwracać wynik lub modyfikować stan.

Te konstrukcje umożliwiają pisanie elastycznych, wydajnych i eleganckich skryptów, które z łatwością radzą sobie ze złożonymi zadaniami.
