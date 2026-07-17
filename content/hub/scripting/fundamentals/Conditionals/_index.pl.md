---
title: "Instrukcje warunkowe"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals/_index"
---
Instrukcje warunkowe to podstawa programowania: pozwalają skryptom podejmować decyzje i sterować przebiegiem wykonania według określonych kryteriów. W Scheme instrukcje warunkowe pomagają tworzyć dynamiczne, inteligentne skrypty, które dostosowują się do zmieniających się danych wejściowych, środowiska lub działań użytkownika.

### Rola instrukcji warunkowych w Scheme

Instrukcje warunkowe pełnią w skryptach kilka kluczowych funkcji:
- **Kierowanie logiką:** Uruchamiają różne fragmenty kodu w zależności od tego, czy określone warunki są prawdziwe, czy fałszywe.
- **Większa elastyczność:** Dynamicznie reagując na dane wejściowe lub stan, pomagają obsłużyć wiele scenariuszy.
- **Uproszczenie złożoności:** Dzielą podejmowanie decyzji na przejrzyste struktury, ułatwiając czytanie, debugowanie i utrzymanie kodu.

### Dostępne konstrukcje warunkowe

Scheme oferuje kilka konstrukcji warunkowych, każda dopasowana do innych potrzeb logicznych:
- **`if`:** Do prostych decyzji binarnych — jeden blok, gdy warunek jest prawdziwy, inny, gdy fałszywy.
- **`cond`:** Potężna konstrukcja wielościeżkowa do obsługi wielu warunków w przejrzysty, uporządkowany sposób.
- **`and` / `or`:** Operatory logiczne oceniające kombinacje warunków dla bardziej złożonych decyzji.
- **`else`:** Przypadek domyślny definiujący zachowanie, gdy żaden ze wskazanych warunków nie jest spełniony.

### Jak działają instrukcje warunkowe

Instrukcje warunkowe zazwyczaj obejmują:
1. **Ocena warunku:** Wyrażenie testowe określa, czy warunek jest prawdziwy, czy fałszywy.
2. **Rozgałęziona realizacja:** Na podstawie oceny skrypt wybiera blok kodu do wykonania.
3. **Zwrócenie wartości (opcjonalnie):** W niektórych przypadkach instrukcje warunkowe zwracają też wartość użyteczną w innych częściach skryptu.