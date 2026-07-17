---
title: "Przeglądarka procedur"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f2ea095c0407f9641d28803e937a992e044584f6bcbed960239d0c0df4b430d2
---
**Przeglądarka procedur Lumi** umożliwia przeszukiwanie dostępnych procedur (wbudowanych i dostarczonych w formie wtyczek) oraz sprawdzanie ich parametrów i zwracanych wartości.

### Gdzie znaleźć przeglądarkę procedur Lumi

Dostęp do przeglądarki procedur w Lumi można uzyskać poprzez menu **Pomoc**:

- **Pomoc** -> **Przeglądarka procedur**

### Co robi przeglądarka procedur

Przeglądarka procedur wyświetla wszystkie wewnętrzne procedury Lumi, a także te dodane przez wtyczki, w tym tę, którą właśnie zainstalowałeś. Każdy wpis procedury zawiera przydatne informacje, w tym:

- Nazwa procedury.
- Opis działania.
- Parametry, które akceptuje (wartości wejściowe).
- Wartości zwracane (wyjście).

Jeśli chcesz zweryfikować podpis wywołania lub potwierdzić dokładną nazwę procedury, wyszukuj według słowa kluczowego lub nazwy procedury.

#### (komunikat lumi) w przeglądarce procedur

Wyszukaj `lumi-message`, aby zobaczyć jego parametry i wartości zwracane.

### Znajdowanie wtyczki

Po zainstalowaniu programu „Hello World!” wtyczkę, można ją znaleźć na liście w przeglądarce procedur. Po prostu wyszukaj nazwę funkcji zarejestrowaną w Lumi, w tym przypadku „schemat-hello-world”. Wpis wyświetli parametry i wszelkie zwracane wartości powiązane z wtyczką, wraz z krótkim opisem. Zobaczysz także, gdzie w sekcji **Informacje dodatkowe** wyświetlane są niektóre wiersze tekstu wprowadzone jako parametry wejściowe podczas procesu rejestracji.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Nazwa procedury
  "Hello world!"                                        ;; Nazwa pozycji menu
  "A Scheme procedure plug-in"                       ;; Podpowiedź i opis
  "Your Name"                                           ;; Autor
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; Licencja
  "2024")                                               ;; Data copyright
```

Ułatwia to sprawdzenie, czy wtyczka jest prawidłowo zarejestrowana i pozwala szybko sprawdzić, jak współpracuje ona z innymi procedurami w Lumi. Przeglądarka procedur to potężne narzędzie do debugowania i rozszerzania wtyczek poprzez eksplorację wszystkich dostępnych procedur w Lumi.