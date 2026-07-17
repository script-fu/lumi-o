---
title: "Zawijanie"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7b176d9b546b2566812e825fb2e10da5dd4e86f0e79be2c362a4775546110ac6
url: "hub/scripting/tutorials/Wrapping/wrapping"
---
Polecenia Scheme działają na niskim poziomie, co oznacza, że nawet proste zadania mogą wymagać wielu kroków. Ta szczegółowość daje jednak elastyczność: możemy łączyć polecenia w małe, wielokrotnie używane funkcje, które robią dokładnie to, czego potrzebujemy. Opakowanie nie jest koncepcją czarno-białą; może obejmować zarówno proste aliasy dla często używanych poleceń, jak i bardziej złożone funkcje zarządzające całymi przepływami pracy. Czasami opakowanie to po prostu wygodna funkcja poprawiająca czytelność, a czasem przekształca się w pełnoprawne narzędzie obejmujące wiele operacji.

### Po co zawijać funkcje?

Funkcje owijania mają kilka kluczowych zalet:

- **Ułatwia powtarzalne zadania** – Zamiast powtarzać polecenia niskiego poziomu, zawiń je w funkcję pomocniczą i użyj jej ponownie.
- **Poprawia czytelność** – Nadanie naszym opakowanym funkcjom jasnych, opisowych nazw sprawia, że nasz kod jest łatwiejszy do zrozumienia na pierwszy rzut oka.
- **Hermetyzuje złożoność** – Zamiast zajmować się długimi, tajemniczymi listami poleceń, głęboko zagnieżdżonymi pętlami lub złożonymi instrukcjami komunikatów, możemy podzielić je na mniejsze, dobrze zorganizowane funkcje pomocnicze.
- **Większa łatwość konserwacji** – Jeśli podstawowa funkcjonalność polecenia ulegnie zmianie, wystarczy tylko raz zaktualizować opakowaną funkcję, izolując nasze wtyczki od szczegółów tych zmian.
- **Zachęca do ponownego wykorzystania kodu** – Każdy pomocnik staje się częścią Twojej biblioteki, dzięki czemu przyszłe skrypty można szybciej pisać i debugować.

W miarę rozwoju wtyczek opakowania pomagają zachować czytelność podstawowej logiki i izolować powtarzające się szczegóły.

Kolejną zaletą funkcji zawijających jest zintegrowanie ich z podświetlaniem składni, takim jak Visual Studio Code. Poprawia to czytelność i nawigację, dzięki czemu skrypty są wyraźniejsze. We wtyczce korzystającej z funkcji niestandardowych każda funkcja podświetlona na zielono potwierdza, że poprawnie odwołuje się do niej z naszej biblioteki.

Jeśli utrzymujesz własną bibliotekę pomocniczą, rozważ dodanie nazw funkcji projektu do podświetlania składni edytora. Przyspiesza nawigację i refaktoryzację.

Przykłady:

### Losowy seed

```scheme
;; Cel: Zwraca losową liczbę całkowitą do inicjalizacji filtra
(define (random-seed)
  (msrg-rand))
```

Chociaż moglibyśmy użyć ***msrg-rand*** bezpośrednio w naszym kodzie, zawinięcie go w funkcję o nazwie ***random-seed*** poprawia czytelność. Nadając funkcji jasną i opisową nazwę, łatwiej jest zrozumieć jej cel na pierwszy rzut oka.

Dodatkowo zdefiniowanie ***random-seed*** jako samodzielnej funkcji pozwala nam używać jej w dowolnym miejscu w naszych wtyczkach, jednocześnie centralizując implementację w jednym miejscu. Jeśli kiedykolwiek będziemy musieli zmienić sposób generowania seeda, wystarczy zaktualizować tę funkcję, pozostawiając resztę kodu nietkniętą.

Na przykład, jeśli zamiast tego zdecydujemy się przejść na ***random***:

```scheme
;; Cel: Zwraca losową liczbę całkowitą do inicjalizacji filtra
(define (random-seed)
  (random 1000))
```

Nazwa funkcji pozostaje taka sama, co zapewnia dalsze działanie naszych skryptów bez modyfikacji. Dzięki takiemu podejściu nasz kod jest elastyczny, łatwy w utrzymaniu i łatwy do odczytania.

### Eksportowanie JPEG

Funkcja eksportu JPEG w Scheme ma wiele parametrów, oferujących precyzyjną kontrolę nad sposobem zapisywania obrazów. Jednak w większości przypadków interesuje nas tylko kilka kluczowych ustawień, takich jak nazwa pliku i jakość. Aby uprościć proces, możemy zawinąć funkcję.

```scheme
;; Cel: Zapisuje obraz jako JPEG o określonej jakości
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Unikaj jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

W tej funkcji opakowania większość opcji eksportu jest zakodowana na stałe, ujawniając tylko parametry, które prawdopodobnie będziemy dostosowywać: nazwę pliku i jakość. Takie podejście poprawia czytelność i ułatwia zapisywanie obrazów.

Dodatkowo, jeśli w przyszłości zmieni się eksporter Lumi, wystarczy zaktualizować tylko tę jedną funkcję, zamiast modyfikować każdy skrypt eksportujący plik JPEG.

### Korzystanie z opakowania

Aby wyeksportować plik JPEG do naszych wtyczek, po prostu dołączamy bibliotekę i wywołujemy naszą niestandardową funkcję:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Dzięki temu nasz kod jest czysty, czytelny i elastyczny, a jednocześnie pozwala nam efektywnie eksportować pliki JPEG przy minimalnym wysiłku.

### Zastąpienie `car`

Funkcja ***car*** może być nieintuicyjna i podatna na błędy w skryptach. Łatwo omyłkowo zastosować ***car*** do wektora lub elementu spoza listy, co może prowadzić do nieoczekiwanego zachowania. Aby uczynić nasz kod bardziej solidnym i czytelnym, możemy opakować tę funkcjonalność w bezpieczniejszą funkcję.

```scheme
;; Cel: Zwraca pierwszy element listy lub wektora.
;;          Ostrzega, jeśli dane wejściowe są nieprawidłowe lub puste.
(define (first-item collection)
  (cond
    ;; Obsługuje niepuste listy
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Obsługuje niepuste wektory
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Nieprawidłowe lub puste dane wejściowe
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Ta funkcja bezpiecznie pobiera pierwszy element listy lub wektora, zapewniając jednocześnie przydatne ostrzeżenia w przypadku napotkania nieprawidłowych lub pustych danych wejściowych. Używając ***first-item*** zamiast ***car***, zmniejszamy ryzyko przypadkowych błędów i poprawiamy przejrzystość naszych skryptów.

#### Dlaczego warto używać tego opakowania?

- **Zapobiega awariom skryptu** – Pozwala uniknąć błędów spowodowanych zastosowaniem ***car*** do obiektów niebędących listami.
- **Obsługuje zarówno listy, jak i wektory** – Rozszerza użyteczność poza zwykłe listy.
- **Zawiera znaczące ostrzeżenia** – Pomaga w usuwaniu nieoczekiwanych problemów z danymi wejściowymi.
- **Poprawia czytelność** – Nazwa funkcji jasno oddaje jej przeznaczenie.

Hermetyzując tę logikę w `first-item`, sprawiamy, że nasze wtyczki są solidniejsze i łatwiejsze w utrzymaniu. Oczywiście to kwestia osobistych preferencji — możesz bez problemu używać bezpośrednio `car`, `caar`, `cadr` i podobnych funkcji Scheme.

### Zawijanie opakowanej funkcji

Zawijanie funkcji, która jest już opakowana, może jeszcze bardziej poprawić czytelność i łatwość konserwacji. Na przykład, pracując z parami współrzędnych, takimi jak ***pixel-coords (list 100 200)***, możemy użyć:

```scheme
(first-item pixel-coords)
```

aby pobrać współrzędną ***x***. Choć to działa, nie jest zbyt wyraziste. Zamiast tego możemy zawinąć ***first-item*** w bardziej odpowiednią definicję, aby uczynić nasze intencje jaśniejszymi.

```scheme
;; Cel: Zwraca współrzędną x, dla czytelności
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Cel: Zwraca współrzędną y, dla czytelności
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Dlaczego warto używać tego podejścia?

- **Zwiększa przejrzystość kodu** – Zamiast używać ogólnych funkcji dostępu do list, jawnie definiujemy funkcje, które opisują ich przeznaczenie.
- **Poprawia łatwość konserwacji** – Jeśli nasza reprezentacja współrzędnych ulegnie zmianie (np. użycie wektorów zamiast list), wystarczy zaktualizować te małe funkcje.
- **Zachęca do spójności** – Użycie ***x-coord*** i ***y-coord*** sprawia, że skrypt jest łatwiejszy do odczytania i zrozumienia na pierwszy rzut oka.

Teraz zamiast pisać w ogólnym Scheme:

```scheme
(car pixel-coords) ;; Pobiera współrzędną x
(cadr pixel-coords) ;; Pobiera współrzędną y
```

W _naszym_ Scheme możemy napisać:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Zawijając funkcje niskiego poziomu w znaczące nazwy, tworzymy bardziej intuicyjny sposób pracy z danymi, redukując zamieszanie i potencjalne błędy.

### Dostarczane opakowania: narzędzie Stdlib

Lumi dostarcza zestaw gotowych opakowań ładowanych automatycznie przy uruchomieniu, dzięki czemu są one dostępne w dowolnej wtyczce lub w Scheme Console bez żadnego wywołania `(load ...)`. Biblioteki te (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` i `paths.scm`) działają dokładnie na tej samej zasadzie co powyższe przykłady: nadają zrozumiałym nazwom operacje niskiego poziomu, ukrywają powtarzalny kod i dają jedno miejsce aktualizacji, gdy zmieni się polecenie bazowe. Na przykład `images.scm` udostępnia `image-get-open-list` jako czytelne opakowanie wokół surowego wywołania PDB, a `files.scm` — pomocników do budowy ścieżek, które w innym przypadku wymagałyby powtarzanych łańcuchów `string-append`.

Możesz przeglądać każdą wyeksportowaną nazwę, czytać jej dokumentację i zobaczyć, z której biblioteki pochodzi, w **[Przeglądarce narzędzi]({{< ref "/hub/scripting/reference/utility-browser" >}})** (Pomoc → Programowanie → Przeglądarka narzędzi). Jest to praktyczna demonstracja owijania na dużą skalę i przydatne źródło wzorców do zapożyczenia podczas tworzenia własnej biblioteki pomocniczej.

### Wniosek

Zawijanie funkcji to skuteczny sposób na uproszczenie tworzenia w Scheme, dzięki czemu skrypty są bardziej czytelne, łatwiejsze w utrzymaniu i niezawodne. Hermetyzując złożoność i eksponując tylko niezbędne szczegóły, tworzymy bardziej uporządkowane podejście do pisania wtyczek.

Najważniejsze wnioski z tego podejścia:

- **Ułatwia powtarzalne zadania** – Zamiast ręcznie powtarzać polecenia niskiego poziomu, tworzymy funkcje wielokrotnego użytku.
- **Poprawia czytelność kodu** – Dobrze nazwane opakowania ułatwiają zrozumienie skryptów.
- **Hermetyzuje złożoność** – Szczegóły niskiego poziomu są obsługiwane wewnątrz opakowania, dzięki czemu główny skrypt jest czysty.
- **Większa łatwość konserwacji** – Jeśli zmieni się podstawowa funkcjonalność, wystarczy zaktualizować opakowanie, a nie każdy skrypt, który się na nim opiera.
- **Zachęca do ponownego użycia i zapewnia spójność** – Nasza osobista biblioteka funkcji rozrasta się z biegiem czasu, dzięki czemu programowanie staje się szybsze i wydajniejsze.

Konsekwentnie stosując zawijanie funkcji, możemy zmienić sposób pisania wtyczek Scheme, tworząc bardziej modułowe i wyraziste środowisko skryptowe. Mając na uwadze te zasady, możemy w dalszym ciągu udoskonalać nasze podejście, opracowując bardziej wydajną i dostosowaną do potrzeb wersję Scheme, która spełnia nasze specyficzne potrzeby.

Kolejne kroki: zidentyfikuj powtarzające się bloki w swoich skryptach i wyodrębnij małych pomocników o wyraźnych nazwach.