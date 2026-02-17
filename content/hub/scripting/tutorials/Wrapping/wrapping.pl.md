---
title: "Zawijanie"
type: docs
weight: 4
---
Polecenia schematu działają na niskim poziomie, co oznacza, że ​​nawet proste zadania mogą wymagać wielu kroków. Jednak ta szczegółowość zapewnia elastyczność, możemy łączyć polecenia w małe funkcje wielokrotnego użytku, które robią dokładnie to, czego potrzebujemy. Opakowanie nie jest koncepcją czarno-białą; może obejmować zarówno proste aliasy dla często używanych poleceń, jak i bardziej złożone funkcje zarządzające całymi przepływami pracy. Czasami opakowanie jest po prostu wygodną funkcją poprawiającą czytelność, podczas gdy w innych przypadkach przekształca się w w pełni funkcjonalne narzędzie, które zawiera wiele operacji.

### Po co zawijać funkcje?

Funkcje owijania mają kilka kluczowych zalet:

- **Ułatwia powtarzalne zadania** – Zamiast powtarzać polecenia niskiego poziomu, zawiń je w funkcję pomocniczą i użyj jej ponownie.
- **Poprawia czytelność** – Nadanie naszym opakowanym funkcjom jasnych, opisowych nazw sprawia, że ​​nasz kod jest łatwiejszy do zrozumienia na pierwszy rzut oka.
- ** Hermetyzuje złożoność** – Zamiast zajmować się długimi, tajemniczymi listami poleceń, głęboko zagnieżdżonymi pętlami lub złożonymi instrukcjami komunikatów, możemy podzielić je na mniejsze, dobrze ustrukturyzowane funkcje pomocnicze.
- **Większa łatwość konserwacji** – Jeśli podstawowa funkcjonalność polecenia ulegnie zmianie, wystarczy tylko raz zaktualizować opakowaną funkcję, izolując nasze wtyczki od szczegółów tych zmian.
- **Zachęca do ponownego wykorzystania kodu** – Każdy pomocnik staje się częścią Twojej biblioteki, dzięki czemu przyszłe skrypty można szybciej pisać i debugować.

W miarę rozwoju wtyczek opakowania pomagają zachować czytelność podstawowej logiki i izolować powtarzające się szczegóły.

Kolejną zaletą funkcji zawijających jest zintegrowanie ich z podświetlaniem składni, takim jak Visual Studio Code. Poprawia to czytelność i nawigację, dzięki czemu skrypty są wyraźniejsze. We wtyczce korzystającej z funkcji niestandardowych każda funkcja podświetlona na zielono potwierdza, że ​​poprawnie odwołuje się do niej z naszej biblioteki.

Jeśli utrzymujesz własną bibliotekę pomocniczą, rozważ dodanie nazw funkcji projektu do podświetlania składni edytora. Przyspiesza nawigację i refaktoryzację.

Przykłady:

### Losowe nasiona

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Chociaż moglibyśmy użyć ***msrg-rand*** bezpośrednio w naszym kodzie, zawinięcie go w funkcję o nazwie ***random-seed*** poprawia czytelność. Nadając funkcji jasną i opisową nazwę, łatwiej jest zrozumieć jej cel na pierwszy rzut oka.

Dodatkowo zdefiniowanie ***random-seed*** jako samodzielnej funkcji pozwala nam używać jej w dowolnym miejscu w naszych wtyczkach, jednocześnie centralizując implementację w jednym miejscu. Jeśli kiedykolwiek będziemy musieli zmienić sposób generowania materiału siewnego, wystarczy, że zaktualizujemy tę funkcję, pozostawiając resztę kodu nietkniętą.

Na przykład, jeśli zamiast tego zdecydujemy się przejść na ***random***:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

Nazwa funkcji pozostaje taka sama, co zapewnia dalsze działanie naszych skryptów bez modyfikacji. Dzięki takiemu podejściu nasz kod jest elastyczny, łatwy w utrzymaniu i łatwy do odczytania.

### Eksportowanie JPEG

Funkcja eksportu JPEG w Scheme ma wiele parametrów, oferujących precyzyjną kontrolę nad sposobem zapisywania obrazów. Jednak w większości przypadków interesuje nas tylko kilka kluczowych ustawień, takich jak nazwa pliku i jakość. Aby uprościć proces, możemy zawinąć funkcję.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
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

W tej funkcji opakowania większość opcji eksportu jest zakodowana na stałe, ujawniając tylko parametry, które prawdopodobnie będziemy dostosowywać: nazwę pliku i jakość. Takie podejście poprawia czytelność i ułatwia zapisywanie obrazów.Dodatkowo, jeśli w przyszłości zmieni się eksporter Lumi, będziemy musieli zaktualizować tylko tę jedną funkcję, zamiast modyfikować każdy skrypt eksportujący plik JPEG.

### Korzystanie z opakowania

Aby wyeksportować plik JPEG do naszych wtyczek, po prostu dołączamy bibliotekę i wywołujemy naszą niestandardową funkcję:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Dzięki temu nasz kod jest czysty, czytelny i elastyczny, a jednocześnie pozwala nam efektywnie eksportować pliki JPEG przy minimalnym wysiłku.

### Wymiana samochodu

Funkcja ***samochód*** może być tajemnicza i podatna na błędy skryptów. Łatwo jest omyłkowo zastosować ***samochód*** do wektora lub elementu spoza listy, co może prowadzić do nieoczekiwanego zachowania. Aby uczynić nasz kod bardziej solidnym i czytelnym, możemy opakować tę funkcjonalność w bezpieczniejszą funkcję.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Ta funkcja bezpiecznie pobiera pierwszy element listy lub wektora, zapewniając jednocześnie przydatne ostrzeżenia w przypadku napotkania nieprawidłowych lub pustych danych wejściowych. Używając ***pierwszy element*** zamiast ***samochód***, zmniejszamy ryzyko przypadkowych błędów i poprawiamy przejrzystość naszych skryptów.

#### Dlaczego warto używać tego opakowania?

- **Zapobiega awariom skryptu** – Pozwala uniknąć błędów spowodowanych zastosowaniem ***samochodu*** do obiektów niebędących listami.
- **Obsługuje zarówno listy, jak i wektory** – Rozszerza użyteczność poza zwykłe listy.
- **Zawiera znaczące ostrzeżenia** – Pomaga w usuwaniu nieoczekiwanych problemów z danymi wejściowymi.
- **Poprawia czytelność** – Nazwa funkcji jasno oddaje jej przeznaczenie.

Hermetyzując tę ​​logikę w pierwszym elemencie, sprawiamy, że nasze wtyczki są solidniejsze i łatwiejsze w utrzymaniu. Oczywiście sprowadza się to do osobistych preferencji, możesz czuć się całkowicie komfortowo korzystając bezpośrednio z funkcji car, caar, cadr i podobnych funkcji Scheme.

### Zawijanie opakowanej funkcji

Zawijanie funkcji, która jest już opakowana, może jeszcze bardziej poprawić czytelność i łatwość konserwacji. Na przykład, pracując z parami współrzędnych, takimi jak ***współrzędne pikseli (lista 100 200)***, możemy użyć:

```scheme
(first-item pixel-coords)
```

aby pobrać współrzędne ***x***. Jednak choć funkcjonalne, nie jest to zbyt wyraziste. Zamiast tego możemy zawinąć ***pierwszy element*** w bardziej odpowiednią definicję, aby uczynić nasze intencje jaśniejszymi.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Dlaczego warto używać tego podejścia?

- **Zwiększa przejrzystość kodu** – Zamiast używać ogólnych funkcji dostępu do list, jawnie definiujemy funkcje, które opisują ich przeznaczenie.
- **Poprawia łatwość konserwacji** – Jeśli nasza reprezentacja współrzędnych ulegnie zmianie (np. użycie wektorów zamiast list), wystarczy zaktualizować te małe funkcje.
- **Zachęca do spójności** – Użycie ***x-coord*** i ***y-coord*** sprawia, że ​​skrypt jest łatwiejszy do odczytania i zrozumienia na pierwszy rzut oka.

Teraz zamiast pisać w ogólnym schemacie:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

W _naszym_ schemacie możemy napisać:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Zawijając funkcje niskiego poziomu w znaczące nazwy, tworzymy bardziej intuicyjny sposób pracy z danymi, redukując zamieszanie i potencjalne błędy.

### Wniosek

Zawijanie funkcji to skuteczny sposób na uproszczenie tworzenia schematów, dzięki czemu skrypty są bardziej czytelne, łatwiejsze w utrzymaniu i niezawodne. Hermetyzując złożoność i eksponując tylko niezbędne szczegóły, tworzymy bardziej uporządkowane podejście do pisania wtyczek.

Najważniejsze wnioski z tego podejścia:- **Ułatwia powtarzalne zadania** – Zamiast ręcznie powtarzać polecenia niskiego poziomu, tworzymy funkcje wielokrotnego użytku.
- **Poprawia czytelność kodu** – Dobrze nazwane opakowania ułatwiają zrozumienie skryptów.
- ** Hermetyzuje złożoność** – Szczegóły niskiego poziomu są obsługiwane wewnątrz opakowania, dzięki czemu główny skrypt jest czysty.
- **Większa łatwość konserwacji** – Jeśli zmieni się podstawowa funkcjonalność, wystarczy zaktualizować opakowanie, a nie każdy skrypt, który się na nim opiera.
- **Zachęca do ponownego użycia i zapewnia spójność** – Nasza osobista biblioteka funkcji rozrasta się z biegiem czasu, dzięki czemu programowanie staje się szybsze i wydajniejsze.

Konsekwentnie stosując zawijanie funkcji, możemy zmienić sposób pisania wtyczek Scheme, tworząc bardziej modułowe i wyraziste środowisko skryptowe. Mając na uwadze te zasady, możemy w dalszym ciągu udoskonalać nasze podejście, opracowując bardziej wydajną i dostosowaną do potrzeb wersję Schematu, która spełnia nasze specyficzne potrzeby.

Kolejne kroki: zidentyfikuj powtarzające się bloki w swoich skryptach i wyodrębnij małych pomocników o wyraźnych nazwach.