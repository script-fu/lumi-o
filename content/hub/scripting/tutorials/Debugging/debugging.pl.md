---
title: "Debugowanie"
type: docs
weight: 5
---
W skryptach żadna funkcja nie jest nieomylna. Nawet najbardziej niezawodne polecenia mogą zawieść w obliczu nieoczekiwanych danych wejściowych lub warunków. Aby się przed tym zabezpieczyć, możemy wdrożyć niestandardowy system debugowania i zastosować techniki programowania defensywnego. Opakowując standardowe funkcje w mechanizmy obsługi błędów i dostarczając informacji zwrotnych, możemy sprawić, że nasze skrypty będą solidniejsze i łatwiejsze do rozwiązywania problemów.

Kluczową częścią tej strategii jest użycie globalnej flagi debugowania do kontrolowania pełnych wyników, co pozwala nam na włączenie szczegółowych informacji debugowania, gdy są potrzebne, przy jednoczesnym zachowaniu czystości wyników podczas normalnego wykonywania.

## Flaga globalnego debugowania

Globalna flaga debugowania to prosty, ale skuteczny sposób kontrolowania poziomu informacji wyjściowych podczas wykonywania skryptu. Po włączeniu wyświetla szczegółowe komunikaty debugowania, które mogą być nieocenione przy śledzeniu problemów. Gdy ta opcja jest wyłączona, dane wyjściowe są zwięzłe do użytku produkcyjnego.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

Domyślnie debugowanie jest wyłączone. Aby włączyć szczegółowe wyniki podczas programowania, po prostu ustaw flagę na `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

Możemy także tymczasowo włączyć lub wyłączyć debugowanie dla określonych sekcji kodu, korzystając z funkcji pomocniczych.

### Lokalna kontrola debugowania

Aby uzyskać lepszą kontrolę, możemy włączyć lub wyłączyć debugowanie w określonych częściach skryptu za pomocą funkcji pomocniczych.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Dzięki temu możemy dynamicznie kontrolować debugowanie:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Debuguj system przesyłania wiadomości

Aby efektywnie obsługiwać wyniki debugowania w schemacie, stosujemy podejście strukturalne obejmujące wiele funkcji pomocniczych. Funkcje te zapewniają, że komunikaty debugowania i ostrzeżenia są jasne, czytelne i łatwe w utrzymaniu.

### Omówienie systemu przesyłania komunikatów debugowania

Nasz system przesyłania komunikatów debugowania składa się z następujących elementów:

1. `debug-message` – Wyświetla komunikaty debugowania, gdy debugowanie jest włączone.
2. `serialize-item` – Konwertuje różne typy danych schematu na reprezentację łańcuchową.
3. `concat` – Łączy wiele elementów w jeden ciąg.
4. `list->string` – Formatuje listę do postaci czytelnego ciągu znaków.
5. `message` – Wyświetla dane wyjściowe w konsoli wiadomości Lumi.
6. `warning-message` – Wyświetla komunikaty ostrzegawcze, gdy ostrzeżenia są włączone.

Każda funkcja odgrywa rolę w formatowaniu i wyświetlaniu uporządkowanych komunikatów.

---

### Funkcja komunikatu debugowania

Funkcja `debug-message` jest podstawową metodą wyświetlania wyników debugowania. Zapewnia wyświetlanie komunikatów tylko wtedy, gdy włączone jest debugowanie.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- Warunek `when debug` zapewnia, że komunikaty pojawiają się tylko wtedy, gdy włączone jest debugowanie.
- Wiadomości są poprzedzone `"> "` dla przejrzystości.
- Funkcja wykorzystuje `concat` do formatowania treści wiadomości.
- Na koniec wywołuje `message`, aby wysłać dane wyjściowe do konsoli wiadomości Lumi.

Przykładowe użycie:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Po włączeniu debugowania wynik może być następujący:

```scheme
> item: background-layer has tree position : 3
```

### Serializacja danych dla komunikatów debugowania

Wiadomości mogą zawierać różne typy danych, takie jak listy, wektory i liczby. Aby mieć pewność, że są prawidłowo sformatowane, używamy `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Przykładowe użycie:

```scheme
(serialize-item '(1 2 3))
```

Dane wyjściowe:

```scheme
list:
1
2
3
```

### Łączenie wiadomości

Aby połączyć wiele elementów wiadomości w jeden ciąg, używamy `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Przykładowe użycie:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formatowanie list jako ciągów znaków

Funkcja `list->string` konwertuje listę na sformatowany ciąg znaków.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Komunikaty ostrzegawczeFunkcja `warning-message` działa podobnie do `debug-message`, ale wyświetla ostrzeżenia nawet wtedy, gdy debugowanie jest wyłączone.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Zapewnia wyświetlanie komunikatów tylko wtedy, gdy włączone są ostrzeżenia (flaga `warning` jest ustawiona w `common.scm` jako `#t`).
- Wywołuje `concat` w celu sformatowania treści wiadomości.
- Używa `message` do wysyłania danych wyjściowych do Lumi.

## Udoskonalanie funkcji standardowych

Po wdrożeniu systemu debugowania możemy ulepszyć naszą bibliotekę funkcji, dodając szczegółowe komunikaty. Zapewnia to wgląd w stany elementów, wartości zmiennych i wywołania funkcji.

Typowym przykładem jest `item-is-valid?`, który zawija `lumi-item-id-is-valid` i zwraca `#t` lub `#f`. Jeśli zostanie zwrócony `#f`, możemy wywołać `warning-message` w kodzie wywołującym, jeśli dane wejściowe nie są liczbą, możemy dać ostrzeżenie w funkcji.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Praktyczne zastosowanie

Podczas opracowywania wtyczek Scheme zawijanie funkcji w ten sposób znacznie skraca czas debugowania i zapewnia solidny, łatwy w utrzymaniu kod. Dzięki naszemu systemowi debugowania możemy wygenerować ustrukturyzowany strumień debugowania w konsoli błędów jednym naciśnięciem przełącznika.

W tym strumieniu debugowania wywołania funkcji są oznaczone gwiazdką (*), co ułatwia śledzenie wykonywania skryptów i wykrywanie błędów, szczególnie w przypadku złożonych wtyczek. Ta widoczność pomaga nam zrozumieć przebieg operacji i skutecznie diagnozować nieoczekiwane zachowania.

Opakowanie naszej funkcji wiadomości umożliwiające użycie `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Przykład zastosowania `call` w praktyce:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Przykład strumienia debugowania podczas wykonywania wtyczki:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Ten uporządkowany dziennik zapewnia przejrzysty harmonogram wywołań funkcji i zmian danych, co znacznie ułatwia debugowanie i analizę wydajności.

## Wniosek

Wdrażając ustrukturyzowany system debugowania, tworzymy bezpieczniejsze, łatwiejsze w utrzymaniu skrypty, które oferują wgląd w ich wykonanie w czasie rzeczywistym.

### Kluczowe wnioski

- **Kontrola szczegółowości** – Użyj globalnej flagi debugowania do zarządzania poziomami wyjściowymi.
- **Przekazuj jasne informacje zwrotne** – Dołącz standardowe funkcje informacyjnymi komunikatami debugowania.
- **Większa niezawodność** – Obsługuj nieoczekiwane dane wejściowe z wdziękiem, aby zapobiec błędom.
- **Uprość rozwiązywanie problemów** – Ustrukturyzowane komunikaty debugowania ułatwiają diagnozowanie i naprawianie problemów.

Dzięki takiemu podejściu nasze skrypty skutecznie „wyjaśniają się” podczas przetwarzania danych, redukując frustrację i poprawiając efektywność przepływu pracy. Debugowanie staje się narzędziem proaktywnym, a nie reaktywnym, dzięki czemu nasz proces tworzenia skryptów jest zarówno płynniejszy, jak i bardziej satysfakcjonujący.