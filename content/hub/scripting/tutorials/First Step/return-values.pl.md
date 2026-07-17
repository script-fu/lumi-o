---
title: "Zwracane wartości"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 80a8f61c6fc7f6b86167f7489f61558b49f3d1d2b7e1e5236406cbca31ff611e
---
Zwracane wartości mają znaczenie, ponieważ pozwalają kontrolować przepływ bez dodatkowego stanu. W schemacie ostatnio ocenione wyrażenie staje się wartością zwracaną.

Na tej stronie zastosowano pomocniki sprawdzania poprawności z przykładu przesyłania komunikatów, aby pokazać, jak jawne zwracane wartości ułatwiają tworzenie kodu.

### Co to jest wartość zwracana?

W schemacie wartość zwracana przez funkcję jest określana na podstawie ostatniego wyrażenia ocenianego przez funkcję. Oznacza to, że ostatnia linia kodu funkcji zostanie zwrócona jako wynik funkcji. Jeśli żadna wartość nie zostanie jawnie zwrócona, funkcja zwróci `#f` (false) lub `undefined`.

Powróćmy do funkcji sprawdzania poprawności (is-valid-string?)

```scheme
;; Cel: Sprawdza, czy wiadomość jest niepustym ciągiem znaków
(define (is-valid-string? message)
  ;; Sprawdź, czy wiadomość jest niepustym ciągiem znaków
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

W tej funkcji, jeśli wiadomość jest nieprawidłowa, zgłaszany jest błąd. Jeśli jednak wiadomość jest poprawna, nie jest podana jawna wartość zwracana i funkcja domyślnie zwraca `#f`.

### Jawne określanie zwracanych wartości

Możemy to poprawić, czyniąc zwracaną wartość bardziej wyraźną. Na przykład możemy zwrócić `#t` (true), jeśli wiadomość jest prawidłowa:

```scheme
;; Cel: Sprawdza, czy wiadomość jest wysyłana do prawidłowego wyjścia
(define (is-valid-output-display? output)
  ;; Sprawdź, czy wyjście jest jednym z oczekiwanych miejsc docelowych wyświetlania
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

W tej wersji funkcja zwróci `#t` gdy wiadomość będzie ważna, dając jednoznaczny wynik. Dzięki temu funkcja może być używana bardziej elastycznie w innych kontekstach, w których potrzebny jest wynik logiczny.

### Efektywne używanie zwracanych wartości

Decydując, co zwracają nasze funkcje, możemy uczynić je bardziej przewidywalnymi i użytecznymi. Zwracanie wartości takich jak `#t`, `#f` lub konkretnego wyniku daje nam większą kontrolę nad interakcją funkcji z resztą kodu. Na przykład możesz użyć wartości zwracanej do podjęcia dalszych decyzji w funkcji wywołującej lub przekazać ją jako argument do innej funkcji.

Oto prosty przykład użycia wartości zwracanej do kontrolowania przepływu logiki:

```scheme
;; Cel: Wysyła wiadomość do właściwego miejsca docelowego wyjścia
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

W tym przypadku (wyślij wiadomość) opiera się na wartości zwracanej przez (is-valid-output-display?), aby zdecydować, czy kontynuować.
Instrukcja warunkowa `cond` zostanie pominięta, jeśli pierwszy test zakończy się niepowodzeniem. Zwróć także uwagę, jak odczytuje się to w dość naturalny sposób, czy wyświetlanie danych wyjściowych jest prawidłowe?

## Logika instrukcji if w schemacie

Zanim przedstawimy przykład refaktoryzacji biblioteki, oto krótki przegląd logiki warunkowej. Schemat wykorzystuje `if` do wyboru pomiędzy dwiema ścieżkami.

Oto prosta forma instrukcji `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Struktura ta sprawdza warunek i jeśli warunek jest prawdziwy, wykonuje pierwszą akcję. Jeśli warunek jest fałszywy, wykonuje drugą akcję.

W przypadkach, gdy musisz wykonać wiele akcji, gdy warunek jest prawdziwy lub fałszywy, możesz użyć `begin`, aby zgrupować je razem:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Umożliwia to obsługę bardziej złożonych sytuacji, w których należy wykonać wiele wyrażeń lub instrukcji w zależności od wyniku testu warunkowego.

OK, oto kod biblioteki z osadzonymi wartościami zwracanymi i używanymi do kontrolowania procesu wykonawczego.

### Refaktoryzacja z wartościami zwracanymi

```scheme
;; Cel: Wysyła wiadomość do paska stanu, zwraca #t w przypadku powodzenia
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Cel: Wysyła wiadomość do okna dialogowego, zwraca #t w przypadku powodzenia
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Cel: Wysyła wiadomość do Error Console, zwraca #t w przypadku powodzenia
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Cel: Wysyła wiadomość do terminal, zwraca #t w przypadku powodzenia
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Cel: Wysyła wiadomość do właściwego wyjścia, zwraca #t w przypadku powodzenia
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Cel: Sprawdza, czy wiadomość jest niepustym ciągiem znaków, zwraca #t jeśli poprawna
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Cel: Sprawdza, czy wyjście jest prawidłowym celem, zwraca #t jeśli poprawne
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Wniosek

Wartości zwracane są podstawową częścią zapewniania elastyczności funkcji i możliwości ich ponownego użycia. Uważnie decydując, co powinna zwracać każda funkcja, możemy zapewnić, że nasze funkcje dobrze ze sobą współdziałają i dostarczają przydatnych informacji do reszty kodu. Niezależnie od tego, czy jest to zwrot `#t`, `#f`, czy coś bardziej szczegółowego, zwracane wartości pozwalają nam kontrolować przepływ naszych programów i obsługiwać różne wyniki.