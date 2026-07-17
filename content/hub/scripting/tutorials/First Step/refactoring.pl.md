---
title: "Refaktoryzacja"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 730a20920b8e93d463bfb01f5d729e5ea84a548cc4b846e6e888ee751d095cf1
url: "hub/scripting/tutorials/First Step/refactoring"
---
Kiedy już funkcja zadziała, możemy cofnąć się o krok i zastanowić, jak najlepiej ustrukturyzować nasz kod. Celem jest uczynienie naszej wtyczki tak przejrzystą, zrozumiałą i łatwą w utrzymaniu, jak to tylko możliwe. Ten proces ulepszania i udoskonalania struktury istniejącego kodu bez zmiany jego zachowania nazywany jest refaktoryzacją.

Oto ponownie funkcja początkowa:

```scheme
(define (scheme-hello-world)
  ;; Ustaw obsługę wiadomości, aby wysyłała wiadomość do okna dialogowego GUI
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Ustaw obsługę wiadomości, aby wysyłała wiadomość do konsola błędów
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Wyślij wiadomość do terminala, okna systemu operacyjnego, które uruchomiło Lumi
  (display "Hello world!\n"))
```

Nazwa funkcji to nazwa funkcji, a parametr to to, co funkcja przyjmuje jako dane wejściowe. Ciało to blok kodu uruchamiany po wywołaniu funkcji.

Forma abstrakcyjna:

```scheme
(define (function-name parameter)
  body)
```

### Powtórzenie kodu

Usuń powtórzenia wcześniej. `(lumi-message "Hello world!\n")` powtarza się dwukrotnie, a ciąg komunikatu powtarza się trzykrotnie. Zmienna rozwiązuje powtarzający się ciąg.

### Zmienne

W Scheme zmienna ma „zakres”, o którym jest znana, a zakres ten jest ustawiany za pomocą instrukcji `let`. Zmienna jest powiązana z wartością w części wiążącej, a zmienna ma zakres w treści let. Zmienna jest znana tylko wewnątrz bloku let i nie można uzyskać do niej dostępu poza nim.

```scheme
(let ((variable value))
  body)
```

Wprowadzenie zmiennej o nazwie „wiadomość”:

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Ustaw obsługę wiadomości, aby wysyłała wiadomość do okna dialogowego GUI
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Ustaw obsługę wiadomości, aby wysyłała wiadomość do konsola błędów
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Wyślij wiadomość do terminala, okna systemu operacyjnego, które uruchomiło Lumi
    (display message)))
```

W naszym przykładzie użyliśmy zmiennej o nazwie „wiadomość” powiązanej z ciągiem znaków „Hello world!\n”. Dzięki temu możemy zmienić treść wiadomości raz, a nie trzy razy, co zmniejsza ryzyko błędów i uelastycznia kod.

### Wyodrębnianie funkcji

W programowaniu funkcjonalnym powszechną praktyką jest refaktoryzacja kodu w celu wyodrębnienia logiki wielokrotnego użytku do oddzielnych funkcji. W ten sposób **funkcja główna** staje się znacznie prostsza i bardziej skupiona na celu wysokiego poziomu, podczas gdy **funkcja wyodrębniona** wydaje się bardziej złożona, ponieważ obsługuje szczegółową logikę. Jest to zamierzone i jest zgodne z podstawowymi zasadami programowania funkcjonalnego, takimi jak modułowość, separacja problemów i czytelność. Oto refaktoryzacja Hello World! po wyodrębnieniu.

Wyodrębnianie logiki:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

;; Funkcja główna
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Funkcja obsługująca wysyłanie wiadomości do różnych miejsc docelowych
(define (send-message message output)
  (cond
    ;; Wyślij do konsola błędów
    ((eq? output 'error-console)
       ;; Ustaw obsługę na konsola błędów
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Wyślij do okna dialogowego GUI
    ((eq? output 'gui)
       ;; Ustaw obsługę na okno dialogowe GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Wyślij do okna terminala
    ((eq? output 'terminal)
       ;; Wyjście terminala obsługiwane jest przez display
       (display message)))

  ;; Przywróć domyślną obsługę wiadomości do konsola błędów
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Symbole

W powyższym przykładzie używany jest typ danych zwany symbolem, taki jak `'gui`. Symbole są przekazywane jako parametry do funkcji wysyłania wiadomości i można ich używać do podejmowania prostych decyzji warunkowych. Podobnie jak klucze symboliczne, są one unikalnymi identyfikatorami. Więcej informacji na temat symboli można znaleźć na [tej stronie](/hub/scripting/fundamentals/variables-and-scope/symbols/).

### Uproszczenie funkcji głównej

W oryginalnej funkcji (scheme-hello-world) cała logika wysyłania komunikatów do różnych wyjść (GUI, konsola błędów, terminal) została wmieszana w funkcję główną. Po refaktoryzacji główna funkcja po prostu koncentruje się na tym, **co** należy zrobić: wysłać wiadomość do różnych miejsc docelowych.

Zrefaktoryzowana funkcja główna jest prostsza:

- Jasno określa swój cel: wysłać tę samą wiadomość do wielu wyjść.
- Pozwala uniknąć zaśmiecania głównej logiki powtarzalnym kodem, na przykład ustawiania procedur obsługi komunikatów dla różnych wyników.
- Łatwiej jest przeczytać i zrozumieć na pierwszy rzut oka.

### Złożoność wyodrębnionej funkcji

Natomiast funkcja **`send-message`** zawiera szczegółową logikę. Obsługuje teraz zmiany w zachowaniu każdego wyjścia (GUI, konsola błędów, terminal). Funkcja jest nieco bardziej złożona niż wcześniej, ale teraz jest **scentralizowana** i **izolowana**.

## Powiązanie tego z programowaniem funkcjonalnym

W programowaniu funkcjonalnym funkcje są postrzegane jako **obywatele pierwszej klasy**, co oznacza, że można je ponownie wykorzystać, przekazywać innym i łączyć w bardziej złożone zachowanie. Celem jest:

- **Rozłożyć problemy** na mniejsze, niezależne części.
- **Izolować złożoność** na mniejsze funkcje, które obsługują określone zadania, takie jak `send-message`.
- **Utrzymywać proste funkcje wyższego poziomu**, aby mogły skupić się na organizowaniu przepływu danych i działań, bez konieczności znajomości szczegółów realizacji każdego zadania.
- **Separować obawy**: Funkcja dba o sposób wysłania wiadomości w oparciu o typ wyjścia, co izoluje tę logikę od funkcji głównej.
- **Modularyzować kod**: Obsługując całą logikę wysyłania wiadomości w jednym miejscu, możemy łatwo wprowadzać zmiany (takie jak dodanie nowych opcji wyjściowych) bez zmiany głównej funkcji.
- **Zapewniać ponowne użycie**: Funkcja `send-message` jest wielokrotnego użytku, co oznacza, że jeśli musimy wysłać wiadomość do wielu wyjść w innym miejscu naszego kodu, możemy po prostu wywołać tę funkcję, zamiast przepisywać podobną logikę.

Dzięki refaktoryzacji główna funkcja w tym przykładzie staje się **deklaratywnym** stwierdzeniem tego, co się dzieje („wyślij wiadomość do trzech miejsc”), podczas gdy złożoność sposobu wysyłania tych wiadomości jest abstrakcyjna w funkcji `send-message`.