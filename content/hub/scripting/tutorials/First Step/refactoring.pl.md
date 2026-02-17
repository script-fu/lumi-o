---
title: "Refaktoryzacja"
type: docs
weight: 2
---
Kiedy już funkcja zadziała, możemy cofnąć się o krok i zastanowić, jak najlepiej ustrukturyzować nasz kod. Celem jest uczynienie naszej wtyczki tak przejrzystą, zrozumiałą i łatwą w utrzymaniu, jak to tylko możliwe. Ten proces ulepszania i udoskonalania struktury istniejącego kodu bez zmiany jego zachowania nazywany jest refaktoryzacją.

Oto ponownie funkcja początkowa:

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
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

W schemacie zmienna ma „zakres”, o którym jest znana, a zakres ten jest ustawiany za pomocą instrukcji `let`. Zmienna jest powiązana z wartością w części wiążącej, a zmienna ma zakres w treści let. Zmienna jest znana tylko wewnątrz bloku let i nie można uzyskać do niej dostępu poza nim.

```scheme
(let ((variable value))
  body)
```

Wprowadzenie zmiennej o nazwie „wiadomość”:

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

W naszym przykładzie użyliśmy zmiennej o nazwie „wiadomość” powiązanej z ciągiem znaków „Hello world!\n”. Dzięki temu możemy zmienić treść wiadomości raz, a nie trzy razy, co zmniejsza ryzyko błędów i uelastycznia kod.

### Wyodrębnianie funkcji

W programowaniu funkcjonalnym powszechną praktyką jest refaktoryzacja kodu w celu wyodrębnienia logiki wielokrotnego użytku do oddzielnych funkcji. W ten sposób **funkcja główna** staje się znacznie prostsza i bardziej skupiona na celu wysokiego poziomu, podczas gdy **funkcja wyodrębniona** wydaje się bardziej złożona, ponieważ obsługuje szczegółową logikę. Jest to zamierzone i jest zgodne z podstawowymi zasadami programowania funkcjonalnego, takimi jak modułowość, separacja problemów i czytelność. Oto refaktoryzacja
Witaj świecie! po ekstrakcji.

Wyodrębnianie logiki:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
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
W powyższym przykładzie używany jest typ danych zwany symbolem, taki jak „gui. Symbole są przekazywane jako parametry do funkcji wysyłania wiadomości i można ich używać do podejmowania prostych decyzji warunkowych. Podobnie jak klucze symboliczne, są one unikalnymi identyfikatorami. Więcej informacji na temat symboli można znaleźć na stronie [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Uproszczenie funkcji głównej

W oryginalnej funkcji (scheme-hello-world) cała logika wysyłania komunikatów do różnych wyjść (GUI, konsola błędów, terminal) została wmieszana w funkcję główną. Po refaktoryzacji główna funkcja po prostu koncentruje się na **co należy zrobić**, wysyłając wiadomość do różnych miejsc docelowych.

Zrefaktoryzowana funkcja główna jest prostsza:

- Jasno określa swój cel: wysłać tę samą wiadomość do wielu wyjść.
- Pozwala uniknąć zaśmiecania głównej logiki powtarzalnym kodem, na przykład ustawiania procedur obsługi komunikatów dla różnych wyników.
- Łatwiej jest przeczytać i zrozumieć na pierwszy rzut oka.

### Złożoność wyodrębnionej funkcji

Natomiast funkcja **(wyślij wiadomość)** zawiera szczegółową logikę. Obsługuje teraz zmiany w zachowaniu każdego wyjścia (GUI, konsola błędów, terminal). Funkcja jest nieco bardziej złożona niż wcześniej, ale teraz jest **scentralizowana** i **izolowana**.

## Powiązanie tego z programowaniem funkcjonalnym

W programowaniu funkcjonalnym funkcje są postrzegane jako **obywatele pierwszej klasy**, co oznacza, że można je ponownie wykorzystać, przekazywać innym i łączyć w bardziej złożone zachowanie. Celem jest:- **Rozłóż problemy** na mniejsze, niezależne części.
- **Izoluj złożoność** na mniejsze funkcje, które obsługują określone zadania, takie jak `send-message`.
- **Utrzymuj proste funkcje wyższego poziomu**, aby mogły skupić się na organizowaniu przepływu danych i działań, bez konieczności znajomości szczegółów realizacji każdego zadania.
- **Separacja obaw**: Funkcja dba o sposób wysłania wiadomości w oparciu o typ wyjścia, co izoluje tę logikę od funkcji głównej.
- **Modułowość**: Obsługując całą logikę wysyłania wiadomości w jednym miejscu, możemy łatwo wprowadzać zmiany (takie jak dodanie nowych opcji wyjściowych) bez zmiany głównej funkcji.
- **Ponowne użycie**: Funkcja `send-message` jest wielokrotnego użytku, co oznacza, że ​​jeśli musimy wysłać wiadomość do wielu wyjść w innym miejscu naszego kodu, możemy po prostu wywołać tę funkcję, zamiast przepisywać podobną logikę.

Dzięki refaktoryzacji główna funkcja w tym przykładzie staje się **deklaratywnym** stwierdzeniem tego, co się dzieje („wyślij wiadomość do trzech miejsc”), podczas gdy złożoność sposobu wysyłania tych wiadomości jest abstrakcyjna w funkcji `send-message`.