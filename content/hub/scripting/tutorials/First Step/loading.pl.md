---
title: "Załadunek"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
url: "hub/scripting/tutorials/First Step/loading"
---
Gdy tylko funkcja pomocnicza powiększy się, przenieś ją do małego pliku biblioteki. Dzięki temu wtyczka jest skoncentrowana i sprawia, że pomocnika można używać wielokrotnie w wielu wtyczkach.

### Utwórz funkcję biblioteki

Możemy skorzystać z funkcji send-message i utworzyć nowy plik z tą zawartością. Zapisz plik w folderze repo, a nie w części z wtyczkami, być może w pobliżu najwyższego poziomu;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: To jest główny katalog do przechowywania kodu Scheme.
  - **biblioteka/**: Tutaj działają wspólne funkcje, takie jak `send-message.scm`.
  - **wtyczki/**: Tutaj przechowywane są Twoje indywidualne wtyczki.
    - **hello-world/**: Folder dla konkretnej wtyczki „Hello World!”.
      - **hello-world.scm**: Plik skryptu wtyczki.

Przykład funkcji bibliotecznej send-message.scm

```scheme
;; Funkcja obsługująca wysyłanie wiadomości do różnych miejsc docelowych
(define (send-message message output)
  (cond
    ;; Wyślij do konsoli komunikatów
    ((eq? output 'error-console)
       ;; Ustaw obsługę na konsoli komunikatów
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

  ;; Przywróć domyślną obsługę wiadomości do konsoli komunikatów
  (lumi-message-set-handler 2))
```

### Załaduj funkcję biblioteki

Funkcję biblioteczną możemy załadować poleceniem Scheme `load`:

Ładowanie pliku biblioteki:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Hej! Mamy teraz coś prostszego i krótszego do czytania, co opisuje się samo bez komentarzy. To satysfakcjonujący wniosek refaktoryzacji.