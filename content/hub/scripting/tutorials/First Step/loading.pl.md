---
title: "Załadunek"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
Gdy tylko funkcja pomocnicza powiększy się, przenieś ją do małego pliku biblioteki. Dzięki temu wtyczka jest skoncentrowana i sprawia, że ​​pomocnika można używać wielokrotnie w wielu wtyczkach.

### Make a Library Function

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

- **scheme/**: To jest główny katalog do przechowywania kodu schematu.
  - **biblioteka/**: Tutaj działają wspólne funkcje, takie jak `send-message.scm`.
  - **wtyczki/**: Tutaj przechowywane są Twoje indywidualne wtyczki.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Przykład funkcji bibliotecznej send-message.scm

```scheme
;; Funkcja obsługująca wysyłanie wiadomości do różnych miejsc docelowych
(define (send-message message output)
  (cond
    ;; Wyślij do Message console
    ((eq? output 'error-console)
       ;; Ustaw obsługę na Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Wyślij do okna dialogowego GUI
    ((eq? output 'gui)
       ;; Ustaw obsługę na okno dialogowe GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Wyślij do okna terminala
    ((eq? output 'terminal)
       ;; Wyjście terminal obsługiwane jest przez display
       (display message)))

  ;; Przywróć domyślną obsługę wiadomości do Message console
  (lumi-message-set-handler 2))
```

### Załaduj funkcję biblioteki

We can load that library function with the Scheme `load` command;

Loading a library file:

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

Hej! We've now got something simpler and shorter to read, that kind of describes itself without comments. To jest satysfakcjonujący wniosek z refaktoryzacji.