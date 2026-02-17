---
title: "Załadunek"
type: docs
weight: 3
---
Gdy tylko funkcja pomocnicza powiększy się, przenieś ją do małego pliku biblioteki. Dzięki temu wtyczka jest skoncentrowana i sprawia, że ​​pomocnika można używać wielokrotnie w wielu wtyczkach.

### Utwórz funkcję biblioteczną

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
```

### Załaduj funkcję biblioteki

Możemy załadować tę funkcję biblioteczną za pomocą polecenia Scheme `load`;

Ładowanie pliku biblioteki:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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

Hej! Mamy teraz coś prostszego i krótszego do przeczytania, co w pewnym sensie opisuje się bez komentarzy. To jest satysfakcjonujący wniosek z refaktoryzacji.