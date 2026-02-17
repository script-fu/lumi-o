---
title: "Belastning"
type: docs
weight: 3
---
Så snart en hjälpfunktion växer, flytta den till en liten biblioteksfil. Det håller plugin-programmet fokuserat och gör hjälparen återanvändbar över flera plug-ins.

### Skapa en biblioteksfunktion

Vi kan ta funktionen skicka meddelande och skapa en ny fil med det som innehåll. Spara filen i din repo-mapp, inte plugin-delen, kanske nära den översta nivån;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **schema/**: Detta är din huvudkatalog för lagring av din schemakod.
  - **bibliotek/**: Det är här delade funktioner som `send-message.scm` lever.
  - **plugins/**: Det är här dina individuella plugins lagras.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Exempel på en biblioteksfunktion send-message.scm

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

### Ladda biblioteksfunktionen

Vi kan ladda den biblioteksfunktionen med kommandot Scheme `load`;

Laddar en biblioteksfil:

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

Hej! Vi har nu fått något enklare och kortare att läsa, som beskriver sig själv utan kommentarer. Detta är den tillfredsställande slutsatsen av refaktorering.