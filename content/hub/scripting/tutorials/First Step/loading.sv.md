---
title: "Belastning"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
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

- **scheme/**: This is your main directory for storing your Scheme code.
  - **bibliotek/**: Det är här delade funktioner som `send-message.scm` lever.
  - **plugins/**: Det är här dina individuella plugins lagras.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Exempel på en biblioteksfunktion send-message.scm

```scheme
;; Funktion för att hantera meddelandeutdata till olika destinationer
(define (send-message message output)
  (cond
    ;; Skicka till Message console
    ((eq? output 'error-console)
       ;; Ställ in hanteraren till Message console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Skicka till GUI-dialogrutan
    ((eq? output 'gui)
       ;; Ställ in hanteraren till GUI-dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Skicka till terminalfönstret
    ((eq? output 'terminal)
       ;; Terminal-utdata hanteras med display
       (display message)))

  ;; Återställ standardmeddelandehanteraren till Message console
  (lumi-message-set-handler 2))
```

### Ladda biblioteksfunktionen

Vi kan ladda den biblioteksfunktionen med kommandot Scheme `load`;

Laddar en biblioteksfil:

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

Hej! Vi har nu fått något enklare och kortare att läsa, som beskriver sig själv utan kommentarer. Detta är den tillfredsställande slutsatsen av refaktorering.