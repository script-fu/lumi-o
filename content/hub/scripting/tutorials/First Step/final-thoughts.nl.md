---
title: "Laatste gedachten"
type: docs
weight: 10
---
U beschikt nu over een werkprocedure-plug-in en een kleine helperbibliotheek. In deze serie zijn de kernpatronen geïntroduceerd die u in de meeste Lumi-scripts zult gebruiken:

- Functies: De bouwstenen van onze plug-ins.
- Refactoring: Verbetering van de codestructuur met behoud van functionaliteit.
- Codebibliotheken: centraliseren van herbruikbare functies om onze code schoon en modulair te houden.
- Validatietechnieken: ervoor zorgen dat invoer geldig is voordat onze kernlogica wordt uitgevoerd.

Je zag ook de basisprincipes van het gebruik van Git om veranderingen bij te houden en een strakke projectstructuur te behouden. Die workflow maakt het gemakkelijker om te herhalen zonder dat werkende versies verloren gaan.

Hier is de definitieve versie van onze belangrijkste plug-incode:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Bibliotheekcode:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusie

Door de berichtenhelpers in een kleine bibliotheek te herstructureren, blijft de plug-in gefocust op de intentie en bevat de bibliotheek de implementatiedetails. Validatie en consistente berichtroutering zorgen ervoor dat fouten voorspelbaar blijven.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Volgende stappen:

- Verplaats herbruikbare helpers naar een speciaal bibliotheekbestand.
- Houd plug-ins klein en geef procedures een naam voor wat ze doen.
- Voeg validatie toe aan grenzen (invoer, bestandspaden, menu-opties).

Bewaar het eindresultaat als twee bestanden in uw plug-insrepository:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`