---
title: "Laatste gedachten"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5233667e27065df0a6bc940209f767b9f9e32876d41fa3d09428737b535906e9
---
U beschikt nu over een werkprocedure-plug-in en een kleine helperbibliotheek. In deze serie zijn de kernpatronen geïntroduceerd die u in de meeste Lumi-scripts zult gebruiken:

- Functies: De bouwstenen van onze plug-ins.
- Refactoring: Verbetering van de codestructuur met behoud van functionaliteit.
- Codebibliotheken: centraliseren van herbruikbare functies om onze code schoon en modulair te houden.
- Validatietechnieken: ervoor zorgen dat invoer geldig is voordat onze kernlogica wordt uitgevoerd.

Je zag ook de basisprincipes van het gebruik van Git om veranderingen bij te houden en een strakke projectstructuur te behouden. Die workflow maakt het gemakkelijker om te herhalen zonder dat werkende versies verloren gaan.

Hier is de definitieve versie van onze belangrijkste plug-incode:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

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
;; Doel: Stuurt een bericht naar de statusbalk, retourneert #t bij succes
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar het dialoogvenster, retourneert #t bij succes
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de Error Console, retourneert #t bij succes
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de terminal, retourneert #t bij succes
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Doel: Stuurt een bericht naar de juiste uitvoer, retourneert #t bij succes
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Doel: Controleert dat het bericht een niet-lege tekenreeks is, retourneert #t indien geldig
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Doel: Controleert dat de uitvoer een geldige bestemming is, retourneert #t indien geldig
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