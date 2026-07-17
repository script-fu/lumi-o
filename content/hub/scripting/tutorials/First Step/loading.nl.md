---
title: "Laden"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
url: "hub/scripting/tutorials/First Step/loading"
---
Zodra een helperfunctie groeit, verplaatst u deze naar een klein bibliotheekbestand. Dat houdt de plug-in gefocust en maakt de helper herbruikbaar voor meerdere plug-ins.

### Maak een bibliotheekfunctie

We kunnen de functie voor het verzenden van berichten nemen en een nieuw bestand maken met dat als inhoud. Sla het bestand op in uw repo-map, niet in het plug-insgedeelte, misschien op het hoogste niveau;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Dit is uw hoofdmap voor het opslaan van uw Scheme-code.
  - **bibliotheek/**: Dit is waar gedeelde functies zoals `send-message.scm` live zijn.
  - **plug-ins/**: hier worden uw individuele plug-ins opgeslagen.
    - **hello-world/**: Map voor de specifieke "Hello World!"-plug-in.
      - **hello-world.scm**: Scriptbestand van de plug-in.

Voorbeeld van een bibliotheekfunctie send-message.scm

```scheme
;; Functie voor berichtuitvoer naar verschillende bestemmingen
(define (send-message message output)
  (cond
    ;; Verzenden naar de berichtenconsole
    ((eq? output 'error-console)
       ;; Stel de handler in op berichtenconsole
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Verzenden naar het GUI-dialoogvenster
    ((eq? output 'gui)
       ;; Stel de handler in op GUI-dialoog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Verzenden naar het terminalvenster
    ((eq? output 'terminal)
       ;; Terminal-uitvoer wordt afgehandeld met display
       (display message)))

  ;; Herstel de standaard berichtenhandler naar de berichtenconsole
  (lumi-message-set-handler 2))
```

### Laad de bibliotheekfunctie

We kunnen die bibliotheekfunctie laden met de opdracht Scheme `load`;

Een bibliotheekbestand laden:

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

Hé! We hebben nu iets eenvoudigers en korters om te lezen, dat zichzelf beschrijft zonder commentaar. Dit is de bevredigende conclusie van refactoring.