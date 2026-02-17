---
title: "Geldigmaking"
type: docs
weight: 4
---
Bij het bouwen van robuuste plug-ins is het belangrijk ervoor te zorgen dat onze functies fouten netjes afhandelen en werken zoals verwacht, zelfs in geval van misbruik of onverwachte invoer. Validatie helpt de integriteit van de functie te beschermen en crashes of onbedoeld gedrag te voorkomen.

Laten we eens kijken hoe we de functie `send-message` kunnen verbeteren door validatiecontroles toe te voegen om ervoor te zorgen dat de invoer correct wordt verwerkt.

### Valideer invoer

Voordat we een bericht verzenden, moeten we ervoor zorgen dat het argument `output` dat wordt doorgegeven aan de functie `send-message` geldig is. We kunnen een controle toevoegen om te bevestigen dat de uitvoerbestemming een van de verwachte waarden is (gui, error-console of terminal).

Voorbeeld:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Send to the Error Console
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Send to the GUI dialog box
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Send to the terminal window
      ((eq? output 'terminal)
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In dit voorbeeld gebruiken we `member` om te controleren of het argument `output` geldig is. Als dit niet het geval is, genereert de functie een fout met een duidelijk bericht, waardoor wordt voorkomen dat ongeldige waarden problemen veroorzaken.

### Lege berichten verwerken

Het is ook handig om ervoor te zorgen dat het argument `message` geldig is. Als bijvoorbeeld een lege string of #f (false) als bericht wordt doorgegeven, moet de functie dit netjes afhandelen.

Voorbeeld van het afhandelen van een leeg bericht:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Deze aanpak zorgt ervoor dat de functie altijd geldige invoer ontvangt, waardoor de betrouwbaarheid wordt verbeterd en onverwacht gedrag wordt voorkomen.

### Gecombineerd validatievoorbeeld

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Send to the Error Console
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Send to the GUI dialog box
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Send to the terminal window
        ((eq? output 'terminal)
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

In deze versie:
- De functie controleert eerst of `message` leeg of ongeldig is. Als het bericht geldig is, gaat het verder met het controleren of `output` een van de geaccepteerde waarden is (`gui`, `error-console`, of `terminal`).
- Als beide controles slagen, wordt het bericht naar de juiste uitgang verzonden. Anders verschijnt er een foutmelding met een duidelijke uitleg.
- Er wordt extra gecontroleerd of het bericht ook een string is.

Deze gecombineerde validatiefunctie houdt de code schoner en zorgt ervoor dat beide invoer wordt gevalideerd voordat er actie wordt ondernomen, waardoor de functie robuuster wordt. Merk op dat we ook een berichtensysteem voor foutopsporing aan het inbouwen zijn. Wanneer de
code mislukt, we krijgen een reden, een reden die we zelf hebben geschreven.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```