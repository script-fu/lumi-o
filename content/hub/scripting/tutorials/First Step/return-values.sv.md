---
title: "Returvärden"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 586ad49d823eb3fa85ff606b73c3f95e3fd3efb8bd9a0c9482e2c3e21f953de9
url: "hub/scripting/tutorials/First Step/return-values"
---
Returvärden är viktiga eftersom de låter dig styra flödet utan extra tillstånd. I Scheme blir det senast utvärderade uttrycket returvärdet.

Den här sidan använder valideringshjälparna från meddelandeexemplet för att visa hur explicita returvärden gör kod lättare att komponera.

### Vad är ett returvärde?

I Scheme bestäms returvärdet för en funktion av det sista uttrycket som funktionen utvärderar. Detta betyder att vad den sista koden i funktionen utvärderar till kommer att returneras som resultatet av funktionen. Om inget värde uttryckligen returneras returnerar funktionen `#f` (falskt) eller `undefined`.

Låt oss återvända till valideringsfunktionen, (är-giltig-sträng?)

```scheme
;; Syfte: Validerar att meddelandet är en icke-tom sträng
(define (is-valid-string? message)
  ;; Kontrollera om meddelandet är en icke-tom sträng
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

I den här funktionen, om meddelandet är ogiltigt, skapas ett fel. Men om meddelandet är giltigt ges inget explicit returvärde och funktionen returnerar `#f` som standard.

### Gör returvärden explicita

Vi kan förbättra detta genom att göra returvärdet mer explicit. Till exempel kan vi returnera `#t` (true) om meddelandet är giltigt:

```scheme
;; Syfte: Validerar att meddelandet skickas till en giltig utdata
(define (is-valid-output-display? output)
  ;; Kontrollera om utdata är en av de förväntade visningsdestinationerna
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

I den här versionen kommer funktionen att returnera `#t` när meddelandet är giltigt, vilket ger ett tydligt resultat. Detta gör att funktionen kan användas mer flexibelt i andra sammanhang där ett booleskt resultat behövs.

### Använda returvärden effektivt

Genom att bestämma vad våra funktioner returnerar kan vi göra dem mer förutsägbara och användbara. Att returnera värden som `#t`, `#f` eller ett specifikt resultat ger oss mer kontroll över hur funktionen interagerar med resten av koden. Du kan till exempel använda returvärdet för att fatta ytterligare beslut i den anropande funktionen eller skicka det som ett argument till en annan funktion.

Här är ett enkelt exempel på hur man använder ett returvärde för att styra flödet av logik:

```scheme
;; Syfte: Skickar ett meddelande till rätt utdatamål
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

I det här fallet förlitar sig (sänd-meddelande) på returvärdet för (is-valid-output-display?) för att avgöra om det ska fortsätta.
Det villkorliga uttalandet `cond` hoppas över om det första testet misslyckas. Lägg också märke till hur det läser på ett ganska naturligt sätt, om är giltig utdatadisplay?

## Om Statement Logic in Scheme

Före exemplet med refactored bibliotek, här är en snabb genomgång av villkorlig logik. Scheme använder `if` för att välja mellan två vägar.

Här är en enkel form av ett `if` uttalande:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Denna struktur kontrollerar villkoret, och om villkoret är sant, utför den den första åtgärden. Om villkoret är falskt utförs den andra åtgärden.

I fall där du behöver utföra flera åtgärder när villkoret är sant eller falskt, kan du använda `begin` för att gruppera dem:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Detta gör att du kan hantera mer komplexa situationer, där flera uttryck eller satser måste exekveras beroende på resultatet av det villkorliga testet.

Okej, här är bibliotekskoden med returvärden inbäddade och används för att styra exekveringsprocessen.

### Refaktorerad med returvärden

```scheme
;; Syfte: Skickar ett meddelande till statusfältet, returnerar #t vid lyckat resultat
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till dialogrutan, returnerar #t vid lyckat resultat
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till felkonsol, returnerar #t vid lyckat resultat
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till terminal, returnerar #t vid lyckat resultat
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Syfte: Skickar ett meddelande till rätt utdata, returnerar #t vid lyckat resultat
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Syfte: Validerar att meddelandet är en icke-tom sträng, returnerar #t om giltigt
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Syfte: Validerar att utdata är en giltig destination, returnerar #t om giltig
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Slutsats

Returvärden är en grundläggande del av att göra funktioner flexibla och återanvändbara. Genom att noggrant bestämma vad varje funktion ska returnera kan vi säkerställa att våra funktioner interagerar bra med varandra och ge användbar information till resten av koden. Oavsett om det är att returnera `#t` eller `#f`, eller något mer specifikt, ger returvärden oss ett sätt att kontrollera flödet av våra program och hantera olika resultat.