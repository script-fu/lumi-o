---
title: "Hallo wereld!"
type: docs
weight: 1
---
In deze tutorial wordt de minimale structuur van een Scheme-plug-in besproken. Sommige regels zijn “standaard”: ze zijn nodig voor Lumi om het bestand te laden, zelfs als je ze nog niet helemaal begrijpt.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

Op hoog niveau zul je:

1. Definieer een functie
2. Registreer het zodat het verschijnt in de proceduredatabase
3. (Optioneel) Voeg een menu-item toe
4. Installeer het bestand in een map met plug-ins

### Definieer een functie

Een functie, ook wel bekend als een _procedure_, is een stuk code met een naam en een doel; er is invoer nodig en er wordt uitvoer geproduceerd.

**Invoer** > **_Functie_** > **Uitvoer**

### Registreer de functie

Registreren is het plaatsen van de functienaam op een lijst, zodat Lumi hiervan op de hoogte is.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Link naar het menu

Dit vertelt Lumi waar u uw functie in het menusysteem kunt vinden.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Hierdoor wordt het menu "Funky" in de hoofdmenubalk weergegeven. Wijzig het pad om de plug-in ergens anders te plaatsen. Het pad `<Image>/Funky` betekent dat de plug-in verschijnt onder de menucategorie **Afbeelding**. U kunt `<Image>` wijzigen in `<Tools>`, `<Filters>`, enz., afhankelijk van waar u de plug-in wilt laten verschijnen.

### Opmerkingen

In Scheme, de basistaal van Scheme, worden opmerkingen over het algemeen gedaan door een nuttige regel tekst vooraf te laten gaan door `;;`. Uw gebruik van commentaar hangt af van uw vaardigheid als codeur. Als u af en toe codeert, zal meer commentaar helpen. Als je de hele tijd codeert, is de code net zo gemakkelijk te lezen als het commentaar. Bovendien heeft de code bij functioneel programmeren de neiging beschrijvend genoeg te worden om als een script te lezen.

### Syntaxis

Code heeft meestal kleine regels voor het plaatsen van items op een regel, zodat we de regel gemakkelijk kunnen lezen. Een zin kan bijvoorbeeld een spatie bevatten na een komma of punt. Het komt de leesbaarheid ten goede.

Code kan dingen op een vergelijkbare manier regelen, wat er in eerste instantie misschien vreemd uitziet:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Voorbeeldcode

Hier is het volledige voorbeeld. De meeste Lumi-procedures worden voorafgegaan door `lumi-`. `lumi-message` drukt bijvoorbeeld een tekenreeks af naar de geconfigureerde berichtenhandler.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### Installeer de plug-in

1. Ga naar **Lumi -> Bewerken -> Voorkeuren -> Mappen -> Plug-ins**.
2. Voeg uw plug-insmap [repo](/hub/scripting/tools/git) toe aan de lijst.
3. Maak een map voor de plug-in en sla de bovenstaande voorbeeldcode op als `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Klik met de rechtermuisknop op het bestand `hello-world.scm`.
5. Ga naar **Eigenschappen -> Machtigingen -> Uitvoeren van bestand als programma toestaan**.
6. Start Lumi opnieuw.

### Probeer de plug-in

De plug-in zou nu moeten verschijnen onder het menu "Funky" in het hoofdvenster van Lumi. Klik erop en het bericht "Hallo wereld!" zou moeten verschijnen. bericht. Probeer de code aan te passen, bijvoorbeeld door de berichttekst te wijzigen, en sla het bestand op. Wanneer u de plug-in opnieuw uitvoert, worden uw wijzigingen doorgevoerd zonder Lumi opnieuw te starten.

Probeer te experimenteren door het menupad te wijzigen. `"<Image>/File"` plaatst het bijvoorbeeld in het menu Bestand, en `"<Image>/File/Funky"` maakt een nieuwe sectie in het menu Bestand. Dit is een geweldige manier om aan te passen waar uw plug-in verschijnt en om uw tools te ordenen.