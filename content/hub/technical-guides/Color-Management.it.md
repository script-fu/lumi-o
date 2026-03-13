---
title: "Gestione del colore"
type: docs
weight: 15
---
Lumi-o è configurato per funzionare immediatamente. Finché lavori su un'immagine con **precisione a 16 bit o superiore**, il software è già configurato per utilizzare il soft-proofing (CMYK) predefinito in bundle e i profili sRGB integrati; dovrebbe funzionare tutto senza alcuna configurazione.

Per coloro che necessitano di un controllo più approfondito, questa guida spiega il modello principale di gestione del colore di Lumi, la differenza tra un profilo immagine e un profilo soft-proof, dove risiedono i controlli e esattamente come i profili predefiniti si integrano con l'applicazione.

## Riepilogo rapido

Lumi utilizza tre diversi ruoli del profilo:

1. **Profilo di lavoro dell'immagine**
   - Definisce il significato dei numeri RGB o della scala di grigi dell'immagine.
   - Utilizzato per operazioni di assegnazione/conversione.
   - Esempi tipici: sRGB integrato, Adobe RGB.

2. **Visualizza profilo**
   - Descrive il tuo monitor.
   - Utilizzato per mostrare correttamente l'immagine sullo schermo.
   - Solitamente fornito dal sistema o scelto nelle Preferenze.

3. **Profilo soft-proof**
   - Simula un altro dispositivo di output o una condizione di stampa.
   - **Non** ridefinisce i valori dei pixel dell'immagine.
   - Esempi tipici: profili di stampa CMYK come `CoatedFOGRA39`.

## Profilo immagine e profilo soft-proof

### Profilo immagine

Usalo quando vuoi dire a Lumi in quale spazio colore si trova effettivamente l'immagine.

Due operazioni comuni:

- **Assegna profilo**
  - Modifica l'etichetta del profilo allegata all'immagine.
  - **Non** converte i valori dei pixel.
  - Utilizzare solo quando i numeri di pixel sono già nello spazio di quel profilo.

- **Converti in profilo**
  - Converte i valori dei pixel dal profilo immagine corrente a uno nuovo.
  - Utilizzare quando si desidera che l'immagine si sposti realmente in uno spazio di lavoro diverso.

**Posizioni del menu:**
- Immagine > Gestione colore > Assegna profilo colore...
- Immagine > Gestione colore > Converti in profilo colore...

### Profilo Soft-Proof

Utilizzalo quando desideri visualizzare in anteprima come verrebbe riprodotta l'immagine su un dispositivo di destinazione o su una condizione di stampa.

Prova soft:
- lascia da solo lo spazio di lavoro dell'immagine
- modifica la pipeline di anteprima
- può contrassegnare i colori fuori gamma
- è destinato all'anteprima, non alla riassegnazione dei dati dell'immagine

**Posizioni del menu:**
- Immagine > Gestione colore > Impostazioni Soft-Proof > Scegli profilo Soft-Proof...
- Immagine > Gestione colore > Impostazioni prova video > Intento di rendering
- Immagine > Gestione colore > Impostazioni prova video > Compensazione del punto nero
- Visualizza > Gestione colore > Abilita anteprima prova soft
- Visualizza > Gestione colore > Seleziona colori fuori gamma

## Come visualizzare l'anteprima della prova soft

Esistono due punti di ingresso principali per attivare/disattivare le prove software.

### 1. Menu Visualizza

Utilizzo:
- Visualizza > Gestione colore > Abilita anteprima prova soft

Ciò attiva o disattiva la simulazione dell'anteprima per la visualizzazione corrente.

### 2. Attiva/disattiva la barra di stato

Lumi espone anche il soft-proofing direttamente nella barra di stato inferiore.

- **Clic sinistro** (attiva/disattiva): attiva o disattiva i colori di prova
- **Fare clic con il pulsante destro del mouse**: apre il popover di soft-proof in cui è possibile modificare:
  - profilo attuale
  - selettore del profilo
  - intento di rendering
  - compensazione del punto nero
  - marcatura fuori gamma

{{< callout type="warning" >}}
**Nota importante sulla precisione**
L'anteprima soft-proof è abilitata solo per le immagini **16 bit e 32 bit**.
Per le immagini **8 bit**, l'interruttore è disabilitato e Lumi ti chiederà di convertire la precisione in una profondità maggiore prima di visualizzare in anteprima i colori in modo accurato.
{{< /callout >}}

## Preferenze e impostazioni predefinite

I default globali risiedono in:
- Modifica > Preferenze > Gestione coloreSezioni rilevanti:
- **Profilo monitor manuale**
- **Profilo RGB preferito**
- **Profilo scala di grigi preferito**
- **Prova soft**

### Impostazioni predefinite attuali di Lumi

#### Spazi di lavoro

ICC dello spazio di lavoro in bundle attualmente offerti dalla cartella dei dati condivisi:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Per il lavoro sRGB standard, Lumi fornisce internamente anche un **profilo di lavoro sRGB integrato**.

#### Impostazioni predefinite per la prova software

Profili soft-proof in bundle attualmente installati:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Se disponibile, `CoatedFOGRA39.icc` viene utilizzato come profilo di riferimento soft-proof/CMYK in bundle predefinito.

## Flussi di lavoro pratici

### Per la verniciatura e il normale lavoro su serigrafia

- Conserva l'immagine nello spazio sRGB integrato o in un altro spazio di lavoro RGB valido.
- Consenti a Lumi di utilizzare il profilo del monitor di sistema, se disponibile.

### Per l'anteprima di stampa

- Mantieni l'immagine nel suo spazio di lavoro RGB standard.
- Scegli un profilo soft-proof che corrisponda alle condizioni di stampa target (ad esempio FOGRA39).
- Abilita l'anteprima della prova virtuale.
- Facoltativamente, abilita gli avvisi sulla gamma per visualizzare gli intenti di rendering ritagliati.