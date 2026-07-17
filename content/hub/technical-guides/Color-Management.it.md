---
title: "Gestione del colore"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e124f17c1f65c73f4e135c25dd7962eb44f1d0676147a7e4bcbf6dc8ecf51e69
---

Lumi-o è configurato per funzionare immediatamente. Finché lavori su un'immagine con **precisione a 16 bit o superiore**, il software è già impostato per usare il soft-proofing (CMYK) predefinito incluso e i profili sRGB integrati; tutto dovrebbe funzionare senza alcuna configurazione.

Per chi necessita di un controllo più approfondito, questa guida spiega il modello principale di gestione del colore di Lumi, la differenza tra un profilo immagine e un profilo soft-proof, dove si trovano i controlli e come i profili predefiniti sono inclusi nell'applicazione.

## Riepilogo rapido

Lumi utilizza tre ruoli di profilo distinti:

1. **Profilo di lavoro dell'immagine**
   - Definisce il significato dei valori RGB o della scala di grigi dell'immagine.
   - Viene usato per le operazioni di assegnazione e conversione.
   - Esempi tipici: sRGB integrato, Adobe RGB.

2. **Profilo del monitor**
   - Descrive il tuo monitor.
   - Viene usato per visualizzare correttamente l'immagine sullo schermo.
   - Di solito è fornito dal sistema o scelto nelle Preferenze.

3. **Profilo soft-proof**
   - Simula un altro dispositivo di output o una condizione di stampa.
   - **Non** ridefinisce i valori dei pixel dell'immagine.
   - Esempi tipici: profili CMYK da stampa come `CoatedFOGRA39`.

## Profilo immagine e profilo soft-proof

### Profilo immagine

Usalo quando vuoi indicare a Lumi in quale spazio colore si trova effettivamente l'immagine.

Due operazioni comuni:

- **Assegna profilo**
  - Modifica l'etichetta del profilo associata all'immagine.
  - **Non** converte i valori dei pixel.
  - Usalo solo quando i valori dei pixel appartengono già a quello spazio colore.

- **Converti in profilo**
  - Converte i valori dei pixel dal profilo immagine corrente a uno nuovo.
  - Usalo quando vuoi che l'immagine passi effettivamente a uno spazio di lavoro diverso.

**Posizioni nel menu:**
- Immagine > Gestione colore > Assegna profilo colore...
- Immagine > Gestione colore > Converti in profilo colore...

### Profilo soft-proof

Usalo quando vuoi visualizzare in anteprima come verrebbe riprodotta l'immagine su un dispositivo di destinazione o in una condizione di stampa.

Il soft-proofing:
- lascia invariato lo spazio di lavoro dell'immagine
- modifica la pipeline di anteprima
- può contrassegnare i colori fuori gamma
- è pensato per l'anteprima, non per riassegnare i dati dell'immagine

**Posizioni nel menu:**
- Immagine > Gestione colore > Impostazioni soft-proof > Scegli profilo soft-proof...
- Immagine > Gestione colore > Impostazioni soft-proof > Intento di rendering
- Immagine > Gestione colore > Impostazioni soft-proof > Compensazione del punto nero
- Visualizza > Gestione colore > Abilita anteprima soft-proof
- Visualizza > Gestione colore > Contrassegna colori fuori gamma

## Come visualizzare l'anteprima soft-proof

Esistono due modi principali per attivare o disattivare il soft-proof.

### 1. Menu Visualizza

Usa:
- Visualizza > Gestione colore > Abilita anteprima soft-proof

Questo attiva o disattiva la simulazione di anteprima per la visualizzazione corrente.

### 2. Interruttore nella barra di stato

Lumi offre anche l'accesso diretto al soft-proofing nella barra di stato inferiore.

- **Clic sinistro** (interruttore): attiva o disattiva i colori di prova
- **Clic destro**: apre il popover del soft-proofing, dove puoi regolare:
  - profilo corrente
  - selettore del profilo
  - intento di rendering
  - compensazione del punto nero
  - contrassegno dei colori fuori gamma

{{< callout type="warning" >}}
**Nota importante sulla precisione**
L'anteprima soft-proof è abilitata solo per le immagini a **16 e 32 bit**.
Per le immagini a **8 bit**, l'interruttore è disabilitato e Lumi chiederà di convertire prima la precisione a una profondità maggiore prima di visualizzare i colori in modo accurato.
{{< /callout >}}

## Preferenze e impostazioni predefinite

Le impostazioni globali predefinite si trovano in:
- Modifica > Preferenze > Gestione colore

Sezioni rilevanti:
- **Profilo monitor manuale**
- **Profilo RGB preferito**
- **Profilo scala di grigi preferito**
- **Soft-proofing**

### Impostazioni predefinite attuali di Lumi

#### Spazi di lavoro

Profili ICC degli spazi di lavoro attualmente inclusi nella cartella dati condivisa:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Per il lavoro sRGB standard, Lumi fornisce anche internamente un **profilo di lavoro sRGB integrato**.

#### Impostazioni predefinite soft-proof

Profili soft-proof attualmente inclusi e installati:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Quando disponibile, `CoatedFOGRA39.icc` viene usato come profilo di riferimento soft-proof/CMYK incluso per impostazione predefinita.

## Flussi di lavoro pratici

### Per la pittura e il lavoro abituale su schermo

- Mantieni l'immagine in sRGB integrato o in un altro spazio di lavoro RGB valido.
- Lascia che Lumi usi il profilo del monitor di sistema, se disponibile.

### Per l'anteprima di stampa

- Mantieni l'immagine nel suo spazio di lavoro RGB standard.
- Scegli un profilo soft-proof che corrisponda alla condizione di stampa di destinazione (ad es. FOGRA39).
- Abilita l'anteprima soft-proof.
- Facoltativamente, abilita gli avvisi di gamma per vedere i colori tagliati in base all'intento di rendering.
