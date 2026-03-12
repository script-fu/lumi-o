---
title: "Caching del pennello"
type: docs
---
La memorizzazione nella cache dei pennelli è progettata per rendere i tuoi pennelli preferiti veloci il più presto possibile. Invece di ricalcolare più e più volte lo stesso timbro di pennello trasformato, Lumi può conservare una cache salvata delle forme di pennello effettivamente utilizzate e ricaricare automaticamente la cache in un secondo momento.

## Panoramica

La funzionalità si basa sull'idea che molti pennelli espressivi rivisitano ancora le stesse combinazioni pratiche di dimensioni, angolo, durezza e proporzioni durante la pittura. Quando queste combinazioni vengono riutilizzate, Lumi può fornire il timbro del pennello trasformato direttamente dalla cache invece di ricostruirlo.

Il risultato è:

- avvio del tratto più veloce dopo il salvataggio della cache
- Uso ripetuto più fluido dei preset preferiti
- Meno ricalcoli sprecati durante lunghe sessioni di pittura
- ripristino automatico delle cache salvate quando il preset viene utilizzato nuovamente

## Intento

La memorizzazione nella cache dei pennelli è pensata per i pennelli a cui torni spesso: preimpostazioni di pittura di base, strumenti di inchiostrazione preferiti, pennelli asciutti con texture e altri pennelli i cui timbri trasformati sono abbastanza costosi da essere notati.

L'obiettivo non è pre-cottura di ogni stato teorico del pennello. L'obiettivo è consentire all'utilizzo reale della pittura di popolare prima gli stati più preziosi, quindi salvare la cache popolata in modo che il pennello sia già caldo la prossima volta che lo si utilizza.

## Come funziona

La memorizzazione nella cache dei pennelli funziona insieme alla quantizzazione dei pennelli.

Quando la quantizzazione è abilitata per un preset di dinamiche, le uscite che influenzano la trasformazione vengono agganciate a passaggi discreti. Ciò fornisce a Lumi un insieme finito di stati del pennello riutilizzabili. Mentre dipingi:

1. Lumi controlla se il timbro trasformato esiste già nella cache.
2. In tal caso, il timbro viene immediatamente riutilizzato.
3. In caso contrario, Lumi lo costruisce una volta e lo immagazzina.
4. Nel tempo, la cache si riempie con gli stati del pennello effettivamente utilizzati.

Se salvi la cache, Lumi può caricarla automaticamente in un secondo momento in modo che il pennello inizi più vicino a uno stato riscaldato invece di ricostruire tutto da zero.

## Flusso di lavoro tipico

1. Scegli un pennello predefinito che usi frequentemente.
2. Abilita la quantizzazione per la sua dinamica.
3. Dipingi normalmente per un po' in modo che la cache si riempia in modo organico.
4. Apri **Tool Preset Editor** e controlla la sezione **Preset Cache**.
5. Guarda le metriche in tempo reale:
   - **Percentuale di successo**
   - **Copertura**
   - **Memoria**
6. Fai clic su **Salva** quando la cache sembra utile.
7. Nelle sessioni successive, Lumi carica automaticamente la cache salvata quando la preimpostazione diventa attiva.

Ciò rende la preimpostazione più rapida, soprattutto per i pennelli con trasformazioni costose o timbri di grandi dimensioni.

## Dove trovarlo

### Editor delle dinamiche

Utilizza l'**Editor delle dinamiche** per controllare la quantizzazione:

- abilitare la quantizzazione
- scegli il conteggio dei passi globale
- facoltativamente sovrascrivere i conteggi dei passi per asse di uscita

La quantizzazione è ciò che rende pratica la cache riducendo la variazione continua in contenitori riutilizzabili.

### Editor delle preimpostazioni degli strumenti

Utilizza lo **Strumento Editor delle preimpostazioni** per gestire la cache per la preimpostazione corrente:

- **Salva**: mantiene la cache in memoria corrente su disco
- **Carica**: ripristina una cache salvata in precedenza
- **Memoria libera**: rilascia la cache in memoria senza eliminare la copia salvata
- **Rimuovi**: elimina la cache salvata dal disco

L'espansore **Preset Cache** mostra anche il tasso di successo in tempo reale, la copertura e l'utilizzo della memoria.

## Cosa viene memorizzato nella cache

La memorizzazione nella cache dei pennelli ha come target i timbri dei pennelli trasformati: i costosi risultati rasterizzati dopo che dimensioni, angolo, durezza, proporzioni e i relativi input di trasformazione sono stati risolti.

È particolarmente utile quando:- Il pennello ha un lavoro di trasformazione costoso
- la stessa preimpostazione viene utilizzata in molte sessioni
- il pennello rivisita ripetutamente stati dinamici simili
- La rapidità di risposta all'avvio è importante

È meno utile per i pennelli il cui stato di trasformazione cambia notevolmente e si ripete raramente.

## Caricamento automatico

Le cache salvate hanno lo scopo di aiutare dall'inizio di una sessione, non solo dopo aver già dipinto per un po'.

Quando esiste una cache salvata per il preset attivo, Lumi può caricarla automaticamente in modo che il tuo pennello preferito inizi con molti stati utili già disponibili. Ciò riduce il periodo di avvio a freddo e avvicina immediatamente la spazzola alla massima reattività.

## Sicurezza della memoria

La memorizzazione nella cache delle spazzole è progettata per migliorare la velocità senza prendere il controllo della macchina.

Lumi tiene traccia dell'utilizzo della memoria cache, lo espone nell'interfaccia utente e applica limiti di runtime in caso di pressione della memoria. Se il sistema ha poca RAM disponibile, la crescita della cache viene limitata automaticamente.

## Migliori casi d'uso

La memorizzazione nella cache dei pennelli è particolarmente utile per:

- spazzole quotidiane preferite
- pennelli strutturati utilizzati in un dipinto
- grandi pennelli espressivi con costi di trasformazione elevati
- preimpostazioni dei pennelli condivise tra flussi di lavoro di illustrazione ripetuti
- preset che vuoi sentire "pronti" non appena li selezioni

## In breve

La memorizzazione nella cache dei pennelli consente a Lumi di apprendere gli stati dei pennelli che usi effettivamente, salvarli e ripristinarli automaticamente in un secondo momento. È una pratica funzionalità di velocità per i preset preferiti: dipingi con il pennello, lascia che la cache si riempia, salvala e le sessioni future inizieranno più velocemente.