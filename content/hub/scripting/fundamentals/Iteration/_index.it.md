---
title: "Iterazione"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: df3e2118b9a580de4eed6ac56d9717aa3cbf555ab66bb49fabb4164b2994af91
url: "hub/scripting/fundamentals/Iteration"
---
L'iterazione è un pilastro della programmazione: consente agli script di ripetere azioni ed elaborare raccolte di dati in modo efficiente. In Scheme, l'iterazione offre strumenti per automatizzare compiti ripetitivi, manipolare strutture dati e creare schemi di esecuzione sofisticati.

### Il ruolo dell'iterazione in Scheme

L'iterazione svolge diversi compiti essenziali:
- **Automatizzare la ripetizione:** eseguire la stessa azione più volte senza duplicare codice.
- **Aumentare l'efficienza:** elaborare strutture dati in modo sistematico per operazioni su larga scala.
- **Snellire il codice:** l'iterazione elimina ridondanza e rende il codice più conciso, leggibile e manutenibile.

### Tipi di iterazione disponibili

Scheme offre diverse costrutti:
- **map:** applica una funzione a ogni elemento di una lista, restituendo una nuova lista di risultati.
- **for-each:** simile a `map`, ma esegue una funzione su ogni elemento senza restituire un risultato.
- **do:** costrutto di loop generale per molti processi iterativi.
- **recursion:** tecnica potente in cui le funzioni chiamano se stesse.

### Come funziona l'iterazione

In genere comprende:
1. **Definire la ripetizione:** l'azione da ripetere e i dati o l'intervallo da elaborare.
2. **Eseguire in sequenza:** ripetere l'azione per ogni elemento, passo o condizione fino al completamento.
3. **Restituire un risultato (opzionale):** a seconda del costrutto, l'iterazione può produrre un risultato o modificare lo stato.

Questi costrutti aiutano a scrivere script adattabili, efficienti ed eleganti per compiti complessi.
