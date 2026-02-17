---
title: "Strutture dati"
type: docs
weight: 3
---
In Scheme, le **strutture dati** sono strumenti essenziali per organizzare, archiviare e manipolare i dati. Consentono agli sviluppatori di creare script efficienti, leggibili e riutilizzabili. Scegliendo la struttura dati corretta per un problema specifico, puoi ottimizzare sia le prestazioni che la chiarezza del tuo codice.

## Strutture dati chiave nello schema

Scheme fornisce diverse strutture dati potenti e versatili, ciascuna adatta per attività specifiche. Le strutture dati primarie includono:

### Elenchi
Gli elenchi sono raccolte ordinate di elementi che possono crescere o ridursi dinamicamente. Sono ideali per dati sequenziali o gerarchici e sono ampiamente utilizzati nella programmazione funzionale.

Caratteristiche principali:
- Dimensioni dinamiche.
- Gli elementi possono essere di tipo misto.
- Comunemente utilizzato per algoritmi ricorsivi e per rappresentare strutture ad albero.

Esempi di utilizzo:
-Gestione delle collezioni di oggetti.
- Rappresentare sequenze o gerarchie.

---

### Vettori
I vettori sono raccolte di elementi di dimensione fissa, indicizzati per un accesso rapido. Sono più adatti per scenari in cui le prestazioni e l'accesso posizionale sono fondamentali.

Caratteristiche principali:
- Dimensioni fisse alla creazione.
- Si accede agli elementi tramite il loro indice.
- Più veloce degli elenchi per determinate operazioni come l'accesso casuale.

Esempi di utilizzo:
- Memorizzazione di configurazioni o dati a dimensione fissa.
- Ricerche rapide e aggiornamenti in base alla posizione.

---

### Scegliere la giusta struttura dei dati

La decisione di utilizzare una **lista** o un **vettore** dipende dalle esigenze specifiche del tuo script. Ecco alcune linee guida:

| Caratteristica | Elenchi | Vettori |
|-------------------------|------------------------------------|--------------------------------|
| **Flessibilità delle dimensioni** | Dinamico | Risolto |
| **Velocità di accesso** | Più lento (accesso sequenziale) | Più veloce (accesso indicizzato) |
| **Facilità di modifica**| Più facile | Più difficile (richiede la riallocazione)|
| **Casi d'uso** | Dati dinamici, ricorsione | Dati statici, ricerche veloci |

---