---
title: "Condizionali"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8e9a64dd1bc1445c996fe17ce6b666b8d597ab16040cf0ef0876232026ff11b2
url: "hub/scripting/fundamentals/Conditionals"
---
I condizionali sono un elemento fondamentale della programmazione: consentono agli script di prendere decisioni e controllare il flusso in base a criteri specifici. In Scheme, basato sul linguaggio di programmazione Scheme, i condizionali aiutano a creare script dinamici e intelligenti che si adattano a input, ambienti o azioni dell'utente in evoluzione.

### Il ruolo dei condizionali in Scheme

I condizionali svolgono diverse funzioni chiave negli script:
- **Guidare la logica:** Eseguono porzioni di codice diverse a seconda che determinate condizioni siano vere o false.
- **Maggiore flessibilità:** Rispondendo dinamicamente a input o stati, aiutano lo script a gestire una varietà di scenari.
- **Semplificare la complessità:** Scompongono le decisioni in strutture gestibili, rendendo il codice più facile da leggere, debuggare e mantenere.

### Tipi di condizionali disponibili

Scheme offre diversi costrutti condizionali, ciascuno adatto a esigenze logiche differenti:
- **`if`:** Per decisioni binarie semplici — un blocco se la condizione è vera, un altro se è falsa.
- **`cond`:** Un potente costrutto a ramificazioni multiple per gestire più condizioni in modo chiaro e strutturato.
- **`and` / `or`:** Operatori logici che valutano combinazioni di condizioni per decisioni più complesse.
- **`else`:** Un caso di fallback che definisce il comportamento quando nessuna condizione specificata è soddisfatta.

### Come funzionano i condizionali

I condizionali implicano in genere:
1. **Valutare una condizione:** Un'espressione di test determina se una condizione è vera o falsa.
2. **Esecuzione ramificata:** In base alla valutazione, lo script sceglie quale blocco di codice eseguire.
3. **Restituire un valore (opzionale):** In alcuni casi, i condizionali producono anche un valore utilizzabile altrove nello script.