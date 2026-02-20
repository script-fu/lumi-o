---
title: "Sviluppo assistito dall'intelligenza artificiale"
type: docs
---
I moderni strumenti di intelligenza artificiale possono accelerare in modo significativo lo sviluppo dei plug-in Lumi agendo come partner di codifica collaborativo.

## Codice VS in modalità agente

L'utilizzo di Visual Studio Code con un assistente AI in **modalità agente** (come la modalità agente di GitHub Copilot o altri assistenti abilitati per gli strumenti) consente di eseguire attività complesse in più passaggi utilizzando il linguaggio naturale.

Invece di completare semplicemente una singola riga di codice, un agente può:
- Leggi l'intero spazio di lavoro per comprendere il contesto.
- Creare nuovi file e directory.
- Esegui comandi del terminale per testare o convalidare gli script.
- Cerca modelli esistenti nella tua codebase.

## Accesso al repository

L'assistenza AI è più efficace quando l'agente ha accesso a **lumi-dev** o al repository del tuo progetto specifico. Grazie alla visibilità sulla codebase esistente, l'agente può:
- Utilizzare **[Utility Libraries](@@LUMI_TOKEN_4@@)** come riferimento per le funzioni di supporto.
- Segui i modelli esistenti per le operazioni GEGL e la gestione dei livelli.
- Riutilizzare il codice boilerplate dei plug-in esistenti.

## Esempio di flusso di lavoro

Puoi chiedere direttamente all'Agent di generare un plug-in completo descrivendo il risultato funzionale desiderato:

> "Utilizzando le utilità e gli esempi di Scheme disponibili nell'area di lavoro, scrivi un nuovo plug-in che crei una guida orizzontale al 50% sull'immagine attiva e la chiami 'Guida centrale'."

L'agente cercherà come creare guide, identificherà la funzione di utilità corretta (come `lumi-image-add-hguide-percent` da `common.scm`) e genererà il file `.scm` completo con il boilerplate di registrazione corretto.

## Migliori pratiche

- **Sii specifico**: descrivi esattamente cosa vuoi che faccia il plug-in.
- **Utilità di riferimento**: incoraggia l'agente a consultare la directory `share/lumi/scripts/` per gli aiutanti di alto livello.
- **Revisione e test**: testa sempre il plug-in generato dall'intelligenza artificiale, spesso è un processo iterativo e creativo.