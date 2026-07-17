---
title: "Sviluppo assistito dall'IA"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 7867639dd5e951131133f23635a10898d35de3c275f48b78b7ed7091c73e15c4
url: "hub/scripting/tools/ai-assisted"
---
Gli strumenti IA moderni possono accelerare notevolmente lo sviluppo di plug-in Lumi fungendo da partner di codifica collaborativo.

## VS Code in modalità Agent

Usare Visual Studio Code con un assistente IA in **modalità Agent** (come la modalità Agent di GitHub Copilot o altri assistenti con strumenti) consente di eseguire compiti complessi in più passaggi usando il linguaggio naturale.

Invece di completare una sola riga di codice, un agent può:
- leggere l'intero workspace per capire il contesto
- creare nuovi file e directory
- eseguire comandi da terminale per testare o convalidare gli script
- cercare pattern esistenti nella codebase

## Accesso al repository

L'assistenza IA è più efficace quando l'agent ha accesso a **lumi-dev** o al repository del progetto. Con visibilità sul codice esistente, l'agent può:
- usare le **[Utility Libraries]({{< ref "/hub/scripting/reference/utility-browser" >}})** come riferimento per le funzioni di supporto
- seguire i pattern esistenti per operazioni GEGL e gestione dei livelli
- riutilizzare il codice boilerplate dei plug-in consolidati

## Flusso di lavoro di esempio

Potete chiedere direttamente all'agent di generare un plug-in completo descrivendo il risultato funzionale desiderato:

> "Usando le utility Scheme e gli esempi disponibili nel workspace, scrivi un nuovo plug-in che crea una guida orizzontale al 50% sull'immagine attiva e la chiama 'Center Guide'."

L'agent cercherà come creare guide, identificherà la funzione utility corretta (come `lumi-image-add-hguide-percent` da `common.scm`) e genererà il file `.scm` completo con il boilerplate di registrazione corretto.

## Buone pratiche

- **Siate specifici**: descrivete esattamente cosa deve fare il plug-in.
- **Referenziate le utility**: incoraggiate l'agent a consultare la directory `share/lumi/scripts/` per trovare helper di alto livello.
- **Revisione e test**: testate sempre il plug-in generato dall'IA — spesso è un processo iterativo e creativo.
