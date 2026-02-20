---
title: "Browser delle procedure"
type: docs
---
Il Procedure Browser è lo strumento di riferimento principale per scoprire le centinaia di funzioni disponibili nel Procedural Database (PDB) di Lumi. Poiché ogni strumento, filtro e script in Lumi deve essere registrato nel PDB per essere richiamabile, questo browser è effettivamente un esploratore PDB completo.

## Apertura del browser delle procedure

Andare su **Aiuto → Programmazione → Browser delle procedure**.

Puoi anche accedervi dalla Scheme Console tramite **Sfoglia**.

## Cosa mostra

Il Browser delle procedure può elencare tutte le procedure attualmente registrate nel PDB, indipendentemente dalla loro origine. L'impostazione predefinita è la ricerca di "interno", per mostrare le procedure principali registrate internamente.

- **Procedure interne**: funzioni principali per la manipolazione delle immagini, la gestione dei livelli e il controllo degli strumenti.
- **Plugin esterni**: procedure fornite dai plug-in C/C++ compilati o dalle estensioni persistenti.

## Ricerca e filtraggio

- **Casella di ricerca**: filtra le procedure per nome, descrizione o autore. Cancellando il campo di ricerca vengono visualizzate tutte le procedure disponibili.
- **Tipo di ricerca**: il menu a discesa di ricerca ti consente di filtrare in base a campi specifici. Se lo imposti su **per tipo** e cerchi "interno", l'elenco si restringerà per mostrare solo le procedure principali registrate internamente.
- **Visualizzazione dettagliata**: facendo clic su una procedura vengono visualizzati i suoi parametri, i valori restituiti, l'autore, la data e una descrizione di ciò che fa.

Questo è essenziale per trovare il nome esatto e la firma dell'argomento di una funzione che desideri chiamare dal tuo script.