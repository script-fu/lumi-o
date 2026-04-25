---
title: "Formato file (.lum)"
type: docs
---
Il formato file nativo di Lumi è progettato per progetti di pittura a più livelli che devono rimanere affidabili, ispezionabili e recuperabili nel tempo. È progettato in base alla realtà del lavoro di illustrazione: molti livelli, tele di grandi dimensioni, informazioni sui colori incorporate, maschere, effetti e dati di recupero.

Invece di trattare un progetto come un unico blob opaco, il formato mantiene visibile all'applicazione la struttura dell'opera d'arte. Ciò consente a Lumi di salvare, caricare e recuperare immagini di grandi dimensioni in modo più intelligente, preservando l'organizzazione da cui dipendono gli artisti.

## Apri la struttura del progetto

Un progetto Lumi mantiene separate le parti dell'opera d'arte: struttura dell'immagine, contenuto del livello, maschere, dati di colore, metadati e informazioni di ripristino hanno ciascuno un ruolo chiaro. Ciò rende il formato più facile da ragionare e più adatto all’accesso a lungo termine rispetto a un contenitore chiuso e monolitico.

L'obiettivo non è solo memorizzare i pixel, ma memorizzare lo stato di funzionamento di un'illustrazione. I livelli rimangono livelli, le maschere rimangono maschere e il file continua a riflettere il modo in cui è stata creata la grafica.

## Progettato per quadri di grandi dimensioni

Le immagini a più livelli di grandi dimensioni possono diventare rapidamente pesanti. Il formato di Lumi supporta flussi di lavoro in cui non è necessario inserire in memoria tutti i dati dell'immagine contemporaneamente. I progetti possono rimanere reattivi caricando le parti dell'immagine effettivamente necessarie per la visualizzazione, la modifica, la composizione o l'esportazione.

Questo approccio aiuta i file complessi a risultare gestibili, soprattutto quando un disegno contiene molti livelli nascosti, archiviati, sperimentali o raggruppati.

## Risparmiare senza interrompere il flusso

Il formato file supporta sia il normale salvataggio del progetto che gli snapshot leggeri in stile ripristino. Ciò offre agli artisti la possibilità di proteggere frequentemente il proprio lavoro senza trasformare ogni punto di controllo in un duplicato completo dell'intera immagine.

Poiché le informazioni di ripristino appartengono alla struttura del progetto, Lumi può conservare la cronologia utile vicino all'opera d'arte consentendo comunque ai salvataggi di sicurezza automatici di vivere separatamente dal file di lavoro.

## Interscambio ed esportazione

Il formato nativo è destinato al lavoro Lumi in corso, mentre i formati di esportazione vengono utilizzati per condividere risultati appiattiti o incentrati sulla compatibilità. Il supporto all'importazione aiuta a portare le opere d'arte esistenti nell'ambiente a più livelli di Lumi, mentre il supporto all'esportazione consente ai pezzi finiti di lasciare il formato del progetto quando sono pronti per la pubblicazione, la consegna o un'ulteriore elaborazione.

La distinzione mantiene il file di lavoro ricco e modificabile consentendo al tempo stesso di produrre immagini finali in formati esterni comuni.

## Affidabilità a lungo termine

In breve, il formato `.lum` è un contenitore pratico per lavori di pittura seri: sufficientemente aperto per l'ispezione, sufficientemente strutturato per il recupero e sufficientemente flessibile per gestire in modo economico immagini complesse a strati.