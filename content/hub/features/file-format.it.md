---
title: "Formato file (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

Il formato nativo di Lumi-o è pensato per progetti di pittura a livelli che devono restare affidabili, ispezionabili e recuperabili nel tempo. È progettato intorno alla realtà del lavoro di illustrazione: molti livelli, tele di grandi dimensioni, informazioni colore incorporate, maschere, effetti e dati di recupero.

Invece di trattare un progetto come un unico blocco opaco, il formato mantiene visibile all'applicazione la struttura dell'opera. Così Lumi-o può salvare, caricare e recuperare immagini grandi in modo più intelligente, preservando l'organizzazione da cui dipendono gli artisti.

## Struttura aperta del progetto

Un progetto Lumi-o mantiene separate le parti dell'opera: struttura dell'immagine, contenuto dei livelli, maschere, dati colore, metadati e informazioni di recupero hanno ciascuno un ruolo chiaro. Il formato risulta più comprensibile e più adatto all'accesso a lungo termine rispetto a un contenitore chiuso e monolitico.

L'obiettivo non è solo memorizzare i pixel, ma conservare lo stato operativo di un'illustrazione. I livelli restano livelli, le maschere restano maschere e il file continua a riflettere il modo in cui l'opera è stata costruita.

## Progettato per opere di grandi dimensioni

Le immagini a livelli di grandi dimensioni possono diventare pesanti rapidamente. Il formato di Lumi-o supporta flussi di lavoro in cui non è necessario caricare in memoria tutti i dati dell'immagine contemporaneamente. I progetti possono restare reattivi caricando solo le parti necessarie per visualizzazione, modifica, composizione o esportazione.

Questo approccio rende gestibili i file complessi, soprattutto quando un'opera contiene molti livelli nascosti, archiviati, sperimentali o raggruppati.

## Salvare senza interrompere il flusso

Il formato supporta sia il salvataggio normale del progetto sia istantanee leggere in stile recupero. Gli artisti possono proteggere frequentemente il proprio lavoro senza trasformare ogni checkpoint in un duplicato completo dell'intera immagine.

Poiché le informazioni di recupero appartengono alla struttura del progetto, Lumi-o può conservare una cronologia utile vicino all'opera consentendo comunque ai salvataggi di sicurezza automatici di restare separati dal file di lavoro.

## Interscambio ed esportazione

Il formato nativo è destinato al lavoro continuo in Lumi-o, mentre i formati di esportazione servono per condividere risultati appiattiti o orientati alla compatibilità. Il supporto all'importazione aiuta a portare opere esistenti nell'ambiente a livelli di Lumi-o, mentre il supporto all'esportazione consente ai pezzi finiti di uscire dal formato di progetto quando sono pronti per pubblicazione, consegna o ulteriore elaborazione.

La distinzione mantiene il file di lavoro ricco e modificabile, consentendo al tempo stesso di produrre immagini finali in formati esterni comuni.

## Affidabilità a lungo termine

In breve, il formato `.lum` è un contenitore pratico per lavori di pittura seri: abbastanza aperto per l'ispezione, abbastanza strutturato per il recupero e abbastanza flessibile per gestire in modo efficiente immagini complesse a livelli.
