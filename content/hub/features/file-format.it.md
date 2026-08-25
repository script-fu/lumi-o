---
title: "Formato file (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

Il formato nativo `.lum` di Lumi è una cartella di progetto, non un unico file chiuso. È pensato per l'illustrazione a livelli: alberi di livelli profondi, tele grandi, maschere, effetti non distruttivi e checkpoint che non devono duplicare l'intero dipinto.

Il compito del formato è mantenere intatta quella struttura di lavoro: così un progetto si riapre fedelmente, si può ispezionare quando qualcosa va storto e si recupera da un checkpoint recente, senza trattare l'opera come un blocco opaco.

## Pezzi separati, di proposito

Un progetto `.lum` è una cartella. L'albero dei livelli e le proprietà dell'immagine stanno in XML leggibile. Ogni livello e ogni maschera conserva il proprio buffer di pixel, nominato secondo l'opera e non secondo un ID interno. I tracciati vettoriali sono memorizzati come SVG ordinario. Le impostazioni dei filtri pesanti occupano file propri, accanto all'immagine. I profili ICC sono memorizzati una sola volta nella radice del progetto, così le istantanee di recupero possono farvi riferimento invece di copiarli.

È questa separazione a rendere possibile il resto del formato. I livelli invariati possono restare intatti su disco. Un buffer danneggiato fallisce da solo, invece di trascinare con sé l'intero file. I pixel di livello mancanti diventano livelli vuoti che hanno ancora nome, posizione e impostazioni di fusione; un'anteprima di gruppo mancante si ricostruisce dai figli. Il progetto resta una mappa di come è stato costruito il dipinto.

Le tavolozze di pigmenti appartengono agli strumenti colore di Lumi. Un progetto può ricordare quale tavolozza era associata all'immagine, ma la libreria delle tavolozze è fuori dal `.lum`.

## Stato modificabile, non un appiattimento

Il file conserva il dipinto in lavorazione. I livelli restano livelli, i gruppi restano gruppi, le maschere restano maschere, compresi spostamenti, blocchi, comportamento di fusione e pile di filtri. I filtri non distruttivi si salvano come operazioni e parametri, non come pixel già applicati. Un livello di una sola tinta unita non ha bisogno di un file di pixel.

I gruppi compressi conservano anche una vista composta di se stessi. È quell'anteprima salvata a comparire sulla tela quando un gruppo è chiuso, così i figli non devono essere ricostruiti solo per guardare l'immagine. Le modalità di ispezione solo per la visualizzazione restano fuori da quella cache: mostrare una maschera o l'alfa per la modifica si ripristina come metadati, non viene inciso nel gruppo salvato.

## I file grandi possono restare in parte su disco

Aprire un `.lum` non implica caricare ogni pixel. Il contenuto dei gruppi compressi può restare su disco mentre l'anteprima salvata del gruppo viene mostrata subito. Quando si espande un gruppo, quei livelli, maschere e gruppi nidificati arrivano in memoria. I gruppi che restano chiusi restano leggeri.

Il file registra anche quali gruppi erano davvero in uso. I gruppi sul percorso della selezione attiva possono riaprirsi espansi; le altre cartelle sono memorizzate come compresse anche se nella sessione precedente erano aperte. Così un file profondo non deve caricare in memoria ogni ramo inutilizzato nel momento in cui si apre.

Raggruppare è quindi una scelta di prestazioni oltre che di organizzazione. Grandi sfondi, esperimenti archiviati e varianti inutilizzate possono stare in gruppi chiusi senza occupare la stessa memoria dei livelli su cui si dipinge. Il salvataggio segue la stessa regola: i buffer ancora nascosti vengono copiati o saltati come file, non riportati in memoria solo per essere scritti di nuovo.

## Checkpoint che scrivono solo ciò che è cambiato

File → Salva aggiorna il progetto di lavoro. I salvataggi incrementali e il salvataggio automatico scrivono in un albero di recupero, e scrivono solo i dati modificati — i buffer di livello cambiati, non una seconda copia dell'intera immagine. Ogni checkpoint porta comunque una descrizione completa dell'albero dei livelli, così qualsiasi punto di quella traccia può essere aperto riempiendo i pixel invariati da checkpoint più vecchi e, se serve, dal file di lavoro stesso.

Il salvataggio automatico usa lo stesso schema in una cache separata, così la protezione automatica non deve riscrivere il file su disco. Se si apre un progetto quando esistono checkpoint più recenti dell'ultimo salvataggio completo, Lumi può proporli invece di scartare in silenzio il lavoro più recente. Le immagini recuperate si aprono con un nome distinto, così un salvataggio rapido non può sovrascrivere l'originale.

## Un formato di lavoro

`.lum` serve a continuare un dipinto in Lumi. I formati appiattiti o di compatibilità servono alla pubblicazione, alla consegna e ad altre applicazioni. Poiché un progetto è una cartella di molti file, va archiviato se deve viaggiare.

Il file di lavoro resta ricco e modificabile. Le esportazioni sono il modo in cui un'immagine finita o condivisa lascia quella struttura.
