---
title: "Livelli"
type: docs
url: "hub/features/layers"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 6dbb2dc10c71714d1aed2036ac55c90a05b2e1fb1738e447049234b2577e3edb
---

Il sistema a livelli di Lumi dà struttura a un'illustrazione. Consente agli artisti di separare schizzi, colore, ombreggiatura, texture, maschere, regolazioni, esperimenti e dettagli finali senza consolidare ogni decisione in un'unica immagine piatta.

I livelli non sono solo una pila di pixel. Portano visibilità, fusione, maschere, blocchi, effetti, raggruppamento e comportamento di composizione, e costituiscono la base dei flussi di lavoro di pittura flessibili e non distruttivi.

![layers](/images/screens/layers.jpg)

## Pittura strutturata

Un'immagine a livelli può essere costruita per fasi. I segni grezzi possono stare sotto linee pulite, il colore può essere bloccato separatamente dall'illuminazione, la texture può essere isolata e idee alternative possono restare disponibili senza disturbare la composizione principale.

I gruppi rendono leggibile quella struttura. Parti correlate di un'illustrazione possono spostarsi insieme, fondersi insieme o essere trattate come parte condivisa dell'opera, mentre i singoli livelli restano modificabili.

## Fusione e maschere

La fusione dei livelli controlla come una parte dell'opera interagisce con ciò che sta sotto. È possibile ombreggiare, schiarire, tingere, texturizzare o correggere il colore senza ridipingere le forme sottostanti.

Le maschere aggiungono un ulteriore livello di controllo. Consentono di dipingere, ammorbidire, nascondere, ripristinare o modellare la visibilità indipendentemente dal contenuto colore del livello. Le decisioni sui bordi e le transizioni tonali restano flessibili per tutta la vita dell'opera.

## Selezione e navigazione

I dipinti complessi possono contenere molti elementi piccoli. Lumi supporta la navigazione diretta orientata ai livelli, così gli artisti possono passare dalla tela alla pila di livelli senza perdere il ritmo della pittura.

L'obiettivo è far sentire il lavoro a livelli come qualcosa di spaziale, non amministrativo: se un segno è visibile sulla tela, il sistema dei livelli dovrebbe aiutare l'artista a tornarci rapidamente.

## Protezione e intenzione

I livelli possono essere protetti in diversi modi, così lavoro finito, maschere, posizioni, trasparenza o decisioni cromatiche non vengono modificati per errore. Queste protezioni sono utili quando un'immagine diventa densa e alcune parti devono restare stabili mentre altre continuano a evolversi.

La protezione supporta flussi di lavoro deliberati: disegna liberamente dove il cambiamento è benvenuto, blocca le aree risolte e continua a sviluppare l'immagine senza timore di danni accidentali.

## Colore di pittura bloccato

Un livello riempito con un unico colore uniforme può essere bloccato a un colore della tavolozza. Selezionando il livello si seleziona il colore collegato nella tavolozza, e cambiare quel colore aggiorna subito il livello.

Dipingere su un livello con un colore di pittura bloccato usa sempre il colore collegato della tavolozza. Si crea così un collegamento vivo tra tavolozza e livello, così i colori si possono regolare dinamicamente mentre la tavolozza è ancora in fase di progettazione.

## Effetti non distruttivi

Filtri ed effetti possono far parte dello stato modificabile di un livello invece di diventare subito pixel permanenti. Le modifiche visive restano regolabili e uno stack di effetti può rimanere parte della composizione operativa.

Per gli artisti significa che la sperimentazione può restare reversibile. Una resa visiva può essere testata, nascosta, riordinata, perfezionata o infine consolidata quando diventa parte dell'immagine finale.

## Prestazioni per file complessi

Le illustrazioni a livelli possono diventare complesse, soprattutto quando gruppi, maschere ed effetti interagiscono. Il sistema a livelli di Lumi è progettato per mantenere reattive le azioni di pittura più comuni, evitando ricomposizioni non necessarie quando possibile.

La finestra dell'immagine può disegnare quella pila composta con OpenGL. Spostamento, zoom e rotazione restano fluidi, perché la vista muove un'immagine già pronta per lo schermo, e un tratto può comparire al suo posto tra i livelli che lo circondano. [OpenGL](/hub/features/opengl/) descrive la navigazione sulla tela e i tratti del pennello per ogni preimpostazione.

Il risultato è un flusso di lavoro a livelli orientato sia al controllo sia alla velocità: abbastanza dettagliato per un lavoro attento e non distruttivo, ma comunque pratico per la pittura quotidiana.
