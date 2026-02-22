---
title: "Mappa delle tavolozze"
type: docs
---
La Mappa della Tavolozza risponde a una domanda pratica per i pittori: dato un insieme di pigmenti, quali colori possono effettivamente essere miscelati da essi? Partendo dai pigmenti di input della tavolozza, esplora proceduralmente ogni combinazione (miscele di due pigmenti, miscele a tre vie, variazioni tonali) e mappa i risultati su una ruota di colori. Il risultato è un'immagine dello spazio colore raggiungibile per quello specifico set di pigmenti.

La Mappa è anche uno strumento di navigazione basato sulle coordinate. Organizza ogni mix generato per tonalità e luminosità in una griglia circolare, quindi l'intera tavolozza è leggibile a colpo d'occhio e ogni colore ha un indirizzo di casa stabile.

## Struttura della griglia

La Mappa è divisa in una griglia 36×15:

- **36 settori tonalità**: passi di 10° attorno alla ruota, centrati sui nomi delle tonalità principali.
- **15 celle di luminosità**: 3 celle per banda di valori × 5 bande (High Key, Upper Mid, Middle, Lower Mid, Deep), che vanno dal bianco all'esterno al nero al centro.

Ogni cella è un piccolo cuneo sulla ruota. Si dice che una voce inserita in una cella abbia quella cella come **origine**: il suo indirizzo di casa logico sulla mappa.

## Colori nelle celle

Quando più colori competono per la stessa cella, solo un **vincitore** viene visualizzato in evidenza:

1. Le voci **primarie** vincono sempre nella loro cella, indipendentemente dagli altri occupanti.
2. Se non è presente alcun Primario, vince il mix generato (Secondario o Terziario) con il **chroma più alto**.

Le candidature che non vincono si classificano seconde e rimangono accessibili tramite il clic ciclico (vedi sotto).

Le voci personalizzate (mix salvati) vengono visualizzate come punti quadrati; i mix e i primari generati vengono visualizzati come punti rotondi.

## Fai clic su Ciclismo

Facendo clic su una cella occupata si seleziona il vincitore come colore di primo piano. Facendo nuovamente clic sulla stessa cella si passa all'occupante successivo (mix generati dal secondo classificato, quindi eventuali voci personalizzate salvate in quell'indirizzo di griglia). Ogni clic avanza di un passo nello stack.

Il **clic sinistro** porta in primo piano. Quando il colore target è impostato sullo sfondo (dalla casella degli strumenti), i clic vengono invece indirizzati allo sfondo.

## Maiusc-Seleziona: caricamento degli endpoint del mixer

Tieni premuto **Shift** per accedere alla modalità di caricamento dell'endpoint:

- **Clic sinistro** assegna la voce su cui si è fatto clic come **Genitore A (CCW)** nel Mixer tavolozza.
- **Click destro** lo assegna come **Genitore B (CW)**.

In questa modalità sono selezionabili solo le voci di Classe A (miscele primarie e personalizzate con provenienza intatta). I terziari sono nascosti e i punti non di Classe A sono oscurati. Una breve sovrapposizione conferma che la modalità è attiva.

## Punti salienti del genitore mixer

Quando il Mixer tavolozza ha endpoint Genitore A e Genitore B attivi, entrambi sono contrassegnati sulla mappa con **anelli di diamante** (una forma di diamante con un bordo nero). Queste evidenziazioni rimangono visibili anche quando vengono attivati/disattivati ​​altri elementi di visualizzazione, quindi gli elementi principali della fusione attiva sono sempre identificabili.

## Origine e posizione visiva

Ogni voce ha due posizioni sulla mappa:

- **Origine (cella sorgente)**: l'indirizzo della griglia logica a cui appartiene la voce, fissato per tutta la sua vita.
- **Posizione visiva del punto**: dove il colore viene effettivamente visualizzato in base alla tonalità e alla luminosità percettive.

Con **Best-Match Relocation**, quando una miscela viene salvata, il sistema calcola la ricetta ottimale per il colore finale e imposta l'origine in modo che corrisponda alla posizione visiva del colore. Ciò mantiene i colori salvati vicini alla loro posizione visiva sulla ruota e rende la mappa coerente dal punto di vista spaziale.

## Trascinando i mix salvati

Le voci personalizzate (mix salvati) possono essere riposizionate trascinando:1. Fare clic e tenere premuta una voce personalizzata (punto quadrato) e trascinare oltre la soglia di 5 pixel.
2. Il cursore cambia per indicare la modalità di trascinamento. Le evidenziazioni dei genitori si aggiornano in tempo reale mentre ti sposti sulla mappa per mostrare i nuovi genitori della fusione in ciascuna posizione del candidato.
3. Il punto trascinato si aggancia alla posizione del campione valido più vicina.
4. Rilascia per impegnare. La voce adotta la ricetta della cella di destinazione: i suoi genitori, la fusione, il tono e la crominanza vengono aggiornati per corrispondere e la sua origine viene aggiornata per corrispondere alla nuova posizione visiva.

Le mosse di trascinamento possono essere annullate tramite **Modifica → Annulla**.

## Doppio clic: commutazione dell'area di lavoro della mappa

Nell'**Editor tavolozza**, facendo doppio clic su qualsiasi voce della tavolozza si attiva e disattiva la visualizzazione dell'area di lavoro Mappa tavolozza. Questo è un modo rapido per passare dalla navigazione dei colori salvati alla miscelazione sulla mappa senza utilizzare un menu. Il comportamento del clic singolo (ripristino della ricetta della voce nel Mixer) non viene influenzato.

## Sovrapposizione tela

La Mappa tavolozza può essere richiamata direttamente sulla tela dell'immagine come sovrapposizione a schermo intero facendo clic sul **campione Primo piano/Sfondo** nella casella degli strumenti. Ciò fornisce un'ampia superficie di miscelazione senza dedicare un pannello permanente alla Mappa.

## Campione colore centrale

Un campione circolare si trova al centro del foro della ciambella e riflette il colore di qualunque cella su cui si trova il cursore:

- **Colore al passaggio del mouse**: quando il cursore si posiziona su una voce della mappa, il campione si aggiorna immediatamente per mostrare il colore di quella voce.
- **Colore selezionato come fallback**: quando non si passa il mouse su alcuna cella, il campione mostra il risultato calcolato dal Mixer tavolozza per la voce attualmente selezionata. Se il mixer non ha ancora risolto il problema, utilizza il colore di visualizzazione di base della voce in modo che lo spot non diventi mai vuoto.
- Un sottile bordo scuro delinea sempre il campione.
- Dopo che il cursore si è fermato brevemente sul campione centrale, appare un anello esterno bianco e nero per segnalare che l'area è interattiva.
- **Facendo clic sul campione centrale** si chiude la sovrapposizione della tela, tornando alla normale visualizzazione dell'immagine (come fare clic all'esterno dell'anello esterno).

## Tasto Alt: modalità di confronto tela

Quando la sovrapposizione della tela Mappa tavolozza è aperta, tenendo premuto **Alt** viene temporaneamente visualizzata l'immagine sottostante:

- L'intera interfaccia utente della mappa della tavolozza diventa invisibile (la sua opacità scende a zero), scoprendo la tela.
- Un campione circolare da 64 pixel segue il cursore, riempito con il colore campionato corrente del Mixer tavolozza, in modo da rimanere consapevoli del mix attivo mentre si ispeziona l'immagine.
- Rilasciando Alt si ripristina la mappa della tavolozza alla massima opacità.

Un'etichetta di suggerimento, *"Tieni premuto il tasto Alt per vedere l'immagine"*, viene mostrata all'interno della vista dell'area di lavoro come promemoria.