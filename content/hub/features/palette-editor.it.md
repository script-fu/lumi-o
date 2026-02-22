---
title: "Editor della tavolozza"
type: docs
---
L'Editor tavolozza è il luogo in cui costruisci e gestisci una tavolozza Lumi. Contiene il tuo set di pigmenti, memorizza le miscele salvate dal Mixer tavolozza, registra i colori effettivamente utilizzati durante la pittura e ti consente di configurare la struttura dei valori e le sfumature per la tavolozza.

## Selezione di una tavolozza

Una tavolozza è più di una raccolta di pigmenti: è un impegno stilistico. Molti artisti lavorano con un insieme ristretto e fisso di pigmenti che conoscono intimamente: il modo in cui si mescolano, i neutri che producono, la temperatura cambia tra loro. Quella familiarità diventa parte della loro voce visiva. Un pittore potrebbe mantenere una tavolozza calda e a basso croma per le figure e una tavolozza separata per i paesaggi, oppure potrebbe eseguire tutto il suo lavoro all'interno di un unico set di quattro pigmenti come vincolo deliberato che unifica un corpo di lavoro.

Lumi supporta questo modo di lavorare. Ogni tavolozza ha i propri pigmenti, miscele, struttura dei valori e gradienti. Cambiando tavolozza si cambia l'intero sistema di colori: la Mappa, il Mixer e le miscele disponibili si aggiornano tutti per riflettere il nuovo set.

Un menu a discesa nella parte superiore dell'editor della tavolozza seleziona la tavolozza attiva. Lumi viene fornito con tre tavolozze nel gruppo **Standard**:

| Tavolozza | Carattere |
| :--- | :--- |
| **Predefinito** | Una tavolozza versatile e calda che copre l'intera ruota dei colori. Buon punto di partenza per la maggior parte delle materie. |
| **Maestro** | Un'ampia tavolozza a spettro completo per i pittori che desiderano la massima copertura delle tonalità e il controllo esplicito sugli assi di grigio. |
| **Zorn** | Una tavolozza limitata a quattro pigmenti basata sull'approccio di Anders Zorn. Copre una gamma sorprendentemente ampia di tonalità calde dell'incarnato e neutri a bassa crominanza con un set di pigmenti minimo. |

Le tavolozze possono anche essere create, importate o duplicate dalla scheda Tavolozze.

## Pigmenti della tavolozza

La sezione **Pigmenti tavolozza** nella parte superiore della visualizzazione della tavolozza elenca le voci principali: i pigmenti di base da cui è costruito il resto della tavolozza. Questi sono gli ingressi al sistema di miscelazione spettrale. I secondari e i terziari vengono generati automaticamente da essi e vengono utilizzati per popolare la mappa della tavolozza

## Mix salvati

La sezione **Mix salvati** contiene i colori che hai esplicitamente mantenuto dal Mixer tavolozza utilizzando **Aggiungi alla tavolozza**. Questi sono i colori derivati: i risultati della miscelazione spettrale, delle regolazioni del tono e della crominanza salvati per il riutilizzo.

I Mix salvati sono suddivisi in cinque fasce di valori:

| Banda | Intervallo luminosità predefinito |
| :--- | :--- |
| Chiave alta | 80 – 100%|
| Medio superiore | 60 – 80%|
| Medio | 40 – 60%|
| Medio inferiore | 20 – 40%|
| Profondo | 0 – 20%|

Lumi posiziona automaticamente ciascun mix salvato nella banda appropriata in base alla sua luminosità percettiva (CIE L\*). Questo organizza i tuoi mix in base al valore anziché cercare in un elenco piatto e in genere corrisponde al modo in cui un artista pensa al colore.

I mix salvati possono essere rinominati tramite il pulsante **Rinomina personalizzato** o il menu contestuale.

## Miscele Usate

La sezione **Miscele usate** è una cronologia attivata dalla vernice. Ogni volta che un colore della tavolozza viene applicato alla tela, viene registrato qui. I mix usati sono ordinati dal più al meno recente.

Questa sezione è utile per recuperare un colore con cui hai dipinto ma non hai salvato esplicitamente. Per conservare permanentemente un Mix usato, selezionalo e fai clic su **Promuovi** per spostarlo nei Mix salvati nella fascia di valore appropriata.

I mix utilizzati vengono archiviati per tavolozza e persistono tra le sessioni.

## Bande di valoreLe fasce valore definiscono dove si trovano i confini tra le cinque zone di luminosità. Per impostazione predefinita, dividono la luminosità in modo uniforme nell'intervallo 0-100%, ma puoi regolarli per adattarli alla struttura tonale del soggetto. È utile per i pittori definire e gestire le fasce di valore _e_ gli spazi tra di loro.

### Il cursore della fascia di valore

L'**espansore Bande valori** nell'editor tavolozza contiene un dispositivo di scorrimento con cinque divisori trascinabili. Trascina qualsiasi divisore per spostare il confine tra le bande adiacenti. L'etichetta sopra il cursore mostra il nome e l'esatto intervallo percentuale della banda attiva.

**Pulsanti:**

| Pulsante | Effetto |
| :--- | :--- |
| **Annulla** | Ripristina il dispositivo di scorrimento all'ultimo stato applicato |
| **Copia** | Copia la configurazione corrente della banda negli appunti |
| **Incolla** | Incolla una configurazione di banda copiata da un'altra tavolozza |
| **Predefiniti** | Ripristina le impostazioni predefinite di fabbrica per la divisione uguale |
| **Applica** | Conferma le modifiche e rigenera la tavolozza |

**Applica** è necessario per rendere le modifiche permanenti. Attiva una rigenerazione completa della tavolozza e rimuoverà tutti i mix salvati la cui luminosità non rientra più in nessuna banda. Lumi mostra una finestra di dialogo di conferma che elenca quanti mix verranno rimossi prima di procedere.

### Bande di valori e mappa della tavolozza

La mappa della tavolozza mostra la tavolozza come una ruota delle tonalità con 36 settori della tonalità (10° ciascuno) e 15 celle di luminosità disposte come anelli concentrici. Ogni fascia corrisponde a tre anelli: le cinque fasce × 3 anelli = 15 celle totali.

La regolazione delle bande di valore sposta i valori di luminosità in ciascun livello dell'anello. Una banda compressa verso l'estremità scura fa sì che i suoi tre anelli coprano una gamma tonale più ristretta; un'ampia fascia conferisce ai suoi tre anelli una maggiore diffusione tonale. Questo è il modo in cui la stessa struttura della Mappa delle Palette si adatta alle tavolozze ottimizzate per diverse priorità tonali.

## Sfumature della tavolozza

Ciascuna tavolozza può memorizzare uno o più **Gradienti**: progressioni uniformi derivate dalle voci della tavolozza che possono essere applicate alla tela come riempimenti sfumati o utilizzate come strisce di riferimento.

I gradienti vengono gestiti nell'**espansore gradienti**. La combinazione in alto elenca i gradienti nella tavolozza corrente. **Aggiungi** crea un nuovo gradiente. **Rimuovi** elimina quello selezionato. **Rinomina** lo rinomina.

### Editor di gradienti

L'**espansore Editor gradiente** configura il gradiente selezionato. Ciascun gradiente ha tre punti finali (**A**, **B** e **C**) visualizzati come campioni di colore. Fare clic su un campione per renderlo il punto finale attivo per la modifica.

Ciascun punto finale può essere impostato facendo clic su **Seleziona** e quindi su una voce della tavolozza nella Mappa tavolozza o nella vista tavolozza. L'endpoint è collegato a quella voce della tavolozza tramite UID; se la voce cambia, il gradiente si aggiorna.

**Controlli per endpoint:**

| Controllo | Effetto |
| :--- | :--- |
| **Forza** | Quanto fortemente il colore dell'endpoint contribuisce rispetto ai suoi vicini |
| **Opacità** | Alfa del colore finale nel gradiente |
| **Curva** | Regolazione gamma per la caduta del colore da questo endpoint |

**Cursori di distribuzione** (S1, S2, S3) impostano il punto in cui cadono i tre punti medi tra i punti finali lungo la striscia del gradiente. Il loro ripristino riporta i punti medi alla stessa spaziatura.

La striscia di anteprima del gradiente nella parte superiore del blocco Editor gradiente mostra il risultato dell'endpoint corrente e delle impostazioni di distribuzione.

## Tavolozza ancorabileLa **Tavolozza** agganciabile (**Pannelli > Tavolozza**) è un pannello focalizzato sulla lettura più semplice per sfogliare e selezionare i colori da qualsiasi tavolozza. Mostra la stessa vista in tre sezioni (Pigmenti tavolozza, Miscele salvate, Miscele usate) senza gli espansori Bande valore e Gradienti.

Un selettore a discesa della tavolozza in alto ti consente di passare da una tavolozza all'altra disponibile. Fare clic su una voce qualsiasi per impostarla come colore di primo piano. Fare doppio clic per aprire l'editor dei nomi dei colori. Per le tavolozze scrivibili, nella barra dei pulsanti sono disponibili le azioni Modifica colore, Nuovo colore da FG ed Elimina colore.

La tavolozza agganciabile è destinata all'accesso rapido al colore durante la pittura quando l'intero editor della tavolozza occuperebbe troppo spazio.

## Scheda Tavolozze

La **Scheda Tavolozze** (disponibile come scheda agganciabile) mostra la tavolozza attiva in modalità compatta. Esclude i pigmenti per concentrarsi sulle miscele salvate