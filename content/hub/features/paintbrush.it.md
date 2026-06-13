---
title: "Strumento Pennello"
type: docs
---
Lo strumento Pennello è lo strumento di pittura principale di Lumi: un modo reattivo ed espressivo per disegnare, dipingere, sfumare, strutturare e creare segni direttamente sulla tela. È progettato per essere immediato, pur offrendo agli artisti la possibilità di modellare il comportamento del tratto.

Anziché essere un unico pennello fisso, agisce come un sistema di verniciatura. La forma, la consistenza, il movimento, la pressione, il tempismo e il colore del pennello possono tutti contribuire al segno finale, rendendolo adatto per lavori con linee pulite, pittura morbida, effetti multimediali asciutti, tratti calligrafici, trame sparse e formazioni di pennelli a più teste.

![brush-tool](/images/screens/brush-tool.jpg)

## Segni di pennello espressivi

I pennelli possono essere basati su timbri bitmap, forme procedurali o origini animate basate su fotogrammi. Ciò consente al tratto di spaziare da un semplice segno rotondo e morbido a una testina riccamente strutturata o in evoluzione. Lo stesso motore di pittura può supportare disegni precisi, accumuli pittorici, segni decorativi e interruzioni in stile media naturale.

Quando un pennello diventa visivamente complesso, l'anteprima può rimanere semplificata in modo che il disegno rimanga reattivo e facile da leggere.

![tool-setup](/images/screens/tool-setup.jpg)


## Dinamica e risposta in ingresso

Lo strumento Pennello risponde a input in tempo reale come pressione dello stilo, velocità, direzione, inclinazione e altri valori del controller. Questi segnali possono influenzare il tratto visibile in molti modi: spessore, opacità, angolo, risposta della trama, comportamento del colore, spaziatura e altre qualità possono cambiare mentre la mano si muove.

Ciò fa sì che il Pennello sembri meno un motivo stampato e più uno strumento di disegno fisico. Un tocco leggero può produrre segni delicati, un movimento più veloce può aprire texture o forme e un comportamento sensibile alla direzione può aiutare i tratti a seguire il gesto della mano.

![dynamics](/images/screens/dynamics.jpg)

## Comportamento della corsa

I colpi possono essere diretti e immediati oppure possono essere assistiti da livellamento e stabilizzazione. Queste funzionalità aiutano a ridurre il jitter indesiderato, ad ammorbidire i cambiamenti bruschi e a rendere i movimenti più lunghi più controllati senza rimuovere il carattere dell'input dell'artista.

Il pennello supporta anche diversi approcci all'accumulo di vernice. Può comportarsi come un tratto continuo, accumulare tocchi ripetuti o emettere segni nel tempo mentre il puntatore è tenuto in posizione. Questa flessibilità lo rende utile sia per il lavoro di linea deliberato che per la costruzione tonale più lenta.

Per i segni calligrafici o simili all'inchiostro, il Pennello può generare un tratto dalla forma più continua invece di fare affidamento solo su timbri ripetuti. Ciò produce forme fluide, simili a nastri, che rispondono naturalmente al gesto e alla velocità.

![stroke](/images/screens/stroke.jpg)

## Acquisizione di tratti e rendering simulato

Il pennello può catturare un piccolo esempio di come un'impostazione predefinita viene normalmente disegnata a mano, quindi utilizzare quel profilo durante il rendering di tratti definiti dalla geometria anziché dal movimento dal vivo. Le linee diritte premendo Maiusc, i tracciati e le selezioni tracciate possono tutti utilizzare il modello di pressione e velocità catturato dalla preimpostazione dello strumento attivo invece di comportarsi come una linea meccanica piatta.

Ciò mantiene i tratti costruiti più vicini al carattere del pennello. Una linea tracciata da un tracciato può iniziare dolcemente, aumentare la pressione, assottigliarsi o variare la risposta della velocità allo stesso modo del tratto della mano campionato, pur seguendo la forma esatta del tracciato, del bordo di selezione o del gesto in linea retta.

## Elaborazione successivaIl pennello può registrare un tratto mentre lo disegni, quindi riprodurre il gesto catturato una volta sollevato, perfezionando il percorso prima che venga tracciato il segno finale. Puoi disegnare liberamente e ottenere comunque una direzione più pulita, angoli più netti o una struttura più deliberata senza dover disegnare con precisione meccanica.

Questo apre tratteggi e segni di costruzione rigati che si agganciano ad angoli puliti mantenendo la lunghezza e il carattere disegnati a mano, tratti del nastro stabili all'inclinazione e riproduzione sensibile agli angoli che tratta curve e tratti rettilinei in modo diverso. I pennelli a più teste possono condividere un percorso corretto mentre ciascuna testa mantiene la propria variazione e le dinamiche possono comunque modellare il tratto lungo la sua curva finale durante la riproduzione. La post-elaborazione si applica ai tratti disegnati piuttosto che all'emissione continua dell'aerografo.

## Colore e consistenza

I tratti del pennello possono utilizzare il colore di pittura attivo, rispondere alle sfumature o variare il colore attraverso la dinamica. La gestione delle texture consente al pennello di passare dalla copertura solida ai segni spezzati e radenti alla superficie, utile per effetti di pennello asciutto, grana e ombreggiature espressive.

Poiché colore e texture possono far parte dello stesso sistema dinamico di forma e opacità, un singolo tratto può evolversi mentre si sposta sulla tela invece di rimanere visivamente uniforme.

## Teste e formazioni di spazzole

Lo strumento Pennello può dipingere con più di una testa alla volta. È possibile disporre più testine attorno al percorso del tratto per creare segni di pennino, tratti a ventaglio, comportamento simile a quello delle setole, modelli di spruzzo, formazioni strutturate o tratteggi strutturati.

Queste teste possono seguire la direzione del viaggio, variare l'una dall'altra e disperdersi in modi che rendono il tratto organico piuttosto che ripetuto meccanicamente. Ciò è particolarmente utile per i pennelli naturali, i tratti decorativi, il fogliame, la pelliccia, i tratteggi e altri segni che traggono vantaggio dall'irregolarità controllata.

![brush-heads](/images/screens/brush-heads.jpg)

## Carico spazzole e prelievo vernice

Il Pennello può anche simulare la quantità di vernice o materiale attualmente trasportata sul pennello. Man mano che un tratto continua, il carico può gradualmente ridursi, lasciando che i segni diventino più leggeri, più asciutti, più sottili, più ruvidi o comunque più spezzati a seconda di come sono impostate le dinamiche del pennello.

Il carico può essere reintrodotto tra una passata e l'altra, mantenuto al livello prescelto o utilizzato come segnale di controllo in tempo reale per altri comportamenti della spazzola. Ciò rende possibile costruire pennelli che sembrano più dei veri e propri media: bagnati all'inizio di una pennellata, progressivamente esausti attraverso la distanza e poi immersi nuovamente per la passata successiva.

![material-state](/images/screens/material-state.jpg)

## Contatto con la superficie della spazzola

Il pennello può anche simulare la perdita intermittente di contatto con la superficie del dipinto: i segni rotti che compaiono quando una matita, un bastoncino di carbone, un pennello asciutto o un pennarello parzialmente esaurito si impegnano solo parzialmente con la carta.

Quando la simulazione del contatto è abilitata, la spazzola è in contatto o sollevata. Durante il contatto, i segni si depositano normalmente. Durante il sollevamento non si deposita materiale e la corsa lascia uno spazio la cui lunghezza viene scelta casualmente tra una distanza minima e massima. La transizione è binaria: l'effetto non cambia opacità, dimensione, durezza, spaziatura o flusso, ma solo se la vernice è stesa.La facilità con cui si perde il contatto dipende dalla soglia di contatto, dalla pressione dello stilo e, facoltativamente, dal carico della spazzola. Valori di soglia più alti rendono le pause più frequenti. La pressione agisce come una forza stabilizzante: una pressione leggera aumenta la possibilità di perdere il contatto, mentre una pressione decisa rende più probabile che la corsa rimanga bassa. Quando il carico della spazzola è abilitato, un carico basso può rendere il segno più rotto, mentre un carico elevato può aiutare a mantenere il contatto, in modo simile a uno strumento che trasporta ancora materiale sufficiente per far presa sulla superficie.

La perdita viene valutata dalla distanza del tratto percorso piuttosto che dal conteggio dei tocchi, quindi i pennelli con spaziatura densa o sparsa si comportano in modo coerente. La funzione funziona sia con il rendering basato su timbri che con quello calligrafico, producendo spazi coerenti lungo il tratto anziché tocchi saltati isolati.

## Animazione e variazione

Le sorgenti dei pennelli animati possono cambiare cornice man mano che il tratto avanza, conferendo ai pennelli un senso di movimento e varietà. La randomizzazione e la variazione per tratto possono evitare che i segni ripetuti sembrino identici, mentre la semina stabile può preservare un carattere coerente quando è necessaria la ripetibilità.

Questi comportamenti sono utili per i pennelli che dovrebbero sembrare vivi: le setole si spostano durante un tratto, i timbri con texture cambiano leggermente nel tempo o gli strumenti multi-testa in cui ogni testa ha la propria personalità.

## Flusso di lavoro incentrato sull'artista

Lo strumento Pennello è organizzato in modo che le decisioni più comuni sulla pittura restino a portata di mano, mentre le scelte di configurazione meno frequenti rimangano fuori mano. L'intento è quello di mantenere lo strumento accessibile durante la pittura pur supportando una profonda personalizzazione per la progettazione del pennello.

Nel complesso, il pennello è progettato per coprire sia la pittura di tutti i giorni che la creazione di segni specializzati: schizzi rapidi, illustrazioni raffinate, rendering strutturato, lavori espressivi con inchiostro ed effetti procedurali complessi del pennello condividono tutti la stessa base flessibile.