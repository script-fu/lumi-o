---
title: "Strumento Pennello"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
---

Lo strumento Pennello è lo strumento di pittura principale di Lumi-o: un modo reattivo ed espressivo per disegnare, dipingere, sfumare, texturizzare e creare segni direttamente sulla tela. È progettato per essere immediato, pur offrendo agli artisti ampio margine per modellare il comportamento del tratto.

Non è un unico pennello fisso, ma un sistema di pittura. Forma, texture, movimento, pressione, tempismo e colore del pennello possono tutti contribuire al segno finale, rendendolo adatto a linee pulite, pittura morbida, effetti di media asciutta, tratti calligrafici, texture sparse e formazioni a più teste.

![brush-tool](/images/screens/brush-tool.jpg)

## Segni espressivi del pennello

I pennelli possono basarsi su timbri bitmap, forme procedurali o sorgenti animate fotogramma per fotogramma. Un tratto può spaziare da un semplice segno rotondo e morbido a una testa di pennello ricca di texture o in evoluzione. Lo stesso motore di pittura supporta disegno preciso, accumulo pittorico, segni decorativi e rottura in stile media naturale.

Quando un pennello diventa visivamente complesso, l'anteprima può restare semplificata, così la pittura rimane reattiva e leggibile.

![tool-setup](/images/screens/tool-setup.jpg)

## Dinamiche e risposta all'input

Lo strumento Pennello risponde a input in tempo reale come pressione dello stilo, velocità, direzione, inclinazione e altri valori del controller. Questi segnali possono influenzare il tratto visibile in molti modi: spessore, opacità, angolo, risposta della texture, comportamento del colore, spaziatura e altre qualità possono cambiare mentre la mano si muove.

Il Pennello sembra meno un motivo stampato e più uno strumento di disegno fisico. Un tocco leggero produce segni delicati, un movimento più rapido apre texture o forme e un comportamento sensibile alla direzione aiuta i tratti a seguire il gesto della mano.

![dynamics](/images/screens/dynamics.jpg)

## Comportamento del tratto

I tratti possono essere diretti e immediati, oppure assistiti da levigatura e stabilizzazione. Queste funzioni aiutano a ridurre il tremolio indesiderato, ad ammorbidire i cambiamenti bruschi e a rendere i movimenti più lunghi più controllati senza togliere carattere all'input dell'artista.

Il Pennello supporta anche diversi approcci all'accumulo della vernice. Può comportarsi come un tratto continuo, accumulare tocchi ripetuti o emettere segni nel tempo mentre il puntatore resta fermo. Questa flessibilità lo rende utile sia per il lavoro di linea deliberato sia per la costruzione tonale più lenta.

Per segni calligrafici o simili all'inchiostro, il Pennello può generare un tratto dalla forma più continua invece di affidarsi solo a timbri ripetuti. Produce forme fluide, simili a nastri, che rispondono naturalmente a gesto e velocità.

![stroke](/images/screens/stroke.jpg)

## Acquisizione del tratto e rendering simulato

Il Pennello può catturare un piccolo campione di come una preimpostazione viene normalmente disegnata a mano, poi usare quel profilo nel rendering di tratti definiti dalla geometria anziché dal movimento in tempo reale. Linee dritte con Maiusc+clic, tracciati e selezioni con tratto possono usare il modello di pressione e velocità catturato dalla preimpostazione strumento attiva invece di comportarsi come una linea meccanica piatta.

I tratti costruiti restano più vicini al carattere del pennello. Una linea tracciata da un percorso può iniziare dolcemente, aumentare la pressione, assottigliarsi o variare la risposta alla velocità nello stesso modo del tratto campionato a mano, pur seguendo la forma esatta del percorso, del bordo di selezione o del gesto in linea retta.

## Post-elaborazione

Il Pennello può registrare un tratto mentre lo disegni, poi riprodurre il gesto catturato quando sollevi, perfezionando il percorso prima che venga depositato il segno finale. Puoi schizzare liberamente e ottenere comunque una direzione più pulita, angoli più netti o una struttura più deliberata senza dover disegnare con precisione meccanica.

Questo apre tratteggi e segni di costruzione regolati che si agganciano ad angoli puliti mantenendo lunghezza e carattere disegnati a mano, tratti a nastro stabili all'inclinazione e riproduzione sensibile agli angoli che tratta curve e tratti rettilinei in modo diverso. I pennelli a più teste possono condividere un percorso corretto mentre ciascuna testa mantiene la propria variazione, e le dinamiche possono ancora modellare il tratto lungo la curva finale durante la riproduzione. La post-elaborazione si applica ai tratti disegnati, non all'emissione continua dell'aerografo.

## Colore e texture

I tratti del pennello possono usare il colore di pittura attivo, rispondere alle sfumature o variare il colore tramite le dinamiche. La gestione della texture consente al pennello di passare da copertura solida a segni spezzati e radenti alla superficie, utile per effetti di pennello asciutto, grana e ombreggiature espressive.

Poiché colore e texture possono far parte dello stesso sistema dinamico di forma e opacità, un singolo tratto può evolversi mentre attraversa la tela invece di restare visivamente uniforme.

## Teste di pennello e formazioni

Lo strumento Pennello può dipingere con più di una testa alla volta. Più teste possono essere disposte attorno al percorso del tratto per creare segni di pennino, tratti a ventaglio, comportamento simile alle setole, pattern di spruzzo, formazioni strutturate o tratteggi organizzati.

Queste teste possono seguire la direzione del movimento, variare l'una dall'altra e disperdersi in modi che rendono il tratto organico piuttosto che ripetuto meccanicamente. È particolarmente utile per pennelli di media naturale, tratti decorativi, fogliame, pelliccia, tratteggi e altri segni che beneficiano di irregolarità controllata.

![brush-heads](/images/screens/brush-heads.jpg)

## Carico del pennello e prelievo della vernice

Il Pennello può anche simulare quanta vernice o materiale è attualmente trasportata sul pennello. Man mano che un tratto continua, il carico può ridursi gradualmente, lasciando che i segni diventino più leggeri, più asciutti, più sottili, più ruvidi o comunque più spezzati a seconda di come sono impostate le dinamiche del pennello.

Il carico può essere reintrodotto tra una passata e l'altra, mantenuto a un livello scelto o usato come segnale di controllo in tempo reale per altri comportamenti del pennello. È possibile costruire pennelli che sembrano più vicini ai media reali: bagnati all'inizio di una pennellata, progressivamente esauriti lungo la distanza e poi immersi di nuovo per la passata successiva.

![material-state](/images/screens/material-state.jpg)

## Contatto con la superficie del pennello

Il Pennello può anche simulare la perdita intermittente di contatto con la superficie di pittura: i segni spezzati che compaiono quando una matita, un pezzo di carboncino, un pennello asciutto o un pennarello parzialmente esaurito toccano la carta solo in parte.

Quando la simulazione del contatto è attiva, il pennello è in contatto oppure sollevato. In contatto i segni si depositano normalmente. Quando è sollevato non si deposita materiale e il tratto lascia uno spazio la cui lunghezza viene scelta casualmente tra una distanza minima e massima. La transizione è binaria: l'effetto non cambia opacità, dimensione, durezza, spaziatura o flusso, ma solo se la vernice viene depositata.

La facilità con cui si perde il contatto dipende da una soglia di contatto, dalla pressione dello stilo e, facoltativamente, dal carico del pennello. Valori di soglia più alti rendono le interruzioni più frequenti. La pressione agisce come forza stabilizzante: una pressione leggera aumenta la probabilità di perdere il contatto, mentre una pressione decisa rende più probabile che il tratto resti a contatto. Quando il carico del pennello è attivo, un carico basso può rendere il segno più spezzato e un carico alto può aiutare a mantenere il contatto, come uno strumento che trasporta ancora abbastanza materiale per aderire alla superficie.

La perdita viene valutata in base alla distanza percorsa dal tratto, non al conteggio dei tocchi, così i pennelli con spaziatura densa o rada si comportano in modo coerente. La funzione funziona sia con il rendering basato su timbri sia con quello calligrafico, producendo interruzioni coerenti lungo il tratto invece di tocchi saltati isolati.

## Animazione e variazione

Le sorgenti animate del pennello possono cambiare fotogramma man mano che il tratto avanza, dando ai pennelli un senso di movimento e varietà. Randomizzazione e variazione per tratto possono evitare che i segni ripetuti sembrino identici, mentre un seed stabile può preservare un carattere coerente quando serve ripetibilità.

Questi comportamenti sono utili per pennelli che dovrebbero sembrare vivi: setole che si spostano durante un tratto, timbri con texture che cambiano leggermente nel tempo o strumenti a più teste in cui ogni testa ha una personalità propria.

## Flusso di lavoro orientato all'artista

Lo strumento Pennello è organizzato in modo che le decisioni di pittura più comuni restino a portata di mano, mentre le scelte di configurazione meno frequenti restino fuori mano. L'obiettivo è mantenere lo strumento accessibile durante la pittura, supportando al tempo stesso una personalizzazione profonda per la progettazione del pennello.

Nel complesso, il Pennello copre sia la pittura quotidiana sia la creazione di segni specializzati: schizzi rapidi, illustrazioni rifinite, rendering texturizzato, lavoro espressivo a inchiostro ed effetti procedurali complessi condividono tutti la stessa base flessibile.
