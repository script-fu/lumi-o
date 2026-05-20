---
title: "Filtri"
type: docs
---
Il menu Filtri di Lumi riunisce in un unico posto regolazioni correttive, effetti di lenti stilizzate, generatori di texture procedurali, trattamenti ispirati alla stampa e strumenti di analisi. L'ordine dei menu è pratico piuttosto che accademico: gli strumenti di sfocatura e di miglioramento si trovano uno accanto all'altro, gli effetti di distorsione e di illuminazione sono raggruppati per aspetto e i generatori di texture o pattern vengono tenuti insieme quando l'obiettivo è creare materiale sorgente piuttosto che modificare un'immagine esistente.

Le finestre di dialogo dei filtri seguono lo stesso flusso di lavoro generale. Le preimpostazioni, l'anteprima, la visualizzazione divisa e i controlli di opacità o fusione consentono di ottimizzare rapidamente un effetto e sui livelli il risultato può rimanere come filtro modificabile non distruttivo invece di essere unito immediatamente. Lumi conserva anche una cronologia recente dell'utilizzo del filtro, quindi ripetere l'ultimo effetto o riaprire l'ultimo dialogo fa parte del normale ritmo di pittura piuttosto che di un compito separato.

## Sfocatura

### Sfocatura gaussiana

Sfocatura gaussiana è il filtro attenuatore standard di Lumi: una sfocatura pulita e uniforme con controlli separati delle dimensioni orizzontali e verticali, gestione dei bordi e opzioni del kernel. È la scelta generica per messa a fuoco morbida, maschere attenuate, profondità atmosferica e qualsiasi flusso di lavoro in cui la sfocatura stessa dovrebbe rimanere neutra.

### Pixelizza

Pixelizzazione riduce i dettagli in strutture a blocchi deliberate invece che in una morbida sfocatura. Poiché la finestra di dialogo espone la larghezza del blocco, l'altezza del blocco, gli offset, la forma dei pixel e il comportamento di riempimento, funziona sia come un effetto di censura grossolano che come un mosaico controllabile o un trattamento grafico a bassa risoluzione.

### Sfocatura gaussiana selettiva

La sfocatura gaussiana selettiva si ammorbidisce all'interno delle regioni cercando di preservare i bordi più forti. È utile quando un'immagine necessita di una trama più calma o di vibrazioni ridotte senza perdere i confini della forma più ampi che devono ancora essere letti chiaramente.

### Sfocatura dell'obiettivo

Lens Blur è uno dei filtri sfocatura di Lumi più incentrati sull'illustrazione. I suoi controlli sono costruiti attorno alla forma dell'iride poligonale, alla curvatura della lama, all'allungamento anamorfico, all'aumento delle luci e a una regione di messa a fuoco configurabile, quindi si comporta meno come un ammorbidente generico e più come uno strumento di profondità di campo stilizzato con bokeh sagomato.

### Tiltshift

Il tilt-shift mantiene nitida una banda di messa a fuoco controllabile sfocando progressivamente l'immagine sopra e sotto di essa. L'angolo di banda, la sfumatura, la polarizzazione prospettica, la forma dell'iride e l'aumento della miniatura del dialogo lo rendono adatto a scene dall'aspetto in miniatura, viste architettoniche e qualsiasi composizione in cui la messa a fuoco dovrebbe essere letta come una striscia progettata piuttosto che come un indicatore di profondità circolare.

### Sfocatura movimento circolare

L'effetto movimento circolare diffonde i dettagli attorno a un punto centrale, trasformando i bordi in tracce rotazionali. È la scelta naturale per soggetti rotanti, energia simile a una turbina o illustrazioni che necessitano di un senso di movimento orbitale.

### Sfocatura del movimento lineare

L'effetto movimento lineare estende i dettagli in una direzione, simulando il viaggio, il movimento della fotocamera o il gesto rapido nell'inquadratura. È particolarmente utile quando il movimento deve sembrare direzionale e grafico piuttosto che diffuso.

### Sfocatura movimento zoom

Zoom Motion Blur irradia i dettagli verso l'esterno da un centro, producendo la sensazione di una corsa verso o lontano dallo spettatore. Funziona bene per momenti di impatto, linee di velocità e composizioni che richiedono l'energia dello zoom della fotocamera senza ridipingere l'intera immagine.

## Migliora

### Passaggio altoPassa alto isola il contrasto locale fine piuttosto che il cambiamento tonale ampio. Con solo scala e contrasto da gestire, è uno strumento semplice per estrarre i dettagli dei bordi, creare sovrapposizioni nitide o preparare passaggi di nitidezza che dovrebbero enfatizzare la struttura più del colore.

### Riduzione del rumore

La riduzione del rumore è la mossa opposta: sopprime le variazioni fini indesiderate in modo che le forme più grandi vengano lette più chiaramente. È utile quando è necessario semplificare materiale scansionato, texture compresse o passaggi sovraccarichi prima di verniciarlo o filtrarlo ulteriormente.

### Affila

Nitidezza utilizza un modello di maschera di contrasto, con raggio, quantità e soglia che controllano la forza con cui viene spinto il contrasto locale. In pratica, ciò lo rende adatto per ripristinare la chiarezza dopo la sfocatura, il ridimensionamento dell'esportazione o passaggi di finitura sottili in cui i dettagli devono emergere senza trasformare ogni pixel in rumore.

## Distorci

### Aberrazione cromatica

L'aberrazione cromatica separa i canali di colore verso l'esterno da un centro scelto, con controlli per la direzione radiale o tangenziale, polarizzazione tra coppie di canali, decadenza e conservazione della luminanza. Sia il codice che la finestra di dialogo lo trattano come uno strumento a doppio senso: può aggiungere una sfrangiatura stilizzata della lente per ottenere energia o invertire il segno per correggere una lieve aberrazione nel materiale originale.

### Distorsione dell'obiettivo

La distorsione dell'obiettivo rimodella l'immagine attraverso la curvatura a barilotto o a cuscinetto, i termini dei bordi, la compensazione dello zoom, gli offset centrali e la luminosità degli angoli. Ciò lo rende utile sia per correggere un'immagine che sembra piegata otticamente sia per spingerla deliberatamente verso un obiettivo grandangolare o retrò.

## Illuminazione

### Fiorisci

Bloom trasforma le aree luminose in un bagliore controllato, con soglia, morbidezza, raggio e forza che definiscono quanto lontano si diffonde la luce e quanto fortemente solleva l'immagine. Il controllo aggiuntivo di limitazione dell'esposizione lo mantiene utilizzabile come effetto di evidenziazione piuttosto che come lavaggio automatico.

### Cielo

Sky è più di una sovrapposizione di tinte o gradienti: esegue il rendering di un cielo analitico utilizzando i modelli Preetham, Hosek/Wilkie o Nishita. Poiché la finestra di dialogo espone proiezione, angolo del sole, torbidità, densità atmosferica, altitudine, controlli del disco solare ed esposizione, può creare qualsiasi cosa, da un semplice sfondo chiaro a un tramonto o un cielo crepuscolare più radicato fisicamente.

### Vignetta

La vignettatura scurisce, colora o addirittura cancella verso i bordi dell'immagine, con controlli di forma, raggio, morbidezza, gamma, proporzione, compressione, rotazione e posizionamento sulla tela. Funziona come un classico trattamento dei bordi fotografici, ma è abbastanza flessibile da fungere da maschera incorniciante o riflettore compositivo irregolare.

## Rumore

### Rumore HSV

Il rumore HSV randomizza tonalità, saturazione e valore in modo indipendente. Ciò lo rende utile quando un'immagine necessita di vivacità cromatica o instabilità analogica senza scomporre completamente la struttura locale.

### Lancia

Hurl è la versione estrema del rumore: sostituisce i pixel con colori del tutto casuali. È meglio pensarlo come una fonte di caos distruttivo per lavori su glitch, trame in difficoltà o maschere che necessitano di una rottura aggressiva.

### Scegli

Scegli sostituisce ogni pixel con un vicino scelto casualmente, quindi l'immagine rimane correlata alla sua fonte invece di diventare puramente statica. Il risultato è una variazione mescolata e granulare che può sembrare più organica del rumore completamente casuale.

### DiffusioneDistribuisci disperde i pixel spostandoli casualmente all'interno di un raggio. È utile quando si desidera un'interruzione immobile: una superficie rotta, un bordo sbavato o una trama invecchiata che conserva ancora le relazioni cromatiche dell'immagine sorgente.

### Frattale

Fractal genera rumore Perlin frattale piastrellabile, che lo rende particolarmente prezioso come fonte riutilizzabile per maschere, nuvole, texture di carta, disgregazione simile al terreno e sovrapposizioni procedurali. Poiché è piastrellabile, può alimentare flussi di lavoro più ampi senza creare giunture evidenti.

### Grana di rumore blu

Blue Noise Grain è il generatore di grana monocromatica in stile pellicola e stampa di Lumi. Le preimpostazioni della dimensione della grana della finestra di dialogo, il mascheramento del rumore blu, la compensazione dei mezzitoni, la compensazione dell'ombra e i controlli seed mostrano che è progettato per posizionare la grana in modo uniforme e controllabile, non solo per spruzzare macchie monocromatiche casuali sull'immagine.

### Grana risografica

Risograph Grain si basa sulla stessa logica della grana ma la trasforma in un effetto di stampa a due lastre. I colori di inchiostro separati, il bilanciamento delle lastre, la registrazione errata deliberata e le variazioni seminate lo rendono adatto per lavori su poster, estetica di stampa indipendente e illustrazioni che dovrebbero sembrare sovrastampate fisicamente piuttosto che perfette digitalmente.

### Mezzitoni (FM)

Halftone (FM) crea un mezzitono stocastico, modulato in frequenza utilizzando il rumore blu o metodi di soglia correlati. Con le modalità colore monocromatiche, bicromatiche e CMYK, oltre ai controlli del dot-gain e della decorrelazione delle lastre, mira a ottenere texture simili a quelle di stampa che rimangono irregolari e vivaci invece di cadere in una griglia rigida.

## Bordi

### Differenza delle gaussiane

La differenza delle gaussiane rileva i bordi sottraendo l'una dall'altra due versioni sfocate dell'immagine. È un operatore compatto e utile per mappe di bordi, estrazione di linee stilizzate e ricerca di transizioni strutturali senza impegnarsi in un contorno con soglia completa.

## Morfologia

### Mediana

La mediana sostituisce ogni pixel con il valore mediano del suo quartiere, che tende a rimuovere il rumore isolato preservando confini più forti meglio di una semplice sfocatura. È un pratico filtro di pulizia per appiattire piccole chiacchiere visive senza ammorbidire immediatamente l'intera immagine.

### Dilatare

Dilate fa crescere le regioni più leggere verso l'esterno utilizzando la stessa logica di vicinato consapevole della forma. In termini di creazione di immagini, può ispessire i segni luminosi, espandere le forme luminose o chiudere piccoli spazi scuri.

### Erodere

Erode fa la mossa complementare, facendo crescere le regioni più scure e ritirando quelle più chiare. È utile per assottigliare i dettagli chiari, ingrandire le masse scure o restringere maschere e forme grafiche.

## Modello

### Scacchiera

La scacchiera genera uno schema regolare di tessere alternate. È semplice, ma questa semplicità lo rende utile per testare la trasparenza, costruire maschere, bloccare sfondi grafici o creare materiale sorgente geometrico pulito.

### Griglia

La griglia disegna divisioni orizzontali e verticali ripetute, rendendola utile per guide di layout, sfondi di design, illustrazioni tecniche e mascheramenti procedurali. Poiché viene generato come filtro, la spaziatura e l'aspetto possono essere regolati senza creare manualmente il modello.

### Voronoi

Voronoi genera una texture cellulare piastrellabile da punti seminati, con controlli per tipo di elemento, metrica della distanza, casualità, dettaglio frattale e avvolgimento senza soluzione di continuità. In pratica può passare da strutture pulite di cellule incrinate a modelli più organici di pietra, pelle, mappa o reticoli astratti.

### OndaWave produce motivi a bande o ad anello modellati in base al profilo della forma d'onda, alla disposizione geometrica, alla distorsione, al dettaglio frattale e all'offset di fase. Ciò lo rende più di un semplice strumento per la creazione di strisce: può generare increspature controllate, bande topografiche, grafica simile all'effetto moiré o campi con motivi concentrici rumorosi.

### Mezzitoni (AM)

Mezzitoni (AM) applica un classico retino a punti modulato in ampiezza, con controlli di frequenza, forma del punto, nitidezza, modalità colore e angolo CMYK per una struttura di stampa in stile rosetta. Rispetto ai mezzitoni FM, è l'opzione più ordinata e riconoscibilmente meccanica quando l'aspetto desiderato è carta da giornale, litografia offset o geometria dello schermo deliberatamente visibile.