---
title: "Filtri"
type: docs
url: "hub/features/filters"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
---

Il menu Filtri di Lumi riunisce in un unico posto regolazioni correttive, effetti di lente stilizzati, generatori di texture procedurali, trattamenti ispirati alla stampa e strumenti di analisi. L'ordine del menu è pratico piuttosto che accademico: gli strumenti di sfocatura e di miglioramento stanno uno accanto all'altro, gli effetti di distorsione e di illuminazione sono raggruppati per resa visiva e i generatori di texture o pattern restano insieme quando l'obiettivo è creare materiale sorgente piuttosto che modificare un'immagine esistente.

Le finestre di dialogo dei filtri seguono lo stesso flusso di lavoro generale. Preimpostazioni, anteprima, visualizzazione divisa e controlli di opacità o fusione consentono di regolare rapidamente un effetto; sui livelli il risultato può restare come filtro non distruttivo modificabile invece di essere fuso subito. Lumi conserva anche una cronologia recente dell'uso dei filtri, così ripetere l'ultimo effetto o riaprire l'ultima finestra fa parte del normale ritmo di pittura, non di un compito separato.

## Sfocatura

### Sfocatura gaussiana

La Sfocatura gaussiana è il filtro di ammorbidimento standard di Lumi: una sfocatura pulita e uniforme con controlli separati per dimensione orizzontale e verticale, gestione dei bordi e opzioni del kernel. È la scelta generale per messa a fuoco morbida, maschere attenuate, profondità atmosferica e qualsiasi flusso di lavoro in cui la sfocatura stessa deve restare neutra.

### Pixelizza

Pixelizza riduce i dettagli in strutture a blocchi deliberate invece di una sfocatura morbida. Poiché la finestra di dialogo espone larghezza e altezza del blocco, offset, forma dei pixel e comportamento di riempimento, funziona sia come effetto di censura grossolano sia come mosaico controllabile o trattamento grafico a bassa risoluzione.

### Sfocatura gaussiana selettiva

La Sfocatura gaussiana selettiva ammorbidisce all'interno delle regioni cercando di preservare i bordi più marcati. È utile quando un'immagine necessita di una texture più calma o di meno grana visiva senza perdere i confini di forma più ampi che devono restare leggibili.

### Sfocatura dell'obiettivo

Lens Blur è uno dei filtri di sfocatura di Lumi più orientati all'illustrazione. I suoi controlli ruotano attorno alla forma dell'iride poligonale, alla curvatura delle lame, all'allungamento anamorfico, all'incremento delle luci e a una regione di messa a fuoco configurabile, quindi si comporta meno come un ammorbidente generico e più come uno strumento di profondità di campo stilizzato con bokeh sagomato.

### Tilt-shift

Il tilt-shift mantiene nitida una banda di messa a fuoco controllabile sfocando progressivamente l'immagine sopra e sotto di essa. Angolo della banda, sfumatura, sbilanciamento prospettico, forma dell'iride e incremento miniatura della finestra di dialogo lo rendono adatto a scene in stile miniatura, viste architettoniche e composizioni in cui la messa a fuoco deve leggersi come una fascia progettata piuttosto che come un indicatore di profondità circolare.

### Sfocatura da movimento circolare

La Sfocatura da movimento circolare trascina i dettagli attorno a un punto centrale, trasformando i bordi in scie rotazionali. È la scelta naturale per soggetti in rotazione, energia simile a una turbina o illustrazioni che richiedono un senso di movimento orbitale.

### Sfocatura da movimento lineare

La Sfocatura da movimento lineare estende i dettagli in una direzione, simulando spostamento, movimento della fotocamera o gesto rapido nell'inquadratura. È particolarmente utile quando il movimento deve risultare direzionale e grafico piuttosto che diffuso.

### Sfocatura da movimento zoom

Zoom Motion Blur irradia i dettagli verso l'esterno da un centro, producendo la sensazione di una corsa verso o lontano dallo spettatore. Funziona bene per momenti di impatto, linee di velocità e composizioni che richiedono l'energia di uno zoom senza ridipingere l'intera immagine.

## Miglioramento

### Passa-alto

Il Passa-alto isola il contrasto locale fine piuttosto che un ampio cambiamento tonale. Con solo scala e contrasto da gestire, è uno strumento diretto per estrarre i dettagli dei bordi, creare sovrapposizioni nitide o preparare passaggi di nitidezza che devono enfatizzare la struttura più del colore.

### Riduzione del rumore

La Riduzione del rumore fa il movimento opposto: sopprime le variazioni fini indesiderate in modo che le forme più grandi risultino più chiare. È utile quando materiale scansionato, texture compresse o passaggi sovraccarichi devono essere semplificati prima di ulteriore pittura o filtraggio.

### Nitidezza

La Nitidezza usa un modello di maschera di contrasto, con raggio, intensità e soglia che controllano quanto fortemente viene spinto il contrasto locale. In pratica è adatta a ripristinare la chiarezza dopo sfocatura, ridimensionamento in esportazione o passaggi di rifinitura sottili in cui i dettagli devono emergere senza trasformare ogni pixel in rumore.

## Colore

### Gradazione tonale

Tonal Grading rimappa il colore per gamma tonale invece di rimodellare il contrasto o disegnare una curva. La luminanza di ciascun pixel sceglie una miscela uniforme di tre colori definiti dall'utente per ombre, mezzitoni e luci; l'immagine mantiene così la sua struttura chiaro-scuro mentre la tavolozza cambia. Intensità per regione, uno sbilanciamento in stile Lightroom (a sinistra favorisce la gradazione delle ombre, a destra quella delle luci) e morbidezza delle transizioni controllano quanto ogni colore si estende e quanto delicatamente le gradazioni si sovrappongono. È pensato per illustrazione, fumetti, concept art e fotografie quando l'obiettivo è una gradazione o una resa visiva coerente.

## Distorsione

### Aberrazione cromatica

L'Aberrazione cromatica separa i canali colore verso l'esterno da un centro scelto, con controlli per direzione radiale o tangenziale, sbilanciamento tra coppie di canali, attenuazione e conservazione della luminanza. Sia il codice sia la finestra di dialogo lo trattano come strumento a doppio senso: può aggiungere frange di lente stilizzate per energia visiva o invertire il segno per correggere una lieve aberrazione nel materiale sorgente.

### Distorsione dell'obiettivo

La Distorsione dell'obiettivo rimodella l'immagine attraverso curvatura a barilotto o a cuscinetto, termini dei bordi, compensazione dello zoom, offset del centro e schiarimento degli angoli. È utile sia per correggere un'immagine che sembra piegata otticamente sia per spingerla deliberatamente verso un carattere grandangolare o retrò.

## Illuminazione

### Bloom

Bloom trasforma le aree luminose in un bagliore controllato, con soglia, morbidezza, raggio e intensità che definiscono quanto la luce si diffonde e quanto solleva l'immagine. Il controllo aggiuntivo di limitazione dell'esposizione lo mantiene utilizzabile come effetto di evidenziazione piuttosto che come lavaggio automatico.

### Cielo

Sky è più di una sovrapposizione di tinta o sfumatura: renderizza un cielo analitico usando i modelli Preetham, Hosek/Wilkie o Nishita. Poiché la finestra di dialogo espone proiezione, angolo del sole, torbidità, densità atmosferica, altitudine, controlli del disco solare ed esposizione, può costruire qualsiasi cosa, da un semplice sfondo chiaro a un tramonto o un crepuscolo più radicato fisicamente.

### Vignetta

La Vignetta scurisce, colora o addirittura cancella verso i bordi dell'immagine, con controlli di forma, raggio, morbidezza, gamma, proporzione, compressione, rotazione e posizionamento sulla tela. Funziona come classico trattamento dei bordi fotografici, ma è abbastanza flessibile da fungere da maschera di inquadratura o da riflettore compositivo irregolare.

## Rumore

### Rumore HSV

Il Rumore HSV randomizza tonalità, saturazione e valore in modo indipendente. È utile quando un'immagine necessita di vivacità cromatica o instabilità analogica senza scomporre completamente la struttura locale.

### Hurl

Hurl è la versione estrema del rumore: sostituisce i pixel con colori del tutto casuali. È meglio pensarlo come fonte di caos distruttivo per lavori glitch, texture logore o maschere che richiedono una rottura aggressiva.

### Pick

Pick sostituisce ogni pixel con un vicino scelto casualmente, così l'immagine resta correlata alla sorgente invece di diventare statico puro. Il risultato è una variazione mescolata e granulare che può sembrare più organica del rumore completamente casuale.

### Spread

Spread disperde i pixel spostandoli casualmente entro un raggio. È utile quando si desidera un'interruzione senza movimento: una superficie spezzata, un bordo sbavato o una texture logora che conserva ancora le relazioni cromatiche dell'immagine sorgente.

### Fractal

Fractal genera rumore Perlin frattale piastrellabile, particolarmente prezioso come sorgente riutilizzabile per maschere, nuvole, texture di carta, rotture simili al terreno e sovrapposizioni procedurali. Poiché è piastrellabile, può alimentare flussi di lavoro più ampi senza creare giunture evidenti.

### Grana rumore blu

Blue Noise Grain è il generatore di grana monocromatica in stile pellicola e stampa di Lumi. Le preimpostazioni di dimensione della grana, il mascheramento del rumore blu, lo sbilanciamento dei mezzitoni, lo sbilanciamento delle ombre e i controlli del seed mostrano che è progettato per posizionare la grana in modo uniforme e controllabile, non solo per spruzzare macchie monocromatiche casuali sull'immagine.

### Grana risografica

Risograph Grain si basa sulla stessa logica della grana ma la trasforma in un effetto di stampa a due lastre. Colori di inchiostro separati, bilanciamento delle lastre, disallineamento deliberato e variazione con seed lo rendono adatto a poster, estetica di stampa indipendente e illustrazioni che dovrebbero sembrare sovrastampate fisicamente piuttosto che perfette digitalmente.

### Mezzitoni (FM)

Halftone (FM) crea un mezzotono stocastico modulato in frequenza usando rumore blu o metodi di soglia correlati. Con modalità colore monocromatiche, bicromatiche e CMYK, oltre ai controlli di dot-gain e decorrelazione delle lastre, mira a texture simili alla stampa che restano irregolari e vivaci invece di cadere in una griglia rigida.

## Bordi

### Differenza di gaussiane

La Differenza di gaussiane rileva i bordi sottraendo l'una dall'altra due versioni sfocate dell'immagine. È un operatore compatto e utile per mappe dei bordi, estrazione di linee stilizzate e individuazione di transizioni strutturali senza impegnarsi in un contorno con soglia completa.

## Morfologia

### Mediana

La Mediana sostituisce ogni pixel con il valore mediano del suo intorno, tendendo a rimuovere il rumore isolato preservando i confini più marcati meglio di una semplice sfocatura. È un filtro di pulizia pratico per appiattire piccole chiacchiere visive senza ammorbidire subito l'intera immagine.

### Dilatazione

La Dilatazione fa crescere le regioni più chiare verso l'esterno usando la stessa logica di intorno consapevole della forma. In termini di creazione d'immagine, può ispessire i segni luminosi, espandere le forme chiare o chiudere piccoli spazi scuri.

### Erosione

L'Erosione fa il movimento complementare, facendo crescere le regioni più scure e ritirando quelle più chiare. È utile per assottigliare i dettagli chiari, ingrandire le masse scure o restringere maschere e forme grafiche.

## Pattern

### Scacchiera

La Scacchiera genera uno schema regolare di tessere alternate. È semplice, ma proprio per questo è utile per testare la trasparenza, costruire maschere, bloccare sfondi grafici o creare materiale sorgente geometrico pulito.

### Griglia

La Griglia disegna divisioni orizzontali e verticali ripetute, rendendola utile per guide di layout, sfondi di design, illustrazioni tecniche e mascheramenti procedurali. Poiché viene generata come filtro, spaziatura e aspetto possono essere regolati senza costruire manualmente il pattern.

### Voronoi

Voronoi genera una texture cellulare piastrellabile da punti con seed, con controlli per tipo di elemento, metrica della distanza, casualità, dettaglio frattale e avvolgimento senza soluzione di continuità. In pratica può passare da strutture pulite a celle incrinata a pattern più organici di pietra, pelle, mappa o reti astratte.

### Onda

Wave produce pattern a bande o ad anello modellati da profilo della forma d'onda, disposizione geometrica, distorsione, dettaglio frattale e offset di fase. È più di un semplice strumento per strisce: può generare increspature controllate, bande topografiche, grafica simile al moiré o campi concentrici rumorosi.

### Mezzitoni (AM)

Halftone (AM) applica un classico retino a punti modulato in ampiezza, con controlli di frequenza, forma del punto, nitidezza, modalità colore e angolo CMYK per una struttura di stampa in stile rosetta. Rispetto ai mezzitoni FM, è l'opzione più ordinata e riconoscibilmente meccanica quando la resa desiderata è carta da giornale, litografia offset o geometria dello schermo deliberatamente visibile.
