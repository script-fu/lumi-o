---
title: "Strumento Pennello"
type: docs
---
Il pennello è lo strumento di pittura principale, progettato per pennellate reattive e intelligenti con il pieno controllo delle dinamiche di pressione, velocità, inclinazione e spaziatura.

## Panoramica

Lo strumento Pennello supporta tipi di pennello raster, generati proceduralmente e animati. I tratti possono essere stabilizzati, smussati e post-elaborati. Le dinamiche del pennello rispondono all'input dello stilo, offrendo un controllo preciso su opacità, dimensione, colore, angolo e altre proprietà durante un tratto.

## Tipi di pennelli

### Pennelli raster (.raster)

Immagini pennello bitmap che supportano la trasparenza alfa.

### Pennelli generati (.param)

Forme renderizzate proceduralmente (cerchio, quadrato, diamante, triangolo) con parametri regolabili: durezza, proporzioni, angolo, rotondità e raggio dell'angolo. I pennelli generati sono leggeri e scalabili.

### Pennelli animati (.anim)

Sequenze di fotogrammi sequenziali che avanzano durante i colpi. I fotogrammi possono essere ciclizzati in modo incrementale (avanzamenti dei fotogrammi per tocco), selezionati casualmente per tocco o indicizzati in base alla dinamica (pressione, velocità, inclinazione, angolo).

## Cursore di disegno

Il cursore si adatta allo stato corrente dello strumento per fornire un feedback chiaro e contestuale:

- **Contorno del pennello**: il cursore traccia l'esatta forma e dimensione del pennello, fornendo un'anteprima in tempo reale di dove atterrerà la vernice.
- **Modalità di cancellazione**: quando la cancellazione è attiva, il contorno diventa un cerchio tratteggiato per distinguere visivamente i tratti di cancellazione dai tratti di pennello.
- **Contorno pennello semplice**: per pennelli complessi o molto grandi in cui il rendering del contorno accurato è costoso, attiva **Contorno pennello semplice** (in Opzioni aggiuntive) per utilizzare invece un cerchio semplice.

## Opzioni dello strumento

### Controlli di livello superiore

Presente in ogni momento, al di fuori di qualsiasi espansore:
- **Modalità**: modalità di fusione dei colori (Normale, Moltiplica, Schermo, ecc.)
- **Opacità**: opacità complessiva del tratto (0–100).

### Proprietà del pennello

Nell'espansore **Proprietà pennello** (espanso per impostazione predefinita):
- **Dimensione**: diametro del pennello in pixel.
- **Proporzioni**: schiaccia o allunga la forma del pennello (-1,0–1,0). 0 = non modificato; valori negativi ruotano la zucca di 90°.
- **Angolo**: ruota il timbro del pennello (-180–180°). Indipendente dalla dinamica della direzione della corsa.
- **Durezza**: dissolvenza morbida (0,0) su spigolo vivo (1,0).
- **Spaziatura**: distanza tra i tocchi dipinti come percentuale della dimensione del pennello. Inferiore = tratti più fluidi; più alto = modello sparso.
- **Texture Bias**: influenza la risposta della texture del timbro; 50 è neutro. Valori più bassi favoriscono la rottura della texture e una superficie rasata tirando verso la punta della curva del valore; valori più alti si bloccano verso riempimenti solidi spingendo verso la spalla. L'effetto visibile dipende dalla gamma tonale della texture.
- **Jitter**: compensa casualmente ciascuna posizione di tocco fino a questo numero di pixel (0–1024).
- **Gomma**: moltiplicatore di dimensione applicato quando questo pennello viene utilizzato come gomma (0,1–10,0). Non mostrato sullo strumento Gomma stesso.

### Dinamica

Nell'espansore **Dinamiche**:
- **Dinamiche**: abilitazione Master per il preset delle dinamiche attive.
- **Dynamics Preset**: seleziona quali mappature di input vengono utilizzate.
- **Moltiplica per pressione**: attiva/disattiva la moltiplicazione extra della pressione (mostrata quando la Dinamica è abilitata).### Comportamento in caso di ictus
Nell'espansore **Comportamento del tratto**:
- **Creazione**: se attivata, ogni tocco accumula opacità anziché essere composto come un singolo tratto.
- **Post processo**: applica la stabilizzazione, la compressione della velocità e la correzione della riproduzione al termine del tratto, migliorando la coerenza senza latenza.
  - **Soglia di svolta**: soglia dell'angolo (0–180°) per la correzione della direzione in corrispondenza di angoli acuti. 0 = correzione della direzione di salto.
  - **Soglia anteprima**: Sopprime l'anteprima post-elaborazione quando la velocità del tratto supera questo valore (0 = sempre anteprima).

#### Calligrafico

Quando è attivo, il dab stamping viene sostituito da un corridoio geometrico continuo:
- **Opacità dinamica**: modula l'opacità all'interno del tratto in base ai cambiamenti di velocità e direzione. Funziona meglio con colpi fini e controllati; i risultati sono meno prevedibili negli scarabocchi rapidi. Sperimentale.
- **Crescita velocità** (0–100%): aumento massimo consentito delle dimensioni per campione come percentuale delle dimensioni del campione precedente. Limita la velocità con cui può crescere una dinamica dimensionale guidata dalla velocità, prevenendo salti improvvisi quando la corsa accelera.
- **Velocity Shrink** (0–100%): riduzione massima consentita delle dimensioni per campione. Limita la velocità con cui le dimensioni possono diminuire quando il tratto decelera.

#### Stabilizzazione e levigatura

- **Distanza di stabilizzazione della direzione** (0–100 px): corsa minima del puntatore prima che inizi il comportamento sensibile alla direzione, aiutando a evitare salti angolari precoci.

#### Levigatura

Abilita l'arrotondamento dell'input in tempo reale applicato al tracciato del tratto mentre dipingi. Si espande per rivelare:
  - **Profondità** (2–256): numero di campioni di input precedenti considerati durante il calcolo della posizione livellata. Valori più alti producono un ritardo più lungo e più impegnato.
  - **Posizione** (0–100): intensità di levigatura applicata alla posizione del pennello. Valori più alti completano bruschi cambiamenti di direzione.
  - **Pressione** (0–100): attenuazione applicata al segnale di pressione dello stilo, riducendo i picchi di pressione e il jitter.
  - **Direzione** (0–100): attenuazione applicata alla direzione del tratto, stabilizzando la dinamica sensibile all'angolo.

#### Dinamica

Assegna l'input dello stilo o altri valori live ai parametri di pittura:

- **Pressione** (stilo): controlla dimensioni, opacità, velocità, durezza, colore e altro in base alla pressione dello stilo.
- **Velocità**: associa la velocità del tratto alle proprietà del pennello.
- **Inclinazione**: gli angoli di inclinazione X e Y dello stilo influiscono sull'angolo e su altri parametri.
- **Rotella**: input dalla rotellina del mouse o dalla rotellina dello stilo.
- **Direzione**: Angolo della direzione della corsa.
- **Dissolvenza**: dissolve l'opacità o la dimensione su un numero fisso di tocchi.

Ogni input dinamico può essere mappato su più proprietà in modo indipendente. Apri **Opzioni strumento** → **Dinamiche** per configurare.

### Modulazione della corsa

Nell'espansore **Modulazione corsa** (mostrato solo quando **Dinamica** è abilitata):- **Angolo iniziale relativo**: il valore dell'**Angolo iniziale** viene interpretato rispetto alla direzione del tratto anziché come un angolo assoluto della tela.
- **Angolo iniziale dissolvenza**: sfuma dall'**Angolo iniziale** all'inizio del tratto verso l'angolo dinamico in tempo reale nel corso del tratto. L'attivazione di questa opzione forza l'attivazione dell'**Angolo iniziale relativo**.
- **Angolo iniziale del pennello** (-180–180°): l'angolo del pennello all'inizio di un tratto, prima che la dinamica prenda il sopravvento.
- **Miscela angolo iniziale** (0,0–1,0): controlla la velocità con cui l'angolo del pennello passa dall'angolo iniziale all'angolo dinamico. 0 = mantiene l'angolo iniziale; 1 = utilizza immediatamente l'angolo completamente dinamico.
- **Lunghezza dissolvenza**: distanza in unità di tela su cui viene riprodotta la dissolvenza.
- **Ripeti**: come viene ripetuta la dissolvenza una volta esaurita la durata della dissolvenza (None, Loop, Sawtooth, Triangle).


### Testine per spazzole

Testine della spazzola posiziona più testine della spazzola indipendenti su un **anello orbitale** circolare centrato sul percorso del tratto. Ogni testa dipinge un tocco completo nella propria posizione ogni volta che il tratto avanza, producendo più tratti paralleli o a ventaglio contemporaneamente.

Il raggio dell'orbita è determinato dalla dimensione globale della spazzola meno la dimensione della testa: le teste più grandi si trovano più vicine al centro; le teste più piccole orbitano più lontano. Le teste si distanziano uniformemente attorno all'anello. Con due teste se ne ottiene una su ciascun lato del tratto, creando una stesura simmetrica che si comporta come un pennino per calligrafia. Il cursore **Segui direzione** ruota l'intero anello per rimanere perpendicolare al tratto, in modo che il pennino segua la direzione in modo naturale mentre dipingi. Aggiungendo più teste, le si ventaglio progressivamente attorno all'anello, fino a un cerchio di spruzzo completo a 16.

I controlli vengono visualizzati nell'espansore **Teste di pennello** nel pannello delle opzioni dello strumento.

- **Conteggio**: numero di testine simultanee (1–16).
- **Dimensione testa**: dimensione renderizzata di ciascuna testa rispetto alla dimensione globale del pennello (0,1–1,0).
- **Rapporto d'aspetto orbita** (0,1–1,0): modella l'orbita di formazione dal cerchio all'ellisse. 1.0 = orbita circolare; valori più bassi schiacciano l'asse minore.
- **Angolo di formazione** (0–360°): orientamento statico dell'anello di formazione, utilizzato quando **Segui direzione** è inferiore a 1,0.
- **Segui direzione** (0,0–1,0): con quale intensità l'anello di formazione segue la direzione di viaggio del tratto. A 1.0 l'anello è sempre perpendicolare al senso di marcia; a 0,0 si blocca sul valore statico **Angolo di formazione**.
- **Variazione della pressione**: variazione delle dimensioni pro capite applicata come bias di pressione indipendente attraverso le curve dinamiche.
- **Variazione dell'opacità**: variazione dell'opacità per testa, indipendente dalla variazione delle dimensioni.

#### Dispersione

Principali controlli di dispersione nell'espansore **Teste di pennello**:

- **Angolo di dispersione** (0–360°, predefinito 10°): ruota solo il componente di dispersione casuale (non la spaziatura di riempimento). Gli angoli per testa/per tocco sono polarizzati verso l'esterno con crossover controllato per evitare pennacchi specchiati rigidi. Bloccato a 360°.
- **Distanza di dispersione** (0–10000 px): spostamento in avanti casuale dalla posizione di spaziatura di riempimento di ciascuna testa. Rilaminato ogni tocco.
- **Bilanciamento dimensione dispersione** (0,0–1,0): controlla la pendenza della soppressione per le teste sopra la soglia. A 1.0, tutte le teste si disperdono equamente; valori più bassi sopprimono sempre più le teste più grandi mentre le teste al/sotto la soglia rimangono alla distanza di dispersione completa.

### Opzioni aggiuntive

Nell'espansore **Opzioni aggiuntive** (compresso per impostazione predefinita), i controlli sono raggruppati come sezioni di overflow che vengono modificate meno spesso. Ciò mantiene gli espansori principali concentrati sui controlli di verniciatura regolati di frequente.#### Proprietà del pennello (overflow)
- **Blocca l'angolo sullo spazio sullo schermo**: blocca l'angolo del pennello sullo spazio dello schermo, in modo che l'angolo rimanga in piano mentre la tela ruota/capovolge. Nessun effetto quando Dynamics controlla l'angolo.
- **Capovolgimento orizzontale casuale**: 50% di possibilità di specchiare ogni timbro da sinistra a destra per ogni tocco.
- **Capovolgimento verticale casuale**: 50% di possibilità di capovolgere ciascun timbro per tocco.
- **Rotazione casuale**: ruota casualmente ciascun timbro di 0°, 90°, 180° o 270° per tocco.
- **Jitter uniforme**: quando attivato, gli offset dei tocchi dal cursore **Jitter** vengono ricavati da una distribuzione uniforme (ogni offset ha la stessa probabilità all'interno dell'intervallo). Quando è disattivata, la distribuzione è gaussiana (sposta il cluster verso il centro).
- **Reimposta animazione**: Per i pennelli animati: quando è attivo, l'animazione riparte dal fotogramma 0 ad ogni nuovo tratto; quando è spento, continua da dove è terminata la corsa precedente.

#### Testine per spazzole (troppopieno)

Formazione:
- **Rigidità delle setole**: quanto rigidamente il raggio dell'orbita segue la dimensione del pennello in scala dinamica. 0 = l'orbita si espande e si contrae con la pressione; 1 = l'orbita rimane fissa alla dimensione della base.
- **Spaziatura riempimento** (0,0–1,0): distribuisce le teste attraverso lo spazio tra posizioni di tocco consecutive. Il valore del carattere stabile di ciascuna testa determina la sua direzione di inclinazione; a 1,0 le teste riempiono l'intero intervallo di spaziatura. Il carattere è stabile per seme.

Dispersione:
- **Soglia dimensione dispersione** (0,01–100 px): raggio di soglia per la distanza di dispersione completa. Le teste pari o inferiori a questo raggio utilizzano la distanza di dispersione completa; le teste più grandi vengono progressivamente avvicinate alla corsa.

Randomizzazione:
- **Seme carattere** (0–255): numero fisso per carattere per testa (dimensione, posizione di spaziatura di riempimento). Lo stesso seme riproduce la stessa formazione ad ogni colpo. Desensibilizzato quando **Randomizza personaggio testa** è attivo.
- **Randomizza carattere testa**: ridisegna i valori dei caratteri per testa (dimensioni, posizione di dispersione) ogni francobollo in modo che la formazione sia completamente caotica lungo il tratto. Sostituisce **Seme carattere**.
- **Randomizza fotogrammi di animazione**: per i pennelli animati: ciascuna testa fa avanzare il proprio fotogramma di animazione in modo indipendente.

#### Comportamento del tratto (overflow)

- **Ripristina gli ultimi colori utilizzati**: ripristina i colori di primo piano e di sfondo della sessione precedente all'avvio, invece di tornare al bianco e nero per impostazione predefinita.
- **Contorno pennello semplice**: utilizza un cerchio semplice per il contorno del cursore del pennello invece di riprodurre la forma completa del pennello. Utile per pennelli complessi o di grandi dimensioni in cui è costoso disegnare un contorno accurato.