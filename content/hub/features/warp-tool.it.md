---
title: "Strumento Deformazione"
type: docs
---
Lo strumento Warp spinge, tira e fa scorrere liberamente i pixel sulla tela. In Lumi va oltre la maggior parte delle implementazioni: può deformare un intero gruppo di livelli – non importa quanti livelli e maschere nidificati contenga – come un singolo oggetto unificato, senza appiattirsi o perdere alcuna struttura.

## Panoramica

Seleziona un livello e trascinalo su di esso per spostare i pixel in qualsiasi direzione. La deformazione non è distruttiva mentre lavori: puoi annullare e ripetere singoli tratti, modificare la dimensione del pennello o il comportamento tra i tratti e continuare a perfezionare fino al commit. Il commit applica la mappa di spostamento accumulata in modo distruttivo ai dati dei pixel del livello.

Quando è selezionato un **livello di gruppo**, lo strumento opera sul gruppo nel suo insieme. Vedi e interagisci con un'anteprima dal vivo dell'intero gruppo composito. Al momento del commit, la stessa distorsione viene applicata in modo preciso e indipendente a ogni livello figlio e maschera all'interno del gruppo, preservando la struttura completa dei livelli.

## Deformazione del gruppo

Deformare un gruppo è la capacità principale che distingue lo strumento di deformazione di Lumi.

### Il problema che risolve

Nella maggior parte dei programmi di pittura, per deformare un'illustrazione multistrato è necessario prima appiattire il gruppo (distruggendo la struttura degli strati) oppure deformare ogni strato separatamente e cercare di abbinarli a occhio (noioso e impreciso). Nessuno dei due approcci preserva la struttura originale per ulteriori modifiche non distruttive.

Lumi deforma l'intero gruppo come un unico oggetto e quindi distribuisce la stessa identica trasformazione su ogni strato al suo interno.

### Come funziona

Quando selezioni un gruppo e inizi un tratto di distorsione, Lumi crea un **livello di anteprima mobile** dalla proiezione composita del gruppo. Se il gruppo ha una maschera, la maschera viene inserita nell'anteprima in modo che l'anteprima rappresenti accuratamente l'aspetto finale. Dipingi i tuoi tratti di curvatura direttamente su questa anteprima: ciò che vedi è esattamente ciò che ottieni.

Al momento dell'impegno, Lumi:

1. Applica lo spostamento a ogni livello base all'interno del gruppo (compresi i livelli profondamente annidati nei sottogruppi), espandendo la tela di ogni livello quanto basta per catturare l'intera area di distorsione.
2. Applica lo stesso spostamento a ogni maschera all'interno del gruppo nello stesso passaggio.
3. Riprende il calcolo automatico dei limiti del gruppo in modo che il gruppo si ridimensioni per adattarsi ai suoi figli appena deformati.
4. Ritaglia ogni strato deformato riportandolo al suo contenuto effettivamente dipinto per mantenere compatte le dimensioni del file.
5. Rimuove il livello di anteprima e rigenera la proiezione di gruppo dai figli aggiornati.

Tutto ciò avviene in un unico passaggio di annullamento. Dopo il commit, il gruppo appare esattamente come nell'anteprima, con ogni livello e maschera intatti.

### Maschere

L'opzione **Maschere Warp** (abilitata per impostazione predefinita) fa sì che le maschere su ogni livello e gruppo all'interno del target di warp ricevano la stessa trasformazione di spostamento. Le maschere di livello si spostano con i rispettivi livelli: una maschera che stava ritagliando il contorno di un personaggio continua a ritagliare lo stesso contorno dopo la deformazione.

Quando **Maschere deformazioni** è disattivato, viene spostato solo il contenuto del livello; le maschere mantengono le loro posizioni originali.

## Opzioni dello strumento

### Comportamento

| Modalità | Effetto |
| :--- | :--- |
| **Sposta** | Spinge i pixel nella direzione del tratto. La modalità principale per la maggior parte dei lavori di deformazione. |
| **Crescere** | Espande i pixel verso l'esterno dal centro del pennello. |
| **Riduci** | Tira i pixel verso l'interno verso il centro del pennello. |
| **Ruota in senso orario** | Ruota i pixel in senso orario attorno al centro del pennello. |
| **Ricciolo in senso antiorario** | Ruota i pixel in senso antiorario attorno al centro del pennello. |
| **Cancella** | Rimuove lo spostamento della distorsione, ripristinando i pixel nelle loro posizioni originali. |
| **Liscio** | Diffonde lo spostamento, ammorbidendo le transizioni brusche tra aree deformate e non deformate. |

### Controlli del pennello

- **Dimensione**: diametro del pennello di ordito in pixel. I pennelli più grandi spostano aree più ampie con una caduta più morbida; le spazzole più piccole danno un controllo preciso e localizzato.
- **Durezza**: caduta dal centro al bordo. L'elevata durezza produce uno spostamento uniforme su tutta l'area della spazzola; la bassa durezza concentra l'effetto al centro.
- **Intensità**: la distanza di spostamento dei pixel per tratto. La resistenza inferiore consente una modellatura sottile e graduale; una forza maggiore produce movimenti drammatici e veloci.

### Cronometraggio dei colpi

- **Tratto durante il movimento** (solo modalità Movimento): applica la distorsione continuamente mentre il mouse si muove, anziché solo in base a un impulso del timer. Utilizzare per tratti fluidi, simili a pennelli in cui si desidera che lo spostamento segua direttamente il cursore.
- **Tratto periodico**: applica la distorsione a un intervallo di tempo fisso mentre si tiene premuto il pulsante del mouse. Utilizzare per le modalità Aumenta, Riduci e Vortice in cui lo scopo è l'applicazione circolare continua.
- **Frequenza**: la frequenza dell'applicazione della corsa periodica.

### Qualità

- **Interpolazione**: il metodo di campionamento utilizzato durante il commit. Lineare è veloce e fluido per la maggior parte del lavoro; Cubic e Nohalo offrono una maggiore fedeltà per i dettagli più fini.
- **Anteprima di alta qualità**: utilizza il campionatore di qualità di commit durante l'anteprima interattiva. Più lento, ma l'anteprima corrisponde esattamente al risultato confermato.

### Opzioni di gruppo

- **Espandi area distorsione** (solo distorsione gruppo): il numero di pixel aggiunti come margine trasparente attorno all'anteprima del gruppo su tutti i lati. Ciò offre spazio ai contenuti spostati in cui spostarsi. I 256 px predefiniti sono sufficienti per la maggior parte dei lavori; ridurlo per immagini di grandi dimensioni in cui la memoria è importante o aumentarlo per tratti di spostamento molto grandi.
- **Maschere di distorsione**: se applicare la stessa distorsione alle maschere di livello e di gruppo. Attivo per impostazione predefinita.

## Annulla e ripristina

Ogni tratto è un passaggio di annullamento distinto all'interno della sessione di deformazione. **Ctrl+Z** rimuove l'ultimo tratto e ripristina la mappa di spostamento allo stato precedente. **Ctrl+Y** (o **Ctrl+Shift+Z**) lo riapplica. Puoi ripercorrere l'intera cronologia dei tratti prima di impegnarti.

Premendo **Esc** o cambiando strumento si eliminano tutti i tratti non salvati e si ripristinano i livelli al loro stato originale. Nessuna modifica viene scritta finché non si effettua un commit esplicito.

## Impegno

Fare clic sul pulsante **Conferma** (o premere **Invio**) per applicare la distorsione accumulata in modo distruttivo. Per gli orditi di gruppo, ciò avvia l'applicazione multistrato completa sopra descritta. La cronologia degli annullamenti per l'ordito impegnato è quindi una singola voce nello stack di annullamento dell'immagine, reversibile con lo standard **Modifica → Annulla**.