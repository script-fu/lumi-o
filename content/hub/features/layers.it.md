---
title: "Livelli e modifica non distruttiva"
type: docs
---
Il sistema a livelli di Lumi consente flussi di lavoro complessi e non distruttivi con il pieno controllo su fusione, mascheramento e composizione.

## Panoramica

I livelli sono il fondamento dell'illustrazione strutturata. Ogni livello è indipendente, con la propria modalità di fusione, opacità e maschera di livello opzionale. I gruppi possono annidare i livelli gerarchicamente con le proprie proprietà di fusione e ritaglio.

## Accesso

**Pannelli** → **Livelli** o il pannello **Livelli** predefinito sulla destra.

## Tipi di livelli

### Strati di pittura
Livelli raster standard per contenuto dipinto. Memorizza i dati dei pixel come buffer GEGL con trasparenza alfa opzionale.

### Raggruppa livelli
Contenitori gerarchici per organizzare livelli correlati. I gruppi possono avere la propria modalità di fusione, opacità e maschere di ritaglio. Le proiezioni di gruppo sono composte su richiesta.

### Maschere di livello
Maschere in scala di grigi collegate a qualsiasi livello, che controllano l'opacità per pixel. Dipingere su una maschera con il bianco rende i pixel opachi; il nero li rende trasparenti; il grigio fornisce un'opacità parziale.

## Modalità di fusione

Ogni livello ha una modalità di fusione che determina come si combina con i livelli sottostanti:

- **Normale**: fusione diretta dell'opacità.
- **Moltiplica**: scurisce moltiplicando i valori del colore.
- **Schermo**: schiarisci invertendo, moltiplicando e invertendo di nuovo.
- **Sovrapposizione**: combinazione di moltiplicazione e schermo.
- **Aggiungi**: fusione additiva (somma dei valori di colore).
- **Sottrai**: fusione sottrattiva.
- **Colore, Tonalità, Saturazione, Luminosità**: fusione dei componenti HSL.

## Ritaglio e mascheramento

- **Modalità composita - Ritaglia sullo sfondo**: l'impostazione della modalità composita di un livello su **Ritaglia sullo sfondo** limita la composizione alle aree in cui i livelli accumulati di **Unione** sottostanti hanno stabilito l'opacità. Il livello dipinge solo dove tali livelli hanno contenuto: non può espandere l'impronta alfa. Questo viene impostato per livello nella finestra di dialogo Attributi livello (elenco a discesa **Modalità composita**). Quando la modalità composita effettiva di un livello è diversa da Unione, l'icona a forma di occhio nel pannello Livelli viene sostituita con un'icona composita per indicare il comportamento di composizione non standard.

  **Esempio: forma alfa condivisa:** In un gruppo, il livello inferiore contiene un cerchio pieno su uno sfondo trasparente, impostato sulla modalità composita **Unione** predefinita. Ogni livello sopra di esso nello stesso gruppo è impostato su **Ritaglia sullo sfondo**. Questi livelli possono dipingere solo dove il cerchio fornisce opacità: una forma, molti livelli. Questo è un modello comune per colorare, ombreggiare e dettagliare all'interno di una silhouette definita senza preoccuparsi di fuoriuscite.
- **Maschere di livello**: applica una maschera in scala di grigi per controllare la visibilità del livello pixel per pixel. Dipingere il bianco sulla maschera rivela; il nero nasconde; il grigio fornisce un'opacità parziale.
- **Maschere Pure-Child**: le maschere vengono archiviate come figli all'interno dello stack disegnabile, prevenendo la perdita di dati durante le trasformazioni.

## Selezione dei livelli (tasto Alt)

Toccando **Alt** (Alt sinistro) mentre si passa con il mouse sulla tela si seleziona il livello con i pixel visibili sotto il cursore, senza cambiare strumento o fare clic.

### Come funziona

- **Premi Alt**: il cursore si trasforma in un mirino, indicando che la modalità di prelievo è attiva.
- **Rilascia Alt**: Lumi seleziona il livello non trasparente più in alto nella posizione del cursore (opacità > 25%) e lo seleziona. Il livello è evidenziato nel pannello Livelli e la barra di stato mostra **"Livello selezionato: 'nome livello'"**.
- Viene disegnata una maniglia nel punto centrale del livello selezionato sulla tela. La maniglia diminuisce e svanisce man mano che il cursore si allontana.

### Passare in rassegna i livelliOgni successivo tocco Alt nella stessa posizione seleziona il **livello successivo in basso** nello stack in quel punto. Lumi ricorda l'ultimo livello selezionato e passa a quello sottostante. Una volta raggiunto il fondo dello stack, il tocco successivo torna allo strato più in alto in quella posizione. Ciò rende semplice raggiungere i livelli nidificati in scene complesse toccando ripetutamente Alt.

### Regole di cancellazione

La scelta viene annullata (non viene attivata al rilascio di Alt) se si verifica una delle seguenti condizioni mentre si tiene Alt:

- Viene premuto un pulsante del mouse (clic sinistro o destro).
- Viene premuto qualsiasi altro tasto.

Ciò garantisce che i gesti di trascinamento Alt (come la regolazione della dimensione del pennello) e le scorciatoie modificate con Alt funzionino senza modificare accidentalmente il livello attivo.

### Limitazioni

- La selezione dei livelli non si attiva durante le operazioni dello strumento **Trasforma**: Alt ha un significato diverso in questo caso.
- La selezione non avviene se è presente una selezione mobile.
- Solo l'Alt sinistro attiva la raccolta; destra Alt viene trattato come un modificatore standard.

## Operazioni

Nel pannello Livelli:

- **Crea livello**: fai clic con il pulsante destro del mouse → **Nuovo livello** o utilizza il menu **Livello**.
- **Duplica**: fai clic con il pulsante destro del mouse → **Duplica** o **Livello** → **Duplica**.
- **Elimina**: fai clic con il pulsante destro del mouse → **Elimina** oppure seleziona e premi **Elimina**.
- **Riordina**: trascina i livelli verso l'alto o verso il basso per modificare l'ordine di sovrapposizione.
- **Rinomina**: fai doppio clic sul nome del livello.
- **Unisci giù**: fai clic con il pulsante destro del mouse → **Unisci giù** per combinarlo con il livello sottostante.
- **Appiattisci immagine**: **Immagine** → **Appiattisci immagine** per unire tutti i livelli visibili.

## Proprietà del livello

- **Opacità**: 0–100%, controlla la trasparenza complessiva del livello.
- **Modalità di fusione**: menu a discesa per selezionare il modo in cui il livello si combina con i livelli sottostanti.
- **Visibile/Nascosto**: l'icona a forma di occhio attiva/disattiva la visibilità del livello.

## Blocchi livello

Le icone di blocco vengono visualizzate nella riga di intestazione del pannello Livelli. Ogni blocco può essere attivato in modo indipendente. Facendo clic con il pulsante destro del mouse sull'icona di un lucchetto la si imposta in modo esclusivo (blocca solo quel tipo, sbloccando tutti gli altri sullo stesso livello).

- **Blocca alfa**: impedisce di dipingere su aree trasparenti. I tratti del pennello influiscono solo sui pixel che hanno già opacità; i pixel completamente trasparenti non vengono modificati. Utile per dipingere all'interno di forme esistenti senza fuoriuscire all'esterno.

- **Blocca maschera**: impedisce la modifica della maschera di livello. La maschera rimane visibile e attiva ma non può essere dipinta o modificata mentre questo blocco è attivo.

- **Blocca colore**: blocca il dipinto su un colore specifico: il colore di primo piano corrente nel momento in cui viene applicato il blocco. I tratti successivi su questo livello utilizzano il colore memorizzato indipendentemente dal colore di primo piano attivo. Lo sblocco elimina il colore memorizzato.

- **Blocca contenuto** (Blocca pixel): impedisce tutte le modifiche dei pixel al livello. Il livello non può essere dipinto, riempito, trasformato o modificato in altro modo. Utile per proteggere gli strati finiti.

- **Blocca posizione**: impedisce che il livello venga spostato o trasformato. Il livello può ancora essere modificato; vengono bloccate solo le modifiche di posizione (strumento Sposta, strumento Trasforma).

- **Blocca visibilità**: impedisce all'icona a forma di occhio di attivare/disattivare la visibilità del livello. Protegge i livelli che dovrebbero rimanere sempre visibili (o nascosti) durante la modifica.

Tutti i blocchi vengono salvati con il progetto e persistono tra le sessioni.

## Effetti di livello (fx)

I filtri GEGL non distruttivi applicati tramite il menu **Filtri** vengono memorizzati come effetti applicati sul livello anziché modificare immediatamente i pixel. Quando un livello ha almeno un effetto confermato, viene visualizzata un'icona **fx** nel pannello Livelli accanto a quel livello.### Accesso al popup degli effetti

Fai clic sull'icona **fx** su una riga di livello nel pannello Livelli per aprire il popover **Effetti di livello** per quel livello.

Il popover mostra lo stack di filtri per il livello: ogni effetto impegnato è elencato per nome con un interruttore di visibilità accanto.

### Controlli

- **Attiva/disattiva visibilità occhio** (parte superiore del popup): attiva o disattiva tutti gli effetti contemporaneamente.
- **Attiva/disattiva visibilità per filtro**: ogni riga di filtro ha la propria icona a forma di occhio per abilitare o disabilitare l'effetto in modo indipendente.
- **Modifica**: apre la finestra di dialogo delle impostazioni per il filtro selezionato, consentendo la regolazione dei suoi parametri in modo non distruttivo.
- **Alza/Abbassa**: sposta il filtro selezionato verso l'alto o verso il basso nello stack, modificando l'ordine in cui vengono applicati gli effetti.
- **Unisci**: applica tutti gli effetti attualmente visibili ai pixel del livello, rendendo le modifiche permanenti. L'icona fx viene rimossa se tutti gli effetti vengono uniti. L'unione non è disponibile sui livelli di gruppo.
- **Rimuovi**: elimina completamente il filtro selezionato. Il popover si chiude automaticamente se non rimangono effetti.

Facendo doppio clic su un filtro nell'elenco si apre anche la relativa finestra di modifica.

**Modifica** e **Rimuovi** sono bloccati se Blocca pixel è attivo sul livello. Non è possibile riordinare i filtri mentre uno è attivamente in fase di modifica.

### Aggiunta di effetti

Applica un filtro da **Filtri** → (qualsiasi categoria). Se viene preso di mira il livello attivo e l'operazione viene eseguita in modo non distruttivo, il risultato viene archiviato come effetto di livello anziché inserito nei dati dei pixel. L'icona fx appare sul livello quando è presente almeno un effetto.

## Finestra di dialogo Attributi layer

Fare doppio clic su un livello nel pannello Livelli per aprire la finestra di dialogo Attributi livello.

### Identità

- **Tag colore**: etichetta colore per l'organizzazione visiva nel pannello Livelli.

### Spazio e modalità compositi

- **Spazio composito**: lo spazio colore utilizzato durante la composizione di questo livello con i livelli sottostanti. Opzioni: Automatico, Lineare (RGB), Percettivo (RGB).
- **Modalità composita**: controlla il modo in cui il livello alfa interagisce con lo sfondo. Le opzioni includono Unione (influenza tutte le aree, impostazione predefinita per la modalità Normale), Ritaglia su sfondo (influenza solo aree con contenuto esistente, impostazione predefinita per la maggior parte delle altre modalità di fusione) e Intersezione.

### Dimensioni e offset

Per un livello esistente, **Dimensioni** mostra le dimensioni del livello e le dimensioni della maschera (se è allegata una maschera) come etichette di sola lettura.

**Offset livello**: ruote X e Y che controllano la posizione del livello sull'area di disegno. Le modifiche vengono applicate immediatamente anziché alla chiusura della finestra di dialogo.

Se il livello ha una maschera, di seguito vengono mostrati **Offset maschera**: gli indicatori X e Y per la posizione indipendente della maschera.

Quando si crea un nuovo livello, i campi Larghezza e Altezza e il menu a discesa **Riempi con** (Primo piano, Sfondo, Bianco, Trasparente) sostituiscono la visualizzazione delle dimensioni di sola lettura.

### Attributi del livello (parassiti persistenti)

La sezione inferiore della finestra di dialogo contiene una tabella Nome/Valore scorrevole per i parassiti persistenti: metadati con valori-chiave arbitrari allegati al livello. Questi valori vengono memorizzati con il progetto e sono accessibili dall'interfaccia di scripting di Scheme.

- Fai clic su qualsiasi cella nella colonna Nome o Valore per modificarla in linea.
- **Aggiungi**: aggiunge una nuova riga vuota.
- **Elimina**: rimuove la riga selezionata e il suo parassita dal livello.

Se lo strato non presenta parassiti persistenti, vengono mostrate tre righe iniziali vuote.

### Stato del contenutoUna riga informativa di sola lettura nella parte inferiore mostra lo stato corrente del contenuto del livello (e della maschera, se presente): **Cancella**, **Uniforme** o **Misto**. Un prefisso `*` indica che il livello presenta modifiche non salvate dall'ultimo salvataggio.

## Prestazioni

- **Modalità veloce**: quando si dipinge su un singolo livello nidificato all'interno di un gruppo, Lumi passa temporaneamente ai gruppi antenati al rendering pass-through per la durata del tratto, saltando la ricomposizione dell'intera proiezione del gruppo. Ciò elimina il ritardo nell'aggiornamento delle proiezioni nidificate durante l'inchiostrazione e la pittura. La composizione completa riprende quando il tratto termina, il livello attivo cambia o prima di un salvataggio.

  La modalità veloce è disabilitata quando a un gruppo antenato si applica una delle seguenti condizioni:
  - Il gruppo dispone di filtri visibili non distruttivi (i filtri necessitano del buffer di proiezione).
  - La modalità di fusione del gruppo è diversa da **Normale** o **Pass-through**.
  - Il gruppo ha un figlio diretto che utilizza la modalità composita **Clip to Backdrop** o **Intersection** (richiedono dati sullo sfondo dal buffer di proiezione).

  Inoltre, la modalità veloce non si attiva per i livelli di livello superiore, le selezioni mobili o quando vengono presi di mira più livelli contemporaneamente.

  La struttura dei file in modo da evitare queste condizioni nei gruppi di pittura, utilizzando le modalità di fusione Normale sui livelli, garantisce che la modalità veloce rimanga attiva durante una sessione di inchiostrazione o pittura.
- **Caricamento lento**: i progetti di grandi dimensioni vengono caricati rapidamente; i dati del livello vengono caricati solo quando necessario (ad esempio, quando resi visibili o dipinti).

## Formato file

Tutti i livelli, le maschere e le proprietà sono archiviati nel formato aperto `.lum` di Lumi. Il file è una directory contenente buffer e metadati di singoli livelli, garantendo compatibilità e accessibilità a lungo termine.