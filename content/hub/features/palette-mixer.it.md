---
title: "Miscelatore di tavolozze"
type: docs
---
Il Mixer tavolozza ricava nuovi colori da coppie di voci della tavolozza utilizzando una pipeline fissa a tre fasi. Poiché la miscelazione avviene nel dominio spettrale anziché nell'RGB, i risultati si comportano come i pigmenti fisici: il blu e il giallo producono il verde, i colori saturi si spostano verso il neutro mentre si mescolano.

## Il gasdotto

Ogni colore prodotto dal Mixer attraversa tre fasi in un ordine fisso:

1. **Miscela**: WGM spettrale tra Genitore A (CCW) e Genitore B (CW).
2. **Croma**: sfuma verso lo spettro neutro della tavolozza, riducendo la saturazione.
3. **Tonalità**: sfumare verso la miscelazione del bianco (tinta) o la miscelazione del nero (tonalità).

Il tono viene sempre applicato per ultimo. Ciò rende la luminosità dominante: una regolazione del tono arriva esattamente al livello di luminosità desiderato senza essere diluita dalla regolazione della crominanza che la precede.

## Selezione dei genitori

Genitore A e Genitore B sono le due voci tra cui viene mescolato il cursore di fusione. Vengono caricati dalla Mappa tavolozza:

- Tieni premuto **Shift** sulla mappa della tavolozza e **fai clic con il pulsante sinistro del mouse** per impostare il genitore A (CCW).
- Tieni premuto **Shift** e **fai clic con il pulsante destro del mouse** per impostare Parent B (CW).

Solo le voci di **Classe A** (miscele primarie e personalizzate con provenienza intatta) sono accettate come genitori. Sono esclusi i terziari e gli ingressi con ascendenza perduta.

Le posizioni Genitore A e Genitore B del Mixer vengono mostrate sulla mappa come evidenziate con un **anello di diamante** in modo da poter sempre vedere quali voci sono caricate.

## Gli slider

| Cursore | Effetto |
| :--- | :--- |
| **Miscela** | Si sposta tra il genitore A (fine in senso antiorario) e il genitore B (fine in senso orario). A 0,0 il risultato corrisponde al Genitore A; a 1.0 corrisponde al genitore B. |
| **Croma** | Desatura la fusione verso il neutro della tavolozza. Valori più alti producono risultati più attenuati e terrosi. |
| **Tono** | Sposta la luminosità verso la miscelazione del bianco (direzione della tinta) o la miscelazione del nero (direzione dell'ombreggiatura). |

## Controlli dei valori

**Value Lock** blocca la leggerezza percettiva (CIE L\*) al livello attuale mentre gli altri cursori si muovono. Usalo per esplorare la variazione di crominanza o tonalità senza modificare il valore di un mix.

**Band Clamp** limita il risultato a rimanere entro i limiti della sua banda di valori corrente (ad esempio, entro i medi inferiori). Il cursore del tono è ancora trascinabile ma la luminosità dell'output è bloccata.

Il cursore Tono riflette anche eventuali Gap di valore configurati nell'Editor tavolozza. Gli intervalli di luminosità che rientrano in uno spazio vuoto vengono visualizzati come bande grigie semitrasparenti sul cursore. La maniglia del dispositivo di scorrimento salta automaticamente su questi spazi vuoti: trascinando attraverso una regione grigia si passa al confine di banda valido più vicino sull'altro lato.

## Mixing endpoint (bianco, nero, neutro)

Le fasi di tono e crominanza richiedono punti finali di riferimento: una miscelazione del bianco, una miscelazione del nero e una neutralità. Lumi li scopre automaticamente cercando nella tavolozza attiva i migliori candidati:

- **Mixing White**: il primario con il croma più elevato, più vicino al bianco puro.
- **Mixing Black**: il primario con la luminosità più bassa.
- **Neutro**: il Primario più vicino all'acromatico (croma più basso).

Questi possono essere sovrascritti manualmente facendo clic con il pulsante destro del mouse su una voce nell'editor della tavolozza.

## Salvataggio di un mixFare clic su **Aggiungi alla tavolozza** per salvare il risultato corrente del mixer come **Mix salvato** (voce personalizzata). Prima di salvare, il sistema applica il **Riposizionamento Best-Match**: cerca nella tavolozza la ricetta ottimale che produca lo stesso colore finale con il miglior adattamento spaziale sulla Mappa tavolozza. Se viene trovata una ricetta più vicina, i cursori del mixer salteranno per rifletterla, confermando che il sistema ha trovato un'origine migliore e la posizione della voce salvata si allineerà con il suo punto visivo sulla mappa.

I mix salvati memorizzano la loro ricetta completa (UID genitore A/B, fattore di fusione, tono, crominanza) in modo che possano essere riprodotti esattamente.

## Recupero ricetta

Facendo clic una volta su una voce personalizzata nell'editor della tavolozza si ripristina la ricetta di quella voce nel Mixer:

- Il genitore A e il genitore B vengono ricaricati.
- I cursori di fusione, tono e crominanza ritornano alle loro posizioni originali.
- Qualsiasi Value Lock o Band Clamp che era attivo durante la creazione viene riattivato.

Ciò rende semplice tornare a un colore e regolarlo ulteriormente o utilizzarlo come punto di partenza per una nuova miscela.