---
title: "Spazi di lavoro"
type: docs
---
Un'area di lavoro è un'istantanea salvata dell'intero ambiente dell'interfaccia utente: quali pannelli sono aperti e dove, le decorazioni e il riempimento della tela sia per la visualizzazione normale che per quella a schermo intero, il tema attivo e il set di icone, il layout della casella degli strumenti, la tavolozza attiva e le impostazioni dello strumento. Lumi ti consente di salvare tutte le aree di lavoro con nome che desideri e di passare da una all'altra istantaneamente: tutte le immagini aperte si aggiornano sul posto, senza bisogno di riavviare.

## Cosa fa risparmiare uno spazio di lavoro

Ciascuna area di lavoro denominata memorizza quanto segue in modo indipendente:

| Componente | Cosa copre |
| :--- | :--- |
| **Disposizione** | Posizione e dimensioni della finestra, disposizione del dock (colonne dei pannelli sinistro e destro, quali pannelli sono aperti e in quale ordine), modalità finestra singola o multifinestra, stato ingrandito, visibilità e posizione della barra delle schede |
| **Opzioni strumento** | Le impostazioni attuali per ogni strumento (dimensione del pennello, durezza, comportamento di deformazione, ecc.) |
| **Dispositivi di input** | Configurazione del dispositivo di input: curve di pressione, assegnazioni dei pulsanti, mappature degli assi per stilo e altri dispositivi |
| **Decorazioni su tela** | Impostazioni predefinite per area di lavoro per righelli, barre di scorrimento, guide, griglia, evidenziazione della selezione, limiti del livello e limiti della tela — impostate tramite **Preferenze → Finestre immagine → Aspetto predefinito** e **Aspetto a schermo intero**, indipendentemente per la visualizzazione normale e a schermo intero |
| **Imbottitura in tela** | Modalità di riempimento e colore per area di lavoro per la visualizzazione normale e a schermo intero: impostabili tramite **Preferenze → Finestre immagine → Aspetto predefinito** |
| **Tema e icone** | Tema attivo, variante colore scuro/chiaro, set di icone, sostituzione della dimensione dell'icona e scala del carattere |
| **Cassetta degli attrezzi** | Posizione widget FG/BG (alto/basso/sinistra/destra), scala FG/BG, visibilità della mascotte Wilber, intestazioni dei gruppi di strumenti |

Anche la **tavolozza** e gli **strumenti preimpostati** attivi vengono registrati per area di lavoro e ripristinati quando si cambia.

> **Le decorazioni e l'imbottitura della tela** sono controllate da
> **Preferenze → Finestre immagine → Opzioni finestra avanzate → Aspetto predefinito** (Visualizzazione normale)
> e **Aspetto a schermo intero** (visualizzazione a schermo intero). Regola queste impostazioni a tuo piacimento,
> quindi salva lo spazio di lavoro. Le voci del **menu Visualizza** (righelli, guide, ecc.) sono locali nel file
> finestra dell'immagine corrente e non vengono salvati per area di lavoro.

### Aggiornamenti in tempo reale su Switch

Quando cambi area di lavoro, tutte le finestre delle immagini aperte si aggiornano immediatamente: righelli, guide, barre di scorrimento, colore di riempimento e ogni altra impostazione di visualizzazione cambiano senza dover chiudere e riaprire le immagini.

## Accesso

**Modifica → Preferenze → Area di lavoro**

La sezione superiore della pagina delle preferenze dell'area di lavoro elenca tutte le aree di lavoro salvate e fornisce i controlli per gestirle.

## Creazione di uno spazio di lavoro

Imposta i pannelli, gli strumenti e la tavolozza esattamente come li desideri, quindi:

1. Apri **Modifica → Preferenze → Area di lavoro**.
2. Fare clic su **Salva layout con nome…**.
3. Inserisci un nome e fai clic su **Salva**.

La nuova area di lavoro viene visualizzata nel menu a discesa **Layout attivo** e nel menu **Finestre**.

## Cambio di area di lavoro

Esistono due modi per cambiare:

- **Menu Windows**: i nomi dei layout vengono visualizzati in **Finestre → Layout** per un accesso rapido dall'area di disegno.
- **Preferenze → Area di lavoro**: seleziona un layout dal menu a discesa **Layout attivo** e fai clic su **Ricarica layout**.

Il passaggio è immediato: Lumi ricostruisce il layout del pannello, ripristina le opzioni degli strumenti, ricarica le impostazioni del dispositivo, aggiorna le decorazioni della tela, l'imbottitura, il tema e il layout della casella degli strumenti, il tutto senza riavviare.

## Gestione degli spazi di lavoro

Da **Modifica → Preferenze → Area di lavoro**:| Azione | Effetto |
| :--- | :--- |
| **Salva layout** | Sovrascrive l'area di lavoro corrente con le impostazioni attuali. |
| **Salva layout con nome…** | Crea una nuova area di lavoro denominata dalle impostazioni attuali. |
| **Rinomina layout…** | Rinomina l'area di lavoro selezionata. |
| **Ricarica layout** | Applica immediatamente l'area di lavoro selezionata. |
| **Elimina layout…** | Rimuove permanentemente l'area di lavoro selezionata e i relativi file. |

## Impostazioni di persistenza

La parte inferiore della pagina delle preferenze dell'area di lavoro controlla ciò che Lumi salva automaticamente:

- **Salva le posizioni delle finestre all'uscita**: quando è attivo, le posizioni del dock e delle finestre vengono scritte sul disco ogni volta che esci.
- **Apri finestre sullo stesso monitor**: riapre ogni finestra sul monitor su cui era accesa durante l'ultima sessione.
- **Salva opzioni strumento all'uscita**: salva le impostazioni attuali dello strumento all'uscita.
- **Salva le impostazioni del dispositivo di input all'uscita**: salva la configurazione dello stilo e del dispositivo all'uscita.

Queste impostazioni si applicano all'area di lavoro: ogni layout mantiene il proprio stato salvato in modo indipendente.

## Esempi di flussi di lavoro

Alcuni modi in cui gli artisti potrebbero utilizzare più spazi di lavoro:

- **Pittura**: ampie aree di posizionamento dei pennelli, colore di riempimento caldo (impostato in Preferenze → Finestre immagine → Aspetto predefinito), la variante del tema preferita
- **Inchiostrazione**: guide e bordi del quadro attivati, barre di scorrimento attivate (impostate in Preferenze → Aspetto predefinito), colore di riempimento neutro
- **Roughs**: dock nascosti, senza righelli o griglia, imbottitura scura, dimensioni compatte delle icone per massimizzare lo spazio sulla tela
- **Messa a fuoco a schermo intero**: diverse impostazioni di colore di riempimento e decorazione nell'aspetto a schermo intero rispetto a quello predefinito, quindi la commutazione a schermo intero offre un ambiente di lavoro veramente diverso
- **Scripting**: pannello di scripting aperto, dimensione del carattere aumentata per la leggibilità, set di icone diverso