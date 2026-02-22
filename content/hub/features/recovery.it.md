---
title: "Recupero file"
type: docs
---
Lumi mantiene due sistemi di ripristino indipendenti: salvataggi automatici in background e checkpoint incrementali manuali, entrambi accessibili da un'unica finestra di dialogo.

## Accesso

**File** → **Recupera immagine**

La finestra di dialogo si apre precompilata con gli stati di ripristino per il file attualmente aperto. Utilizza il selettore file in alto per passare a un file `.lum` diverso.

---

## Salvataggio automatico

Lumi salva un'istantanea in background del tuo lavoro a intervalli regolari durante la modifica. I salvataggi automatici vengono scritti in una **directory cache separata**, lasciando intatto il file `.lum` funzionante:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

La codifica del percorso utilizza `~` come separatore per creare una directory cache univoca per file. Ciò significa che i salvataggi automatici sono disponibili anche se il file di progetto stesso viene perso o danneggiato.

- **Frequenza**: configurabile in **Modifica** → **Preferenze** → **Prestazioni** → Intervallo di salvataggio automatico.
- **Posizione di archiviazione**: impostata anche in Preferenze → Prestazioni.
- **Scopo**: recupero da crash. La scheda Salvataggio automatico nella finestra di dialogo Recupera immagine mostra gli stati di salvataggio automatico disponibili con timestamp.

Quando apri un file che contiene dati di salvataggio automatico più recenti, Lumi ti avvisa all'apertura.

---

## Salvataggi incrementali

Il salvataggio incrementale è un sistema di checkpoint manuale memorizzato **all'interno del file di progetto** in `recovery/`. La struttura è:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Una nuova linea di base `primary-NN.lum/` viene scritta dopo **File → Salva**. Le successive pressioni di Ctrl+S creano sottodirectory `delta-NNNN.lum/` contenenti solo i buffer modificati dall'ultima linea di base. I delta di salvataggio automatico e quelli di salvataggio manuale utilizzano contatori separati in modo che non interferiscano reciprocamente con la cronologia.

I salvataggi incrementali sono **disabilitati per impostazione predefinita** e devono essere abilitati per progetto:

1. **File** → **Salva con nome** (Maiusc+Ctrl+S).
2. Nella finestra di dialogo Salva con nome, seleziona **Salvataggio incrementale** e facoltativamente imposta un limite di **Salvataggi massimi**.
3. L'impostazione viene memorizzata con il progetto e si applica a tutte le successive pressioni di Ctrl+S.

Quando apri un file `.lum` che contiene salvataggi incrementali più recenti rispetto al salvataggio primario, Lumi mostra un messaggio **Salvataggio incrementale rilevato** che offre di caricare il checkpoint più recente.

---

## Finestra di dialogo Recupera immagine

La finestra di dialogo ha tre schede e due pulsanti di azione.

### Scheda Salvataggio automatico

Elenca tutti gli stati di salvataggio automatico disponibili per il file selezionato con timestamp e miniature (ove disponibili). Seleziona uno stato e fai clic su **Ripristina** per aprirlo.

Utilizza questa scheda per:
- Recuperare dopo un incidente.
- Ripristina uno stato precedente dalla stessa sessione.

### Scheda incrementale

Elenca tutti gli stati dei checkpoint archiviati nel file di progetto. Ogni voce mostra il timestamp del checkpoint. Seleziona un checkpoint e fai clic su **Ripristina** per aprirlo.

Utilizza questa scheda per:
- Ritorna a un punto precedente di una sessione senza aver salvato file separati.
- Sfoglia la cronologia delle versioni di un progetto.

### Scheda più recente

La scheda predefinita all'apertura della finestra di dialogo. Identifica automaticamente lo stato di ripristino più recente disponibile sia nei salvataggi automatici che nei checkpoint incrementali e ne mostra il timestamp. Fai clic su **Ripristina** per caricarlo immediatamente senza sfogliare i singoli stati.

---

## Pulsanti

| Pulsante | Azione |
|--------|--------|
| **Recupera** | Apre lo stato di ripristino selezionato come una nuova immagine. |
| **Chiudi** | Chiude la finestra di dialogo senza ripristinare. |
| **Ripulisci i vecchi Stati…** | Apre un prompt di pulizia (vedi sotto). |

---

## Ripulisci i vecchi StatiL'accumulo di stati di ripristino nel tempo può consumare una quantità significativa di spazio su disco. Il pulsante **Ripulisci vecchi stati…** (in basso a sinistra della finestra di dialogo) apre una richiesta di pulizia per la scheda attiva (Salvataggio automatico o Incrementale).

Il messaggio mostra:
- Quanti salvataggi completi esistono per il file.
- Lo spazio totale su disco che occupano.
- Un pulsante di selezione **Conserva più recente** per selezionare il numero di salvataggi da conservare.

L'impostazione di **Mantieni più recente** su `0` elimina tutti gli stati di ripristino. Il successivo Ctrl+S dopo una pulizia completa scriverà un nuovo salvataggio primario.

---

## Ripristino all'avvio

All'avvio, se Lumi rileva che il file aperto più recentemente contiene dati di salvataggio automatico più recenti rispetto all'ultimo salvataggio completo, presenta una richiesta di ripristino prima del caricamento. Puoi accettare (caricare il salvataggio automatico) o ignorare (aprire il salvataggio principale normalmente).