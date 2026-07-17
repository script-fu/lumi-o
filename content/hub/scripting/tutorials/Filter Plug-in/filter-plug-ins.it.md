---
title: "Il plugin filtro"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e8eb69ed9dff7c65cc926ba4bfb4c333fdd8baa3832aa92765ba6bb19b17516d
---
Abbiamo utilizzato un plug-in _procedure_ per il tutorial [Primo passaggio](../../first-step/). Questi tipi di plug-in funzionano senza bisogno di un'immagine o di un elemento disegnabile come input. Di solito, utilizziamo un plug-in per modificare un'immagine e i suoi disegni. Plug-in come questi sono chiamati plug-in _filter_.

### Cos'è un Drawable?

Un **disegnabile** in Lumi si riferisce a un elemento dell'immagine su cui è possibile disegnare, come un livello o un canale. I plug-in di filtro in genere funzionano su questi elementi.

### Un semplice esempio di plug-in di filtro

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Usare un'istruzione let per definire una variabile messaggio e il codice principale
  (let ((message "hello, world"))
    ;; Visualizzare il messaggio nella Error Console di Lumi
    (lumi-message message)
    ;; Invertire i colori del primo drawable selezionato
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Registra il plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Nome della procedura principale
  "Simple Filter Plug-in Demo"             ;; Il nome come appare nel menu di Lumi
  "Tests a basic Scheme filter plug-in"    ;; Descrizione del suggerimento
  "Author Name"                            ;; Dai un po' di merito a te stesso
  "License"                                ;; Licenza
  "Date written"                           ;; Data di scrittura
  "*"                                      ;; Indica che questo plug-in richiede un'immagine
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Richiede uno o più drawable selezionati

;; Specificare la posizione nel menu per il plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Copia il testo e salvalo come `simple-filter-plug-in.scm` in una cartella denominata `simple-filter-plug-in` all'interno di una delle cartelle dei plug-in di Lumi. Una cartella di plug-in Lumi è _qualsiasi_ cartella elencata in:
 **Lumi > Modifica > Preferenze > Cartelle > Plug-in**

In Linux, fare clic con il pulsante destro del mouse sul file `simple-filter-plug-in.scm`, andare su **Proprietà > Autorizzazioni** e selezionare **Consenti l'esecuzione del file come programma**. Una volta che il file è nel posto giusto, eseguibile e privo di errori di sintassi, al riavvio di Lumi apparirà nella barra di intestazione del menu in alto, all'interno di un menu chiamato **Plug-in**.

### Esecuzione del plug-in

1. Apri un'immagine (questo plug-in del filtro richiede un'immagine per funzionare).
2. Aprire **Strumenti > Debug > Console messaggi** per visualizzare un messaggio.
3. Selezionare **Demo plug-in filtro semplice** dal menu **Plug-in**.
4. Uno dei livelli selezionati avrà i colori invertiti e verrà stampato un messaggio sulla console degli errori.

### Modifica del plug-in

È possibile personalizzare il plug-in modificando il relativo file `.scm`. Ad esempio, per modificare il messaggio visualizzato:

1. Aprire il file e individuare la riga che definisce `message`.
2. Sostituisci `"hello, world"` con il testo personalizzato.
3. Salvare il file.

Nella versione 3 di Lumi, i plug-in non necessitano di aggiornamento affinché le modifiche salvate abbiano effetto. È sufficiente eseguire nuovamente il plug-in per visualizzare il messaggio aggiornato.

### Esame dei plug-in

#### Linea Shebang

La prima riga garantisce che lo script funzioni come plug-in in Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Definizione della procedura

La procedura accetta due argomenti: l'immagine attiva e i drawable selezionati.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Logica fondamentale

Un'istruzione `let` definisce una variabile ed esegue operazioni sul drawable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Mostra un messaggio nella Error Console di Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverte i colori del primo drawable selezionato
```

### Registrazione del plug-in

Il plug-in è registrato con Lumi come plug-in filtro:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Registra la procedura principale
  "Simple Filter Plug-in Demo"             ;; Il nome come appare nel menu di Lumi
  "Tests a basic Scheme filter plug-in"    ;; Descrizione del suggerimento
  "Author Name"                            ;; Nome dell'autore
  "License"                                ;; Tipo di licenza
  "Date written"                           ;; Data di scrittura
  "*"                                      ;; Indica che il plug-in richiede un'immagine
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Richiede uno o più drawable selezionati
```

#### Registrazione nel menu

Questa riga specifica la posizione del menu per il plug-in:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Risoluzione dei problemi

Se un plug-in non viene visualizzato, controllane la posizione, il nome e la proprietà eseguibile.

La posizione deve trovarsi in un percorso di ricerca del plug-in.
Il nome del file deve corrispondere al nome della cartella che lo contiene.
Il file deve essere impostato come eseguibile.


La **Console dei messaggi** è uno strumento prezioso per la risoluzione dei problemi relativi ai plug-in personalizzati. Se il tuo plug-in non si comporta come previsto, controlla qui i messaggi di errore o i registri. La finestra **Terminale** può anche fornire informazioni di debug e segnalare problemi di caricamento.