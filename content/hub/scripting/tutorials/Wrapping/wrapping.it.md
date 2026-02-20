---
title: "Avvolgimento"
type: docs
weight: 4
---
I comandi dello schema operano a un livello basso, il che significa che anche le attività semplici possono richiedere più passaggi. Tuttavia, questa granularità offre flessibilità, possiamo raggruppare i comandi in piccole funzioni riutilizzabili che fanno esattamente ciò di cui abbiamo bisogno. Il confezionamento non è un concetto in bianco e nero; può variare da semplici alias per comandi utilizzati di frequente a funzioni più complesse che gestiscono interi flussi di lavoro. A volte, un wrapper è solo una funzione di comodità per migliorare la leggibilità, mentre in altri casi si evolve in un'utilità completa che incapsula più operazioni.

### Perché le funzioni Wrap?

Ci sono diversi vantaggi chiave legati alle funzioni di avvolgimento:

- **Semplifica le attività ripetitive** – Invece di ripetere comandi di basso livello, inseriscili in una funzione di supporto e riutilizzala.
- **Migliora la leggibilità** – Fornire alle nostre funzioni racchiuse nomi chiari e descrittivi rende il nostro codice più facile da comprendere a colpo d'occhio.
- **Incapsula la complessità** – Invece di avere a che fare con lunghi e criptici elenchi di comandi, cicli profondamente annidati o istruzioni di messaggi complesse, possiamo suddividerli in funzioni di supporto più piccole e ben strutturate.
- **Migliora la manutenibilità** – Se la funzionalità principale di un comando cambia, dobbiamo aggiornare la nostra funzione inserita solo una volta, isolando i nostri plug-in dai dettagli di tali modifiche.
- **Incoraggia il riutilizzo del codice**: ogni helper diventa parte della tua libreria, velocizzando la scrittura e il debug degli script futuri.

Man mano che i tuoi plug-in crescono, i wrapper ti aiutano a mantenere leggibile la logica di base e a isolare i dettagli ripetitivi.

Un altro vantaggio del wrapper delle funzioni è la loro integrazione in un evidenziatore di sintassi come Visual Studio Code. Ciò migliora la leggibilità e la navigazione, rendendo gli script più chiari. In un plug-in che utilizza funzioni personalizzate, qualsiasi funzione evidenziata in verde conferma che fa riferimento correttamente alla nostra libreria.

Se mantieni la tua libreria di supporto, valuta la possibilità di aggiungere i nomi delle funzioni del tuo progetto all'evidenziazione della sintassi del tuo editor. Rende la navigazione e il refactoring più veloci.

Esempi:

### Seme casuale

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Anche se potremmo usare ***msrg-rand*** direttamente nel nostro codice, racchiuderlo in una funzione chiamata ***random-seed*** migliora la leggibilità. Dando alla funzione un nome chiaro e descrittivo, diventa più facile comprenderne lo scopo a colpo d'occhio.

Inoltre, definire ***random-seed*** come funzione autonoma ci consente di utilizzarlo ovunque nei nostri plug-in centralizzando l'implementazione in un'unica posizione. Se mai avessimo bisogno di cambiare il modo in cui viene generato il seme, dobbiamo solo aggiornare questa funzione, lasciando intatto il resto del nostro codice.

Ad esempio, se invece decidiamo di passare a ***casuale***:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

Il nome della funzione rimane lo stesso, garantendo che i nostri script continuino a funzionare senza modifiche. Questo approccio mantiene il nostro codice flessibile, gestibile e facile da leggere.

### Esportazione JPEG

La funzione di esportazione JPEG in Scheme è dotata di molti parametri, offrendo un controllo accurato sul modo in cui le immagini vengono salvate. Tuttavia, nella maggior parte dei casi, ci preoccupiamo solo di alcune impostazioni chiave, come il nome e la qualità del file. Per semplificare il processo, possiamo eseguire il wrapper della funzione.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

In questa funzione wrapper, la maggior parte delle opzioni di esportazione sono codificate, esponendo solo i parametri che probabilmente modificheremo: nome e qualità del file. Questo approccio migliora la leggibilità e semplifica il salvataggio delle immagini.Inoltre, se l'esportatore di Lumi dovesse cambiare in futuro, dovremo aggiornare solo questa funzione invece di modificare ogni script che esporta un JPEG.

### Utilizzo del wrapper

Per esportare un JPEG nei nostri plug-in, includiamo semplicemente la libreria e chiamiamo la nostra funzione personalizzata:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Ciò mantiene il nostro codice pulito, leggibile e adattabile consentendoci al tempo stesso di esportare JPEG in modo efficiente con il minimo sforzo.

### Sostituzione auto

La funzione ***car*** può essere criptica e soggetta a errori di scripting. È facile applicare erroneamente ***car*** a un vettore o a un elemento non presente nell'elenco, provocando un comportamento inaspettato. Per rendere il nostro codice più robusto e leggibile, possiamo racchiudere questa funzionalità in una funzione più sicura.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Questa funzione recupera in modo sicuro il primo elemento di un elenco o di un vettore fornendo allo stesso tempo avvisi utili quando vengono rilevati input non validi o vuoti. Utilizzando ***first-item*** invece di ***car***, riduciamo il rischio di errori accidentali e miglioriamo la chiarezza dei nostri script.

#### Perché utilizzare questo wrapper?

- **Previene i crash dello script** – Evita gli errori causati dall'applicazione di ***car*** a non elenchi.
- **Supporta sia elenchi che vettori** – Espande l'usabilità oltre i semplici elenchi.
- **Fornisce avvisi significativi** – Aiuta a eseguire il debug di problemi di input imprevisti.
- **Migliora la leggibilità** – Il nome della funzione trasmette chiaramente il suo scopo.

Incapsulando questa logica nel primo elemento, rendiamo i nostri plug-in più robusti e più facili da mantenere. Naturalmente, questo dipende dalle preferenze personali, potresti sentirti completamente a tuo agio nell'utilizzare direttamente le funzioni Car, Caar, Cadr e Scheme simili.

### Avvolgimento di una funzione racchiusa

Il wrapper di una funzione già incapsulata può migliorare ulteriormente la leggibilità e la manutenibilità. Ad esempio, quando lavoriamo con coppie di coordinate come ***pixel-coords (list 100 200)***, potremmo usare:

```scheme
(first-item pixel-coords)
```

per recuperare la coordinata ***x***. Tuttavia, sebbene funzionale, non è molto espressivo. Possiamo invece racchiudere ***primo elemento*** in una definizione più appropriata per rendere più chiaro il nostro intento.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Perché utilizzare questo approccio?

- **Migliora la chiarezza del codice** – Invece di utilizzare funzioni generiche di accesso agli elenchi, definiamo esplicitamente funzioni che ne descrivono lo scopo.
- **Migliora la manutenibilità** – Se la nostra rappresentazione delle coordinate cambia (ad esempio, utilizzando vettori invece di elenchi), dobbiamo solo aggiornare queste piccole funzioni.
- **Incoraggia la coerenza** – L'uso di ***x-coord*** e ***y-coord*** rende lo script più facile da leggere e comprendere a colpo d'occhio.

Ora, invece di scrivere in Schema generico:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Possiamo scrivere nel _nostro_ Schema:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Avvolgendo le funzioni di basso livello in nomi significativi, creiamo un modo più intuitivo di lavorare con i dati, riducendo confusione e potenziali errori.

### Wrapper spediti: l'utility Stdlib

Lumi fornisce un set di wrapper già pronti caricati automaticamente all'avvio, quindi sono disponibili in qualsiasi plug-in o nella Scheme Console senza alcuna chiamata `(load ...)`. Queste librerie — `common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` e `paths.scm` — sono costruite esattamente sullo stesso principio degli esempi precedenti: forniscono informazioni chiare nomi alle operazioni di basso livello, nascondere i boilerplate ripetitivi e fornire un unico posto per l'aggiornamento se il comando sottostante cambia.Ad esempio, `images.scm` fornisce `image-get-open-list` come wrapper leggibile attorno alla chiamata PDB non elaborata e `files.scm` espone aiutanti per la creazione del percorso che altrimenti richiederebbero catene `string-append` ripetute.

È possibile sfogliare ogni nome esportato, leggere la relativa docstring e vedere da quale libreria proviene in **[Utility Browser](@@LUMI_TOKEN_21@@)** (Guida → Programmazione → Browser delle utilità). Si tratta di una dimostrazione pratica del confezionamento su larga scala e di un'utile fonte di modelli da prendere in prestito quando si crea la propria libreria di supporto.

### Conclusione

Le funzioni di wrapper sono un modo potente per semplificare lo sviluppo di Scheme, rendendo gli script più leggibili, gestibili e robusti. Incapsulando la complessità ed esponendo solo i dettagli necessari, creiamo un approccio più strutturato alla scrittura dei plug-in.

Punti chiave di questo approccio:

- **Semplifica le attività ripetitive** – Invece di ripetere manualmente comandi di basso livello, creiamo funzioni riutilizzabili.
- **Migliora la leggibilità del codice** – I wrapper con nomi ben definiti rendono gli script più facili da comprendere.
- **Incapsula la complessità**: i dettagli di basso livello vengono gestiti all'interno del wrapper, mantenendo pulito lo script principale.
- **Migliora la manutenibilità** – Se la funzionalità principale cambia, dobbiamo aggiornare solo il wrapper, non tutti gli script che si basano su di esso.
- **Incoraggia il riutilizzo e la coerenza**: la nostra libreria personale di funzioni cresce nel tempo, rendendo lo sviluppo più rapido ed efficiente.

Utilizzando in modo coerente il function wraping, possiamo trasformare il modo in cui scriviamo i plug-in Scheme, creando un ambiente di scripting più modulare ed espressivo. Tenendo presenti questi principi, possiamo continuare a perfezionare il nostro approccio, sviluppando una versione dello Schema più efficiente e su misura che soddisfi le nostre esigenze specifiche.

Passaggi successivi: identifica i blocchi ripetuti nei tuoi script ed estrai piccoli aiutanti con nomi chiari.