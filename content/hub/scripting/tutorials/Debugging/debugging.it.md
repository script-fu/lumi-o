---
title: "Debug"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: bd5eaf8ed491a7a74b7e4bcd130ed5177cfb15be41526bb6aefdfa0fb2a2428f
url: "hub/scripting/tutorials/debugging"
---
Nello scripting, nessuna funzione è infallibile. Anche i comandi più affidabili possono fallire di fronte a input o condizioni imprevisti. Per salvaguardarci da ciò, possiamo implementare un sistema di debug personalizzato e adottare tecniche di programmazione difensive. Integrando le funzioni standard con meccanismi di gestione degli errori e fornendo feedback informativo, possiamo rendere i nostri script più robusti e più facili da risolvere.

Una parte fondamentale di questa strategia è l'utilizzo di un flag di debug globale per controllare l'output dettagliato, consentendoci di abilitare informazioni di debug dettagliate quando necessario mantenendo l'output pulito durante la normale esecuzione.

## Flag di debug globale

Un flag di debug globale è un modo semplice ma efficace per controllare il livello di output delle informazioni durante l'esecuzione dello script. Se abilitato, fornisce messaggi di debug dettagliati che possono essere preziosi per individuare i problemi. Quando disabilitato, mantiene l'output conciso per l'uso in produzione.

```scheme
;; Scopo: Flag globale per controllare l'output di debug.
(define debug #f)
```

Per impostazione predefinita, il debug è disattivato. Per abilitare l'output dettagliato durante lo sviluppo, imposta semplicemente il flag su `#t`:

```scheme
;; Scopo: Flag globale per controllare l'output di debug.
(define debug #t)
```

Possiamo anche abilitare o disabilitare temporaneamente il debug per sezioni specifiche di codice utilizzando le funzioni di supporto.

### Controllo debug locale

Per un controllo più preciso, possiamo attivare o disattivare il debug all'interno di parti specifiche dello script utilizzando le funzioni di supporto.

```scheme
;; Scopo: Disattivare la modalità debug per una sezione di codice.
(define (debug-off)
  (set! debug #f))

;; Scopo: Attivare la modalità debug per una sezione di codice.
(define (debug-on)
  (set! debug #t))
```

Questo ci consente di controllare il debug in modo dinamico:

```scheme
(debug-on)  ;; Abilita l'output dettagliato

;; Qualche logica di script qui

(debug-off) ;; Disabilita l'output dettagliato
```

## Debug del sistema di messaggistica

Per gestire in modo efficiente l'output di debug in Scheme, utilizziamo un approccio strutturato che coinvolge più funzioni di supporto. Queste funzioni garantiscono che i messaggi di debug e di avviso siano chiari, leggibili e gestibili.

### Panoramica del sistema di messaggistica di debug

Il nostro sistema di messaggistica di debug è costituito dai seguenti componenti:

1. `debug-message` – Visualizza i messaggi di debug quando il debug è abilitato.
2. `serialize-item` – Converte vari tipi di dati Scheme in una rappresentazione di stringa.
3. `concat` – Concatena più elementi in un'unica stringa.
4. `list->string` – Formatta un elenco in una stringa leggibile.
5. `message` – Visualizza l'output nella console dei messaggi di Lumi.
6. `warning-message` – Visualizza i messaggi di avviso quando gli avvisi sono abilitati.

Ciascuna funzione svolge un ruolo nella formattazione e nella visualizzazione dei messaggi strutturati.

---

### Funzione messaggio di debug

La funzione `debug-message` è il metodo principale per visualizzare l'output di debug. Garantisce che i messaggi vengano visualizzati solo quando il debug è abilitato.

```scheme
;; Scopo: Visualizzare un messaggio di debug.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- La condizione `when debug` garantisce che i messaggi vengano visualizzati solo quando il debug è abilitato.
- I messaggi hanno il prefisso `"> "` per chiarezza.
- La funzione utilizza `concat` per formattare il contenuto del messaggio.
- Infine, chiama `message` per inviare l'output alla console dei messaggi di Lumi.

Utilizzo di esempio:

```scheme
;; Scopo: Restituisce la posizione ad albero dell'elemento o #f se l'elemento non è valido
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Con il debug abilitato, l'output potrebbe essere:

```scheme
> item: background-layer has tree position : 3
```

### Serializzazione dei dati per i messaggi di debug

I messaggi possono contenere diversi tipi di dati come elenchi, vettori e numeri. Per garantire che siano formattati correttamente, utilizziamo `serialize-item`.

```scheme
;; Scopo: Converte vari tipi di dati Scheme (liste, vettori, coppie, ecc.)
;;          in una rappresentazione stringa.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Elenco vuoto
    ((and (string? item) (string=? item "")) "\"\"")  ; Stringa vuota
    ((list? item) (list->string item))                ; Elenco annidato
    ((vector? item)                                   ; Gestisce i vettori
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Gestisce le coppie
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numeri
    ((symbol? item) (symbol->string item))            ; Simboli
    ((boolean? item) (if item "#t" "#f"))             ; Valori booleani
    ((string? item) item)                             ; Stringhe
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Utilizzo di esempio:

```scheme
(serialize-item '(1 2 3))
```

Uscita:

```scheme
list:
1
2
3
```

### Concatenazione per i messaggi

Per unire più componenti del messaggio in un'unica stringa, utilizziamo `concat`.

```scheme
;; Scopo: Concatenare più elementi in un'unica stringa.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Utilizzo di esempio:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formattazione degli elenchi come stringhe

La funzione `list->string` converte un elenco in una stringa formattata.

```scheme
;; Scopo: Convertire un elenco di elementi in una stringa leggibile.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Messaggi di avviso

La funzione `warning-message` funziona in modo simile a `debug-message`, ma visualizza avvisi anche quando il debug è disabilitato.

```scheme
;; Scopo: Visualizzare un messaggio di avviso.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Garantisce che i messaggi vengano visualizzati solo quando gli avvisi sono abilitati (il flag `warning` è impostato in `common.scm` come `#t`).
- Chiama `concat` per formattare il contenuto del messaggio.
- Utilizza `message` per inviare l'output a Lumi.

## Miglioramento delle funzioni standard

Una volta predisposto un sistema di debug, possiamo migliorare la nostra libreria di funzioni incorporando messaggi dettagliati. Ciò fornisce informazioni dettagliate sugli stati degli elementi, sui valori delle variabili e sulle chiamate di funzione.

Un esempio comune è `item-is-valid?`, che avvolge `lumi-item-id-is-valid` per restituire `#t` o `#f`. Se viene restituito `#f`, possiamo attivare un `warning-message` nel codice chiamante, se l'input non è un numero possiamo dare un avviso nella funzione.

```scheme
;; Scopo: Verificare se un elemento è valido, restituisce #t o #f.
;;          Emette un avviso se l'elemento non è un numero.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Utilizzo pratico

Quando si sviluppano plug-in Scheme, il confezionamento delle funzioni in questo modo riduce significativamente i tempi di debug e garantisce un codice robusto e gestibile. Con il nostro sistema di debug in atto, possiamo generare un flusso di debug strutturato nella console degli errori con il semplice tocco di un interruttore.

In questo flusso di debug, le chiamate alle funzioni sono contrassegnate da un asterisco (*), rendendo più semplice tenere traccia dell'esecuzione degli script e individuare gli errori, in particolare nei plug-in complessi. Questa visibilità ci aiuta a comprendere il flusso delle operazioni e a diagnosticare in modo efficiente comportamenti imprevisti.

Un wrapper per la nostra funzione di messaggio per utilizzare `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Esempio di utilizzo pratico di `call`:

```scheme
;; Scopo: Applica il processo di texturing all'elenco dato di maschere di gruppo
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Esempio di flusso di debug durante l'esecuzione di un plug-in:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Questo registro strutturato fornisce una cronologia chiara delle chiamate di funzioni e delle modifiche ai dati, semplificando notevolmente il debug e l'analisi delle prestazioni.

## Conclusione

Implementando un sistema di debug strutturato, creiamo script più sicuri e manutenibili che offrono informazioni in tempo reale sulla loro esecuzione.

### Punti chiave

- **Controlla la verbosità**: utilizza un flag di debug globale per gestire i livelli di output.
- **Fornisci feedback chiaro** – Avvolgi le funzioni standard con messaggi di debug informativi.
- **Miglioramento della robustezza**: gestisci con garbo gli input imprevisti per evitare errori.
- **Semplifica la risoluzione dei problemi**: i messaggi di debug strutturati semplificano la diagnosi e la risoluzione dei problemi.

Con questo approccio, i nostri script si "spiegano da soli" in modo efficace mentre elaborano i dati, riducendo la frustrazione e migliorando l'efficienza del flusso di lavoro. Il debug diventa uno strumento proattivo piuttosto che un compito reattivo, rendendo il nostro processo di scripting più fluido e gratificante.