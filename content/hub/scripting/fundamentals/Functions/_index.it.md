---
title: "Funzioni"
type: docs
weight: 7
---
Le funzioni sono un concetto fondamentale in Scheme, poiché forniscono i mezzi per incapsulare la logica, abilitare il riutilizzo del codice e strutturare gli script in modo efficace. Con le funzioni, puoi creare script modulari e gestibili che gestiscono un'ampia gamma di attività, dalle operazioni di base ai flussi di lavoro avanzati in Lumi.

Questa sezione funge da introduzione alle funzioni di Scheme e pone le basi per comprenderne i tipi, le definizioni e gli usi. Le sezioni successive approfondiranno i tipi di funzioni specifiche e le loro capacità uniche.

## Sintassi ed espressioni minime

Il codice schema è composto da **espressioni**. Un'espressione restituisce un valore. La sintassi è uniforme: le parentesi formano una chiamata, preceduta dal nome dell'operatore o della funzione.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Poiché tutto è un'espressione, il flusso di controllo si adatta naturalmente allo stesso stile delle chiamate di funzione.

## Perché le funzioni sono importanti

Le funzioni svolgono un ruolo fondamentale in Scheme per diversi motivi:

- **Riutilizzabilità del codice:** evita la ripetizione incapsulando la logica in componenti riutilizzabili.
- **Modularità:** suddividi le attività complesse in parti più piccole e gestibili.
- **Comportamento dinamico:** accetta parametri per gestire vari input o adattarsi a diverse situazioni.
- **Astrazione superiore:** Semplifica la logica concentrandosi su "cosa" fa una funzione anziché su "come" lo fa.

## Panoramica dei tipi di funzioni

Scheme offre una varietà di costrutti di funzioni, ciascuno adatto a casi d'uso specifici:

1. **Funzioni con nome**
   Queste sono le funzioni standard definite con `define`. Costituiscono la spina dorsale della maggior parte degli script.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Funzioni anonime**
   Conosciute anche come **funzioni lambda**, si tratta di funzioni senza nome definite in linea per un utilizzo una tantum.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Funzioni di ordine superiore**
   Funzioni che accettano altre funzioni come argomenti o restituiscono funzioni come risultati, consentendo potenti astrazioni come mappatura, filtraggio e riduzione.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Sintassi generale per le funzioni

Le funzioni in Scheme hanno una sintassi semplice e coerente:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** Il nome della funzione.
- **`parameter1, parameter2, ...`:** Gli argomenti accettati dalla funzione.
- **`body-expression`:** La logica eseguita quando viene chiamata la funzione.

Esempio:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Effetti collaterali e stato globale

In Lumi, molte procedure utili hanno **effetti collaterali**: modificano un'immagine, cambiano un disegno, scrivono un file o visualizzano l'output.

- Isolare gli effetti collaterali in procedure piccole e chiaramente denominate.
- Evita di modificare il contesto globale a meno che non sia necessario.
- Quando cambi contesto (colori, pennelli, ecc.), avvolgi il lavoro con `lumi-context-push` e `lumi-context-pop` in modo che lo stato dell'utente venga ripristinato.