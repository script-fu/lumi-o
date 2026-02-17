---
title: "Variabili e ambito"
type: docs
weight: 1
---
In Scheme, la gestione delle variabili e il loro ambito è un concetto fondamentale per scrivere script efficienti e gestibili. Le variabili memorizzano i valori dei dati che lo script può manipolare, mentre l'ambito definisce dove tali variabili sono accessibili. Comprendere come definire e utilizzare le variabili in modo efficace consente di creare codice strutturato, riutilizzabile e privo di errori.

### Digitazione dinamica

Lo schema è tipizzato dinamicamente: non dichiari i tipi in anticipo e una variabile può contenere valori di tipo diverso nel tempo.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### Il ruolo delle definizioni e dell'ambito delle variabili nello schema

La definizione delle variabili e la gestione del loro ambito ha diversi scopi:
- **Organizzazione dei dati:** le variabili memorizzano le informazioni, rendendo i tuoi script più leggibili e gestibili.
- **Miglioramento della riusabilità:** utilizzando variabili con ambito, puoi riutilizzare sezioni di codice senza conflitti.
- **Incapsulamento:** l'ambito localizzato impedisce interazioni involontarie tra variabili in diverse parti dello script.
- **Semplificazione della logica:** le variabili temporanee in un ambito limitato riducono la complessità nei calcoli o nei flussi di lavoro più ampi.

### Tipi di definizioni e ambito delle variabili

Scheme fornisce diversi costrutti per la definizione e l'ambito delle variabili:
- **`let`:** Crea associazioni locali per le variabili all'interno di uno specifico blocco di codice.
- **`let*`:** Una versione sequenziale di `let` in cui ogni associazione può dipendere da quelle precedenti.
- **Nominato `let`:** Un potente costrutto per definire procedure o cicli locali ricorsivi.
- **`define`:** Crea variabili o funzioni globali accessibili da tutto lo script.

### Come funzionano le definizioni e l'ambito delle variabili

Le definizioni e l'ambito delle variabili in genere implicano:
1. **Dichiarazione di variabili:** Assegnazione di un valore a una variabile in un contesto specifico.
2. **Limitazione dell'ambito:** Controllare dove la variabile è accessibile (ad esempio, all'interno di un blocco `let` o globalmente).
3. **Utilizzo delle variabili:** accesso e modifica dei valori delle variabili per eseguire calcoli, operazioni logiche o procedurali.

### Esempio: utilizzo di `let` per variabili locali

Il costrutto `let` consente di definire variabili temporanee disponibili solo all'interno di un blocco specifico:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Questo esempio dichiara `x` e `y` con valori locali e calcola la loro somma.

### Esempio: utilizzo di `define` per le variabili globali

Il costrutto `define` crea variabili o funzioni con ambito globale:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Questo script definisce una costante globale `pi` e una funzione `circle-area` che la utilizza.

### Confronto tra ambiti: locale e globale

| Caratteristica | Ambito locale (`let`, `let*`) | Ambito globale (`define`) |
|------------------|------------------------------------|----------------------------------------------------|
| **Accessibilità** | Limitato al blocco in cui è definito | Accessibile in tutto lo script |
| **Incapsulamento** | Previene le interazioni involontarie | Potrebbe entrare in conflitto con altre variabili definite a livello globale |
| **Caso d'uso** | Variabili temporanee per compiti specifici | Variabili o funzioni condivise utilizzate in |

### Riepilogo- **Le definizioni e l'ambito delle variabili** sono fondamentali per organizzare e gestire i dati negli script di Scheme.
- Utilizzare l'**ambito locale** (`let`, `let*`, denominato `let`) per incapsulare variabili temporanee ed evitare conflitti.
- Utilizza **ambito globale** (`define`) per funzioni riutilizzabili o costanti condivise nel tuo script.
- Una chiara comprensione di questi costrutti migliorerà la leggibilità, la manutenibilità e l'affidabilità del codice.