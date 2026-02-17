---
title: "Quando"
type: docs
weight: 5
---
In Scheme, sebbene `if` sia elegante e versatile, può creare confusione se utilizzato senza un `else` esplicito. Ciò è particolarmente vero quando l'intenzione è quella di eseguire un singolo ramo di codice solo quando una condizione è vera, senza alcuna azione alternativa per il caso `false`. In tali scenari, il costrutto `when` fornisce un'alternativa più chiara e concisa.

La forma base di `when` è simile alla seguente:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` restituisce true (`#t`), tutte le espressioni nel corpo del costrutto `when` vengono eseguite in sequenza.
- Se `test` restituisce false (`#f`), non accade nulla e non viene restituito alcun valore.

### Esempio

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Contrasto `if` e `when`

Per comprendere meglio la differenza tra `if` e `when`, considerare il seguente esempio in cui entrambi vengono utilizzati insieme:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Spiegazione:

1. **La condizione `if`**:
   - Il test `(= 0 1)` verifica se 0 è uguale a 1.
   - Poiché questo è falso (`#f`), viene eseguito il ramo `else` di `if`.

2. **Il costrutto `when` nel ramo `else`**:
   - Il test `when` `(< 0 1)` controlla se 0 è inferiore a 1.
   - Poiché questo è vero (`#t`), tutte le espressioni all'interno del corpo di `when` vengono eseguite in sequenza:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Perché utilizzare `when` qui?

- L'utilizzo di `when` invece di un altro `if` semplifica la logica quando non è necessario un ramo `else` esplicito per la condizione.
- `when` chiarisce che solo il ramo vero è rilevante, riducendo la potenziale confusione.

### Riepilogo

- Utilizza `if` quando hai bisogno sia di un ramo vero che di un ramo falso.
- Utilizzare `when` quando è presente un solo ramo per il caso reale, soprattutto quando è necessario eseguire più azioni.
- La combinazione di `if` e `when` può aiutare a strutturare condizionali più complessi in modo chiaro e conciso.