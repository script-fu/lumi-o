---
title: "when"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 8b6c15ef2763fe95100759e0e2e21f2bf43bf8424317be6d414f2b5260587714
url: "hub/scripting/fundamentals/Conditionals/conditionals-when"
---
No Scheme, `if` é versátil, mas sem um ramo `else` explícito pode confundir — especialmente quando só o ramo verdadeiro deve ser executado, sem ação alternativa para o caso falso. Nesses cenários, `when` é mais claro e conciso.

A forma básica de `when`:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` for verdadeiro (`#t`), todas as expressões no corpo de `when` são executadas em sequência.
- Se `test` for falso (`#f`), nada acontece e nenhum valor é retornado.

### Exemplo

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Comparando `if` e `when`

Os dois juntos no mesmo exemplo:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing additional actions within 'when'.")))
```

#### Explicação

1. **Condição do `if`:**
   - O teste `(= 0 1)` verifica se 0 é igual a 1.
   - Como é falso (`#f`), o ramo `else` é executado.

2. **`when` no ramo `else`:**
   - O teste `(< 0 1)` verifica se 0 é menor que 1.
   - Como é verdadeiro (`#t`), todas as expressões no corpo de `when` são executadas em sequência.

#### Por que `when`?

- Evita um `else` vazio ou fictício.
- Deixa claro que só o ramo verdadeiro importa.

### Resumo

- Use `if` quando ambos os ramos importam.
- Use `when` quando há apenas o ramo verdadeiro, especialmente com várias ações.
- Combinar `if` e `when` ajuda a estruturar condições complexas de forma clara.
