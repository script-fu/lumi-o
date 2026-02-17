---
title: "quando"
type: docs
weight: 5
---
No Scheme, embora `if` seja elegante e versátil, pode se tornar confuso quando usado sem um `else` explícito. Isto é particularmente verdadeiro quando a intenção é executar uma única ramificação do código apenas quando uma condição for verdadeira, sem nenhuma ação alternativa para o caso `false`. Nesses cenários, a construção `when` fornece uma alternativa mais clara e concisa.

A forma básica de `when` é assim:

```scheme
(when test-is-true
  do-this
  do-that)
```

- Se `test` for avaliado como verdadeiro (`#t`), todas as expressões no corpo da construção `when` serão executadas sequencialmente.
- Se `test` for avaliado como falso (`#f`), nada acontece e nenhum valor será retornado.

### Exemplo

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### Contrastando `if` e `when`

Para entender melhor a diferença entre `if` e `when`, considere o seguinte exemplo onde ambos são usados juntos:

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### Explicação:

1. **A condição `if`**:
   - O teste `(= 0 1)` verifica se 0 é igual a 1.
   - Como isso é falso (`#f`), o ramo `else` do `if` é executado.

2. **O Construto `when` na Filial `else`**:
   - O teste `when` `(< 0 1)` verifica se 0 é menor que 1.
   - Como isso é verdade (`#t`), todas as expressões dentro do corpo do `when` são executadas sequencialmente:
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### Por que usar `when` aqui?

- Usar `when` em vez de outro `if` simplifica a lógica quando não há necessidade de uma ramificação `else` explícita para a condição.
- `when` deixa claro que apenas o ramo verdadeiro é relevante, reduzindo possível confusão.

### Resumo

- Use `if` quando precisar de uma ramificação verdadeira e falsa.
- Use `when` quando houver apenas uma única ramificação para o caso verdadeiro, especialmente quando múltiplas ações precisarem ser executadas.
- Combinar `if` e `when` pode ajudar a estruturar condicionais mais complexas de forma clara e concisa.