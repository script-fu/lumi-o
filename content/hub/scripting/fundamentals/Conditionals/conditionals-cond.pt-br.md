---
title: "cond"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c51771681b005905702792ac549ca2707360f265b7d518cc00a6861161158126
url: "hub/scripting/fundamentals/Conditionals/conditionals-cond"
---
Em Scheme, o condicional `cond` é usado para selecionar um de vários blocos de código possíveis com base em múltiplos testes. É como um `if` com vários ramos, em que cada ramo é verificado em ordem até que uma correspondência seja encontrada.

### Sintaxe

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Cada teste é avaliado na ordem em que foi escrito.
- Quando um teste avalia como verdadeiro (`#t`), o **consequent** correspondente é executado e a expressão `cond` para de avaliar testes adicionais.
- A cláusula `else` é opcional e serve como fallback se nenhum teste avaliar como verdadeiro.

### Como funciona

1. **Testar cada condição:**
   - `cond` avalia os testes na ordem listada.

2. **Executar o consequent correspondente:**
   - Quando o primeiro teste que avalia como verdadeiro (`#t`) é encontrado, seu **consequent** é executado.
   - Se nenhum teste avaliar como verdadeiro e houver uma cláusula `else`, o **fallback-consequent** é executado.

### Exemplos

#### Exemplo 1: consequents de expressão única

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- O primeiro teste `(< 3 2)` avalia como falso (`#f`).
- O segundo teste `(= 3 3)` avalia como verdadeiro (`#t`), então `"This will run"` é retornado.
- A cláusula `else` não é executada porque uma correspondência já foi encontrada.

Resultado: **"This will run"**

#### Exemplo 2: várias ações usando `begin`

Quando um consequent envolve várias ações, use `begin` para agrupá-las:

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- O primeiro teste `(< 5 3)` avalia como falso (`#f`).
- O segundo teste `(> 5 3)` avalia como verdadeiro (`#t`):
  - Imprime `"Condition met"`.
  - Depois, calcula `(* 5 5)` e retorna `25`.

Resultado: **Imprime "Condition met" e retorna 25.**

#### Exemplo 3: bloco `let` em um consequent

Quando você precisa introduzir variáveis locais, use um bloco `let`:

```scheme
(cond
  ;; Caso 1: Se 0 for menor que -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Caso 2: se 0 for maior que -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Caso padrão: se nenhuma das condições acima for atendida
  (else
    (let ((z 0))
      z)))
```

- O primeiro teste `(< 0 -1)` é falso.
- O segundo teste `(> 0 -1)` é verdadeiro, então:
  - Um bloco `let` é executado, ligando `y` a `20`.
  - Imprime `"Positive condition met"`.
  - Depois, calcula `(+ y y)` e retorna `40`.

Resultado: **Imprime "Positive condition met" e retorna 40.**

#### Exemplo 4: fallback com `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Nenhum dos dois primeiros testes avalia como verdadeiro.
- A cláusula `else` é executada e retorna `"Fallback value"`.

Resultado: **"Fallback value"**

### Resumo

- Use `cond` para lidar com múltiplas condições de forma clara e concisa.
- Consequents podem ser expressões únicas ou ações agrupadas com `begin`.
- Use `let` em consequents para declarar variáveis locais para cálculos.
- Sempre inclua uma cláusula `else` como fallback para casos inesperados.

Essa flexibilidade torna `cond` uma ferramenta poderosa e legível para lógica de ramificação complexa.
