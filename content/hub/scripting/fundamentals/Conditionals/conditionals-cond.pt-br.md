---
title: "condição"
type: docs
weight: 5
---
No Scheme, a condicional `cond` é usada para selecionar um dos vários blocos de código possíveis para execução, com base em vários testes. É como uma ramificação múltipla `if`, onde cada ramificação é verificada em ordem até que uma correspondência seja encontrada.

### Sintaxe

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- Cada teste é avaliado pela ordem em que são escritos.
- Quando um teste é avaliado como verdadeiro (`#t`), seu **consequente** correspondente é executado e a expressão `cond` para de avaliar testes adicionais.
- A cláusula `else` é opcional e serve como substituto se nenhum dos testes for avaliado como verdadeiro.

### Como funciona

1. **Teste cada condição**:
   - `cond` avalia os testes na ordem em que estão listados.

2. **Executar a correspondência consequente**:
   - Quando o primeiro teste avaliado como verdadeiro (`#t`) é encontrado, seu **consequente** é executado.
   - Se nenhum teste for avaliado como verdadeiro e houver uma cláusula `else`, o **fallback-consequent** será executado.

### Exemplos

#### Exemplo 1: Conseqüentes de Expressão Única

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- O primeiro teste `(< 3 2)` é avaliado como falso (`#f`).
- O segundo teste `(= 3 3)` é avaliado como verdadeiro (`#t`), então `"This will run"` é retornado.
- A cláusula `else` não é executada porque já foi encontrada uma correspondência.

Resultado: **"Isso será executado"**

#### Exemplo 2: múltiplas ações usando `begin`

Quando um consequente envolve múltiplas ações, use `begin` para agrupá-las:

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

- O primeiro teste `(< 5 3)` é avaliado como falso (`#f`).
- O segundo teste `(> 5 3)` é avaliado como verdadeiro (`#t`):
  - Imprime `"Condition met"`.
  - Depois calcula `(* 5 5)` e retorna `25`.

Resultado: **Imprime "Condição atendida" e retorna 25.**

#### Exemplo 3: Usando um bloco `let` em consequência

Quando precisar introduzir variáveis locais, use um bloco `let`:

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- O primeiro teste `(< 0 -1)` é falso.
- O segundo teste `(> 0 -1)` é verdadeiro, então:
  - Um bloco `let` é executado, vinculando `y` a `20`.
  - Imprime `"Positive condition met"`.
  - Depois calcula `(+ y y)` e retorna `40`.

Resultado: **Imprime "Condição positiva atendida" e retorna 40.**

#### Exemplo 4: substituto com `else`

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- Nenhum dos dois primeiros testes é avaliado como verdadeiro.
- A cláusula `else` é executada e retorna `"Fallback value"`.

Resultado: **"Valor substituto"**

### Resumo

- Use `cond` para lidar com múltiplas condições de maneira clara e concisa.
- Consequentes podem ser expressões únicas ou ações agrupadas usando `begin`.
- Use `let` em consequentes para declarar variáveis ​​locais para cálculos.
- Sempre inclua uma cláusula `else` como substituto para lidar com casos inesperados.

Essa flexibilidade torna `cond` uma ferramenta poderosa e legível para lidar com lógica de ramificação complexa.