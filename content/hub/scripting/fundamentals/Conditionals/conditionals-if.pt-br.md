---
title: "se"
type: docs
weight: 4
---
Em sua forma mais simples, a condicional `if` em Scheme avalia um teste e, com base no resultado, executa um dos dois blocos de código possíveis. A forma mais simples é assim:

```scheme
(if test-is-true
  do-this)
```

- Se `test` for avaliado como verdadeiro (`#t`), o **bloco de código no consequente** será executado. O bloco pode retornar um valor ou realizar outras ações, como atribuir uma variável ou imprimir a saída.

### Exemplo

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Neste caso, o `test` é `(< 0 1)` (verificando se 0 é menor que 1).
- Como o teste é avaliado como verdadeiro (`#t`), o bloco de código `(lumi-message "True!")` é executado, que imprime `"True!"`.

### Adicionando uma condição Else: `if-else`

Ao usar uma condicional `if` com um bloco de código alternativo (o caso `else`), a estrutura fica assim:

```scheme
(if test
  do-this
  else-do-this)
```

- Se `test` for avaliado como verdadeiro (`#t`), o bloco de código **consequente** será executado.
- Se `test` for avaliado como falso (`#f`), o bloco de código **alternativo** será executado.

```scheme
(if test
  consequent
  alternative)
```

### Como funciona

1. **Expressão de teste**:
   - A expressão `test` é avaliada primeiro.

2. **Resultado Baseado no Teste**:
   - Se `test` for avaliado como verdadeiro (`#t`), o **bloco de código subsequente** será executado.
   - Se `test` for avaliado como falso (`#f`), o **bloco de código alternativo** será executado.

Ambos os blocos de código `consequent` e `alternative` podem executar qualquer operação de esquema válida, incluindo retornar valores, modificar variáveis ​​ou executar procedimentos.

### Exemplos

#### Exemplo 1: Retornando um valor

```scheme
(if (< 0 1)
  1
  0)
```

- Aqui, o `test` é `(< 0 1)` (verificando se 0 é menor que 1).
- Como o teste é avaliado como verdadeiro (`#t`), o bloco **consequente** (`1`) é executado e seu valor é retornado.

Resultado: **1**

#### Exemplo 2: Avaliando um bloco inicial

Nos casos em que você precisa realizar várias ações quando a condição é verdadeira ou falsa, você pode usar `begin` ou `let` para agrupá-las.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- Neste exemplo, o `test` é `(= 0 1)` (verificando se 0 é igual a 1).
- Como o teste é avaliado como falso (`#f`), o bloco **alternativo** é executado:
  - Primeiro, imprime `"False condition met, calculating..."`.
  - Depois calcula `(* 3 4)` e retorna `12`.

Resultado: **Imprime "Condição falsa atendida, calculando..." e retorna 12.**

#### Exemplo 3: Avaliando uma instrução let

Usar um `let` nos permite declarar variáveis de escopo local dentro do bloco de código.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- Neste exemplo, o `test` é `(= 1 1)` (verificando se 1 é igual a 1).
- Como o teste é avaliado como verdadeiro (`#t`), o bloco **consequente** é executado:
  - Primeiro, imprime `"True condition met, calculating..."`.
  - Depois calcula `(* -1 10)` e retorna `-10`.

Resultado: **Imprime "Condição verdadeira atendida, calculando..." e retorna -10.**

### Resumo- A condicional `if` é uma ferramenta poderosa no Scheme para avaliar testes e executar blocos de código correspondentes.
- Ele pode lidar com expressões simples e blocos de código complexos que retornam valores, modificam variáveis ​​ou executam efeitos colaterais.
- Lembre-se: Se não houver nenhum bloco `else` explícito, o `if` apenas avalia e executa o **consequente** se o teste for verdadeiro. Caso contrário, ele avalia e executa a **alternativa**.