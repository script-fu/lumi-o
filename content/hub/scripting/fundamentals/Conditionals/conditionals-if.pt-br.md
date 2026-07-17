---
title: "if"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 0d4755f22d97955ef430ff8fa948440aecb8db81766bff57ae05ef15ddbf09d2
url: "hub/scripting/fundamentals/Conditionals/conditionals-if"
---
Na sua forma mais simples, o condicional `if` em Scheme avalia um teste e, com base no resultado, executa um de dois possíveis blocos de código. A forma mais simples é:

```scheme
(if test-is-true
  do-this)
```

- Se o `test` avaliar como verdadeiro (`#t`), o **bloco consequent** é executado. O bloco pode retornar um valor ou executar outras ações, como atribuir uma variável ou imprimir saída.

### Exemplo

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- Neste caso, o `test` é `(< 0 1)` (verificando se 0 é menor que 1).
- Como o teste avalia como verdadeiro (`#t`), o bloco de código `(lumi-message "True!")` é executado, imprimindo `"True!"`.

### Adicionando uma condição else: `if-else`

Quando um condicional `if` tem um bloco de código alternativo (o caso `else`), a estrutura é:

```scheme
(if test
  do-this
  else-do-this)
```

- Se o `test` avaliar como verdadeiro (`#t`), o bloco **consequent** é executado.
- Se o `test` avaliar como falso (`#f`), o bloco **alternative** é executado.

```scheme
(if test
  consequent
  alternative)
```

### Como funciona

1. **Expressão de teste:**
   - A expressão `test` é avaliada primeiro.

2. **Resultado com base no teste:**
   - Se o `test` avaliar como verdadeiro (`#t`), o **bloco consequent** é executado.
   - Se o `test` avaliar como falso (`#f`), o **bloco alternative** é executado.

Tanto os blocos `consequent` quanto `alternative` podem executar qualquer operação Scheme válida, incluindo retornar valores, modificar variáveis ou executar procedimentos.

### Exemplos

#### Exemplo 1: retornando um valor

```scheme
(if (< 0 1)
  1
  0)
```

- Aqui, o `test` é `(< 0 1)` (verificando se 0 é menor que 1).
- Como o teste avalia como verdadeiro (`#t`), o bloco **consequent** (`1`) é executado e seu valor é retornado.

Resultado: **1**

#### Exemplo 2: avaliando um bloco `begin`

Quando você precisa executar várias ações quando a condição é verdadeira ou falsa, pode usar `begin` ou `let` para agrupá-las.

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
- Como o teste avalia como falso (`#f`), o bloco **alternative** é executado:
  - Primeiro, imprime `"False condition met, calculating..."`.
  - Depois, calcula `(* 3 4)` e retorna `12`.

Resultado: **Imprime "False condition met, calculating..." e retorna 12.**

#### Exemplo 3: avaliando uma expressão `let`

Usar `let` permite declarar variáveis locais dentro do bloco de código.

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
- Como o teste avalia como verdadeiro (`#t`), o bloco **consequent** é executado:
  - Primeiro, imprime `"True condition met, calculating..."`.
  - Depois, calcula `(* -1 10)` e retorna `-10`.

Resultado: **Imprime "True condition met, calculating..." e retorna -10.**

### Resumo

- O condicional `if` é uma ferramenta poderosa em Scheme para avaliar testes e executar blocos de código correspondentes.
- Pode lidar tanto com expressões simples quanto com blocos de código complexos que retornam valores, modificam variáveis ou produzem efeitos colaterais.
- Lembre-se: se não houver um bloco `else` explícito, o `if` só avalia e executa o **consequent** se o teste for verdadeiro; caso contrário, o **alternative**.
