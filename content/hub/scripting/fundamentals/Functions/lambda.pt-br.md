---
title: "Funções Lambda"
type: docs
weight: 1
---
**Funções lambda** em Scheme são funções anônimas, o que significa que são funções sem nome. Essas funções são definidas em linha e normalmente são usadas para operações curtas e únicas. A construção `lambda` é uma ferramenta poderosa em programação funcional, permitindo criar lógica concisa e flexível dinamicamente.

As funções Lambda são especialmente úteis quando:

- Você precisa de uma pequena função para uma finalidade específica e temporária.
- Passando funções como argumentos para funções de ordem superior como `map`, `filter` ou `fold`.
- Retornando funções de outras funções.

### Sintaxe de funções Lambda

As funções Lambda podem ser definidas por conta própria...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...ou invocado imediatamente:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Os parâmetros que a função aceita.
- **`body-expression`:** A lógica executada quando a função é chamada.
- **Invocação Imediata:** A segunda forma mostra um lambda sendo chamado imediatamente com argumentos.

### Exemplos de funções Lambda

#### Usando Lambda para cálculos simples

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Aqui:

- Uma função lambda é criada para adicionar dois números (`x` e `y`).
- A função é imediatamente invocada com os argumentos `3` e `5`.

#### Funções Lambda embutidas

O exemplo a seguir demonstra como usar `for-each` com uma função nomeada e uma função lambda:

**Usando uma função nomeada:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Explicação**:
  - `print-item` é uma função nomeada que converte um número em uma string (`number->string`) e o imprime usando `lumi-message`.
  - `for-each` aplica `print-item` a cada elemento da lista `(1 2 3 4)`.

**Saída**: 1 2 3 4

**Usando uma função Lambda:**

A mesma lógica pode ser escrita em linha com uma função lambda, evitando a necessidade de uma função nomeada separada:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Explicação**:
  - O `(lambda (x) (lumi-message (number->string x)))` define uma função anônima.
  - Esta função é aplicada a cada elemento da lista `(1 2 3 4)` por `for-each`.

**Saída**: 1 2 3 4

#### Funções Lambda como argumentos

As funções Lambda geralmente são passadas diretamente para funções de ordem superior, como `map` ou `filter`.

#### Quadratura de uma lista de números

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- A função `lambda` quadra cada elemento da lista.
- A função `map` aplica o `lambda` a cada elemento.

#### Funções Lambda como valores de retorno

Você pode retornar uma função lambda de outra função para criar um comportamento dinâmico.

#### Gerando uma função de somador

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` gera uma nova função lambda que adiciona um número específico (`n`).
- O lambda retornado é armazenado em `add5`, que adiciona `5` à sua entrada.

#### Usando Lambda com `let`

Lambdas são frequentemente usados com `let` para criar funções temporárias com escopo local.

#### Lambda local para adição

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- O `let` vincula uma função lambda ao nome `add`.
- O lambda é então usado como uma função normal dentro do escopo `let`.

#### Combinando Lambdas com Funções de Ordem Superior

Lambdas brilham quando combinados com funções de ordem superior para realizar transformações complexas de dados.

#### Filtrando números pares

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- O `lambda` verifica se um número é par.
- A função `filter` usa o lambda para manter apenas os números pares da lista.

### Benefícios das funções Lambda

- **Concisão:** Lambdas reduzem o código padrão, eliminando a necessidade de definir funções nomeadas separadas.
- **Flexibilidade:** você pode defini-los e usá-los onde for necessário, tornando o código mais modular.
- **Legibilidade aprimorada:** Para tarefas curtas e específicas, lambdas deixam a intenção clara sem sobrecarregar o código com funções nomeadas adicionais.

### Quando usar funções Lambda

Use funções lambda quando:

- A lógica é curta e independente.
- A função é necessária apenas temporariamente ou dentro de um escopo específico.
- Você está trabalhando com funções de ordem superior como `map`, `filter` ou `reduce`.

Evite usar lambdas para lógica complexa e multilinha, pois isso pode reduzir a legibilidade. Para operações mais extensas, use uma função nomeada.

### Conclusão

As funções Lambda no Scheme fornecem uma maneira concisa e poderosa de definir funções anônimas para tarefas específicas. Sua flexibilidade e facilidade de uso fazem deles uma ferramenta essencial para qualquer programador de Scheme. Entender como usar `lambda` efetivamente ajudará você a escrever scripts mais limpos, modulares e eficientes.