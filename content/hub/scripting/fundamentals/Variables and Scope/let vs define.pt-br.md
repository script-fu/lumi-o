---
title: "Nomeado let ou Local define"
type: docs
weight: 5
---
Ambos **named `let`** e **local `define`** são ferramentas poderosas no Scheme para estruturar seu código, mas servem a propósitos diferentes. Compreender quando usar cada um ajuda na criação de scripts limpos, modulares e eficientes.

### Visão geral

- **Nomeado `let`**: Uma construção que combina ligação de variáveis e recursão em um escopo localizado, normalmente usado para cálculos iterativos ou recursivos.
- **Local `define`**: Uma maneira de definir funções auxiliares ou variáveis ​​dentro do escopo de uma função envolvente, tornando-as reutilizáveis ​​em diferentes partes dessa função.

---

### Nomeado `let`

#### Características:
1. Combina ligações variáveis e recursão em uma única construção.
2. Escopo do corpo do bloco `let`.
3. Ideal para **recursão localizada** ou processos iterativos específicos para uma única tarefa.

#### Sintaxe
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Exemplo: somando elementos de uma lista
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Resultado**: `10`

- **Como funciona**: A função `loop` é definida dentro de `let`, permitindo chamadas recursivas com ligações atualizadas.

---

### Local `define`

#### Características:
1. Permite a criação de funções auxiliares ou variáveis que são reutilizáveis dentro da função envolvente.
2. Com escopo definido para a função envolvente, mas visível em todo o seu corpo.
3. Ideal para modularizar código com múltiplas etapas ou lógica reutilizável.

#### Sintaxe
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Exemplo: Processando Vários Valores
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultado**: `41` (Calcula \(2^2 + 3^3 + 4^2\))

- **Como funciona**: As funções auxiliares `square` e `cube` são reutilizáveis dentro da função `process-values`, permitindo lógica modular.

---

### Principais diferenças

| **Aspecto** | **Nomeado `let`** | **Local `define`** |
|--------------------------|----------------------------------------------------------------|------------------------------------------------|
| **Objetivo** | Combina recursão e iteração de forma localizada. | Define funções ou variáveis ​​auxiliares reutilizáveis. |
| **Escopo** | Limitado ao corpo do bloco `let`.           | Visível em toda a função envolvente.      |
| **Reutilização** | Não reutilizável fora do bloco `let`.             | Reutilizável várias vezes dentro da função.    |
| **Melhor caso de uso** | Recursão ou iteração localizada vinculada a uma única tarefa. | Modularização de código com múltiplas etapas reutilizáveis. |
| **Sintaxe** | Combina ligação e recursão em uma construção.  | Define explicitamente funções ou variáveis.      |

---

### Quando usar o nome `let`

1. **Lógica de uso único**: Quando a recursão ou iteração é específica para um único cálculo.
2. **Encapsulamento**: Para evitar a adição de nomes de funções extras ao namespace da função envolvente.
3. **Iteração**: Ao gerenciar variáveis ​​intermediárias em uma construção de loop.

**Exemplo: cálculo fatorial**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Resultado**: `120`

---

### Quando usar local `define`

1. **Ajudantes Reutilizáveis**: Quando a lógica precisa ser reutilizada em várias partes da função.
2. **Design Modular**: Para dividir cálculos complexos em subtarefas menores e nomeadas.
3. **Múltiplas etapas**: quando diversas funções auxiliares são necessárias para diferentes partes do cálculo.**Exemplo: Processamento de entradas**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Resultado**: `(13 36)` (Calcula \(2^2 + 3^2\) e \(2^2 \cdot 3^2\))

---

### Combinando declaração e entrada em Named `let`

Um dos recursos mais poderosos de um `let` nomeado é sua capacidade de combinar **declaração de variável local** e **parâmetros de entrada** para recursão em uma única construção. Isso torna o `let` nomeado conciso e expressivo para tarefas iterativas ou recursivas.

#### Declaração de variável local
Em um `let` nomeado, as ligações entre parênteses atuam como **variáveis locais** que são inicializadas com valores específicos. Essas variáveis ​​têm como escopo o corpo do `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` e `y`** são variáveis locais definidas e inicializadas como parte de `let`.

---

#### Parâmetros de entrada para recursão
As mesmas variáveis também atuam como **parâmetros de entrada** para as chamadas recursivas para o `let` nomeado. Quando o `let` nomeado chama a si mesmo, ele atualiza essas variáveis ​​​​com novos valores.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Primeira Iteração**: `x = 1`, `y = 2`
- **Segunda Iteração**: `x = 2`, `y = 4`
- **Terceira iteração**: `x = 3`, `y = 8` e assim por diante...

---

#### Equivalente usando local `define`

Um `let` nomeado inclui inicialização de variável como parte de sua sintaxe. Isto elimina a necessidade de uma etapa separada para configurar os valores iniciais. Os dois exemplos a seguir são equivalentes:

##### Usando Nomeado `let`
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Usando local `define`
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Ambos executam o mesmo cálculo, mas o `let` nomeado combina a declaração de variável e a configuração de recursão em uma construção concisa.

---

#### Vantagens de combinar declaração e entrada

1. **Concisão**: Nomeado `let` reduz o padrão ao mesclar a inicialização de variáveis e a recursão em uma única construção.
2. **Clareza**: deixa claro que a recursão é local para `let` e vinculada a uma tarefa específica.
3. **Encapsulamento**: a lógica recursiva permanece independente e não polui o namespace da função envolvente.

Essa natureza de dupla finalidade de um `let` nomeado - tanto como uma declaração de variável quanto como um mecanismo de entrada recursivo - é o que o torna um recurso poderoso e único na programação de Scheme.

### Resumo

- Use **nomeado `let`** para **recursão localizada** ou **iteração**, especialmente quando a lógica está fortemente acoplada a uma única tarefa.
- Use **local `define`** para **modularizar código** com funções ou variáveis ​​auxiliares reutilizáveis.

Ao compreender suas diferenças, você pode escrever programas de Scheme mais concisos, organizados e fáceis de manter.