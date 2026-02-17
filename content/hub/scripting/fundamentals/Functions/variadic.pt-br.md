---
title: "Funções Variádicas"
type: docs
weight: 2
---
**Funções variáveis** em Scheme são funções que aceitam um número variável de argumentos. Essas funções são altamente versáteis e permitem criar código flexível e reutilizável. Na programação funcional, as funções variáveis ​​simplificam as operações que precisam processar um número arbitrário de entradas, como somar uma lista de números ou concatenar strings.

Funções variáveis são especialmente úteis quando:

- O número de argumentos não pode ser determinado antecipadamente.
- Você precisa aplicar a mesma operação a uma lista dinâmica de entradas.
- Escrever utilitários para agregação ou transformação de dados.

### Sintaxe de funções variáveis

Funções variáveis são definidas usando o símbolo `.` antes do último nome do parâmetro. Este último parâmetro reúne todos os argumentos restantes em uma lista.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Quaisquer argumentos fixos e obrigatórios que a função aceita.
- **`variadic-parameter`:** Um parâmetro especial precedido por `.` que coleta argumentos adicionais como uma lista.
- **`body-expression`:** A lógica executada quando a função é chamada.

### Exemplos de funções variáveis

#### Função Variádica Básica

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Explicação**:
  - `numbers` coleta todos os argumentos em uma lista.
  - `apply` aplica a função `+` a todos os elementos da lista.

**Uso**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Função Variádica com Parâmetros Fixos

Você pode combinar parâmetros fixos com um parâmetro variável para criar funções mais flexíveis.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Explicação**:
  - `prefix` é um argumento fixo.
  - `names` coleta os argumentos restantes em uma lista.
  - Cada nome é prefixado com a string fornecida usando `map` e `lambda`.

**Uso**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Combinando Lógica Fixa e Variádica

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Explicação**:
  - `collection-name` é um parâmetro fixo.
  - `items` coleta argumentos adicionais em uma lista.
  - A função concatena o nome da coleção e os itens em uma única string.

**Uso**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Casos de uso avançados

#### Processando entradas arbitrárias

Funções variáveis são excelentes no tratamento de dados arbitrários. Aqui está um exemplo para somar apenas números positivos:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtra números não positivos antes de somar.

**Uso**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Funções Variádicas com Lógica Recursiva

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Explicação**:
  - `first` lida com o primeiro argumento.
  - `rest` coleta os argumentos restantes em uma lista.
  - Calcula recursivamente o valor máximo.

**Uso**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Benefícios das funções variáveis

- **Flexibilidade:** Eles lidam com uma ampla variedade de casos de entrada.
- **Concisão:** Reduza a necessidade de múltiplas funções sobrecarregadas.
- **Operações dinâmicas:** Habilite o processamento de dados em tempo de execução sem saber antecipadamente a contagem de argumentos.

### Quando usar funções variáveis

Use funções variadas quando:

- A função precisa processar um número desconhecido de argumentos.
- Uma única operação se aplica a todas as entradas (por exemplo, soma, concatenação ou mapeamento).
- Simplificando a lógica de ordem superior com argumentos dinâmicos.

Evite funções variadas quando:

- A validação de entrada ou verificação de tipo é complexa.
- Argumentos fixos são suficientes para a lógica necessária.
- A legibilidade fica comprometida devido a operações excessivamente complexas.

### ConclusãoFunções variáveis ​​no Scheme fornecem um mecanismo robusto para lidar com entradas dinâmicas. Ao compreender sua sintaxe e uso, você pode criar scripts flexíveis e poderosos que se adaptam a vários cenários. Combinadas com funções de ordem superior, as funções variadas tornam seu código mais conciso e expressivo.