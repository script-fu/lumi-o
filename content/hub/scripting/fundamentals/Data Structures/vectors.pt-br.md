---
title: "Vetores"
type: docs
weight: 5
---
No Scheme, um vetor é outra estrutura de dados fundamental usada para agrupar valores. Ao contrário das listas, os vetores são coleções de elementos indexados de tamanho fixo, fornecendo acesso aleatório e atualizações mais rápidas. Cada elemento de um vetor pode ser de qualquer tipo, incluindo outro vetor. Os vetores são representados usando # seguido de parênteses. `#(1 2 3)`

Embora vetores e listas possam parecer semelhantes, eles servem a propósitos diferentes na programação de esquemas:

- As listas são mais comumente usadas para operações recursivas e estruturas dinâmicas, pois sua implementação de nós vinculados permite a manipulação eficiente de seu início e travessia por meio de decomposição recursiva.

- Os vetores, por outro lado, são otimizados para cenários em que é necessário acesso aleatório a elementos ou atualizações em índices específicos, tornando-os mais adequados para casos de uso como tabelas de pesquisa, configurações de tamanho fixo ou operações indexadas de desempenho crítico.

Em essência, as listas são a escolha natural para algoritmos recursivos e dados de tamanho dinâmico, enquanto os vetores brilham quando padrões de acesso de tamanho fixo ou indexados são fundamentais.

### Vetores Simples

```scheme
(vector 1 2 3)
```

- Cria um vetor de três elementos: `1`, `2` e `3`.

Resultado: **`#(1 2 3)`**

#### Acessando elementos vetoriais

Os elementos em um vetor são acessados usando o procedimento `vector-ref`, que recupera o elemento em um índice especificado (começando em `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iteração: Processando cada elemento em um vetor

Você pode iterar através de um vetor usando um loop ou recursão. O esquema fornece `vector-length` para determinar o tamanho de um vetor. Aqui está um loop simples para imprimir todos os elementos de um vetor:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Caso Base:** Se o índice `i` atingir o comprimento do vetor, pare o loop.
- **Caso recursivo:** Imprima o elemento no índice `i` e aumente `i`.

#### Exemplo de uso

```scheme
(print-elements (vector 1 2 3))
```

Resultado:

- `"1"`
- `"2"`
- `"3"`

Resultado: "concluído"

### Vetores mistos

Os vetores podem incluir elementos de diferentes tipos, incluindo strings, booleanos, números, outros vetores ou até mesmo o resultado de expressões:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Isso cria um vetor com:
  - Um número (@LUMI_TOKEN_26@@)
  - Uma string (`"hello"`)
  - Um booleano (@LUMI_TOKEN_28@@)
  - Outro vetor (@LUMI_TOKEN_29@@)
  - O resultado de uma expressão (`(+ 3 4)`, que é avaliada como `7`)

Resultado: **`#(42 "hello" #t #(1 2) 7)`**

### Construindo Vetores

Os vetores são criados usando `vector` ou usando `make-vector` para criar um vetor de tamanho fixo com um valor inicial.

```scheme
(make-vector 5 0)
```

Cria um vetor de tamanho `5` com todos os elementos inicializados como `0`.

Resultado: `#(0 0 0 0 0)`

### Atualizando vetores

O procedimento `vector-set!` atualiza um elemento em um vetor em um índice especificado.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Resultado: `#(1 42 3)`

### Verificando vetores

O procedimento `vector?` verifica se um determinado valor é um vetor.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Resultado:

- `(vector? (vector 1 2 3))` retorna `#t` (verdadeiro)
- `(vector? 42)` retorna `#f` (falso)

### Vetores e comportamento de passagem por referênciaNo Scheme, os vetores são mutáveis ​​e passados ​​por referência. Isso significa que quando você passa um vetor para uma função, a função pode modificar diretamente o vetor original. Quaisquer alterações feitas no vetor dentro da função também serão refletidas fora da função. Esse comportamento é útil para compartilhar e atualizar dados com eficiência em diversas funções, mas também requer cautela para evitar efeitos colaterais indesejados.

#### Exemplo: Modificando um vetor em uma função

Aqui está um exemplo que demonstra como os vetores são passados por referência e modificados:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Resultado: `#(10 99 30)`

#### Explicação passo a passo

1. **Crie um vetor:** `my-vector` é inicializado com os valores `10`, `20` e `30`.
2. **Passar para uma função:** `my-vector` é passado para `modify-vector` junto com o índice e o novo valor a ser atualizado.
3. **Modificar na função:** O procedimento `vector-set!` atualiza o valor no índice especificado diretamente no vetor original.
4. **Refletir alterações:** Como os vetores são passados ​​por referência, as alterações feitas na função são refletidas no vetor original.

#### Implicações da passagem por referência

- **Desempenho:** A passagem de vetores por referência é eficiente porque evita a cópia de grandes estruturas.
- **Efeitos colaterais:** Tenha cuidado ao compartilhar vetores entre funções para evitar modificações não intencionais nos dados compartilhados.

### Operações em vetores

O Scheme fornece vários procedimentos integrados para trabalhar com vetores, incluindo:

- `vector-length`: Retorna o número de elementos em um vetor.
- `vector->list`: Converte um vetor em uma lista.
- `list->vector`: Converte uma lista em um vetor.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Resultado:

- `(vector-length (vector 1 2 3))` retorna `3`
- `(vector->list (vector 1 2 3))` retorna `(1 2 3)`
- `(list->vector (list 1 2 3))` retorna `#(1 2 3)`

### Vetores aninhados

Os vetores no esquema podem conter outros vetores como elementos, criando uma estrutura aninhada.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Cria um vetor de três elementos, cada um dos quais é um vetor.

Resultado: **`#(#(1 2) #(3 4) #(5))`**

#### Acessando dados aninhados

Para acessar elementos dentro de um vetor aninhado, use `vector-ref` várias vezes para navegar pela estrutura.

#### Exemplo: Acessando Elementos

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Resumo

- **Vetores** em Scheme são estruturas de dados indexadas de tamanho fixo.
- Use `vector` para criar um vetor, `vector-ref` para acessar elementos e `vector-set!` para atualizar elementos.
- Procedimentos integrados como `vector-length`, `vector->list` e `list->vector` permitem operações flexíveis.
- Vetores aninhados permitem estruturas de dados hierárquicas e complexas.