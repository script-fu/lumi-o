---
title: "Listas"
type: "docs"
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: caf60dbd4ddbab418dd6779d9efba0217982d37086ed8d485680b96142d5ef6f
url: "hub/scripting/fundamentals/Data Structures/lists"
---
Em Scheme, uma **lista** é uma estrutura de dados fundamental usada para agrupar valores. Listas são coleções ordenadas de elementos onde cada elemento pode ser de qualquer tipo, incluindo outra lista. As listas são amplamente utilizadas em Scheme tanto para armazenamento de dados quanto para estrutura de programa.

### Exemplo 1: Lista Simples

```scheme
(list 1 2 3)
```

- Cria uma lista de três elementos: `1`, `2` e `3`.

Resultado: **`(1 2 3)`**

---

#### Acessando Elementos da Lista

Os elementos de uma lista são acessados usando os procedimentos `car` e `cdr`:

- `car` recupera o primeiro elemento de uma lista.
- `cdr` recupera o resto da lista (tudo exceto o primeiro elemento).

#### Exemplos

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Obtém o primeiro elemento
(cdr my-list)  ; Obtém o restante da lista
```

Resultado:

- `(car my-list)` retorna `1`
- `(cdr my-list)` retorna `(2 3)`

---

#### Recursão simples: iterando por meio de uma lista

Chamando recursivamente `car` no `cdr` de uma lista, você pode processar cada elemento um por um até que a lista seja percorrida. Isso forma a base de muitos algoritmos de processamento de listas.

#### Exemplo: Imprimindo cada elemento de uma lista

Aqui está uma função recursiva simples para imprimir todos os elementos de uma lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Imprime o primeiro elemento
      (print-elements (cdr lst)))))             ;; Processa o restante da lista
```

- **Caso base:** Se a lista estiver vazia (`null? lst`), pare a recursão.
- **Caso recursivo:** Imprima o primeiro elemento (`car lst`) e depois chame a função no restante da lista (`cdr lst`).

#### Exemplo de uso

```scheme
(print-elements (list 1 2 3))
```

Saída:

-`"1"`
- `"2"`
- `"3"`

Resultado: "concluído"

---

#### Como funciona

1. A função recupera o primeiro elemento da lista usando `car` e o processa.
2. Em seguida, ele chama a si mesmo com o restante da lista (`cdr`).
3. Este processo se repete até que a lista fique vazia (`null? lst`).

---

### Exemplo 2: Tipos mistos

As listas podem incluir elementos de diferentes tipos, incluindo strings, booleanos, números, outras listas ou até mesmo o resultado de expressões:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Isso cria uma lista com:
  - Um número (`42`)
  - Uma string (`"hello"`)
  - Um booleano (@LUMI_TOKEN_40@@)
  - Outra lista (`(1 2)`)
  - O resultado de uma expressão (`(+ 3 4)`, que é avaliada como `7`)

Resultado: **`(42 "hello" #t (1 2) 7)`**

---

Esses exemplos demonstram a versatilidade das listas em Scheme, tornando-as uma ferramenta poderosa para organizar e manipular dados.

### Construindo Listas

O procedimento `cons` é usado para construir uma nova lista combinando um elemento com uma lista existente.

```scheme
(cons new-element existing-list)
```

#### Exemplo

```scheme
(cons 0 (list 1 2 3))
```

- Adiciona `0` ao início da lista `(1 2 3)`.

Resultado: **`(0 1 2 3)`**

---

### Verificando listas

O procedimento `list?` verifica se um determinado valor é uma lista.

```scheme
(list? value)
```

#### Exemplo: lista?

```scheme
(list? (list 1 2 3))  ; Verifica se (list 1 2 3) é uma lista
(list? 42)            ; Verifica se 42 é uma lista
```

Resultado:

- `(list? (list 1 2 3))` retorna `#t` (verdadeiro)
- `(list? 42)` retorna `#f` (falso)

---

### Operações em listas

Scheme fornece vários procedimentos integrados para trabalhar com listas, incluindo:

- `length`: Retorna o número de elementos de uma lista.
- `append`: Combina duas ou mais listas em uma.
- `reverse`: Retorna uma nova lista com elementos em ordem inversa.

```scheme
(length (list 1 2 3))          ; Retorna 3
(append (list 1 2) (list 3 4)) ; Retorna (1 2 3 4)
(reverse (list 1 2 3))         ; Retorna (3 2 1)
```

Resultado:

- `(length (list 1 2 3))` retorna `3`
- `(append (list 1 2) (list 3 4))` retorna `(1 2 3 4)`
- `(reverse (list 1 2 3))` retorna `(3 2 1)`

#### Usando `list-ref`

O procedimento `list-ref` recupera o elemento em um índice especificado de uma lista (índice baseado em zero).

```scheme
(list-ref lst index)
```

- **`lst`**: A lista da qual recuperar o elemento.
- **`index`**: Um índice baseado em zero indicando qual elemento retornar.

##### Exemplo: ref-lista

```scheme
(list-ref (list 10 20 30 40) 2)  ; Obtém o elemento no índice 2
```

Resultado: `30`

---

### Listas aninhadas

As listas em Scheme podem conter outras listas como elementos, criando uma estrutura aninhada.

#### Exemplo: Criando uma lista aninhada

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Cria uma lista de três elementos, cada um dos quais é uma lista.

Resultado: **`((1 2) (3 4) (5))`**

---

#### Acessando dados aninhados

Para acessar elementos em uma lista aninhada, você pode usar combinações de `car` e `cdr` para navegar pela estrutura.

#### Exemplo: Acessando Elementos

```scheme
(car nested-list)              ; Obtém o primeiro elemento: (1 2)
(car (car nested-list))        ; Obtém o primeiro elemento da primeira sublista: 1
(cdr (car nested-list))        ; Obtém o restante da primeira sublista: (2)
(car (cdr (car nested-list)))  ; Obtém o segundo elemento da primeira sublista: 2
```

---

#### Explicação

1. **`car nested-list`**:
   - Recupera o primeiro elemento de `nested-list`, que é `(1 2)`.

2. **`car (car nested-list)`**:
   - Recupera o primeiro elemento de `(1 2)`, que é `1`.

3. **`cdr (car nested-list)`**:
   - Recupera o resto de `(1 2)`, que é `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Recupera o primeiro elemento de `(2)`, que é `2`.

---

#### Exemplo: acessando elementos de outras sublistas

```scheme
(car (cdr nested-list))        ; Obtém a segunda sublista: (3 4)
(car (car (cdr nested-list)))  ; Obtém o primeiro elemento da segunda sublista: 3
```

---

Essa abordagem permite navegar e acessar sistematicamente elementos específicos em uma lista aninhada, proporcionando flexibilidade poderosa para trabalhar com dados hierárquicos.

### Resumo

- **Listas** em Scheme são estruturas de dados versáteis e essenciais.
- Use `list` para criar uma lista, `car` e `cdr` para acessar elementos e `cons` para construir listas.
- Procedimentos integrados como `length`, `append`, `reverse` e `list-ref` tornam as operações de lista fáceis e eficientes.
- As listas podem ser aninhadas, permitindo estruturas de dados complexas para casos de uso avançados.