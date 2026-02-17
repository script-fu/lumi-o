---
title: "Listas de Associação (Alistas)"
type: docs
weight: 6
---
Uma **lista de associações** (ou **alist**) é uma estrutura de dados fundamental no Scheme usada para representar coleções de pares chave-valor. É implementado como uma lista de pares, onde cada par associa uma chave (normalmente um símbolo) a um valor. As listas são simples, flexíveis e adequadas para conjuntos de dados de pequeno e médio porte.

### Estrutura de uma lista de associações

Uma lista é uma lista onde cada elemento é um **par** (construído com `cons`). Cada par consiste em:

- **Chave**: O primeiro elemento (normalmente um símbolo).
- **Valor**: O segundo elemento, que pode ser de qualquer tipo de dados.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Chave**: `'name`, `'age`, `'city`
- **Valor**: `"Alice"`, `30`, `"Paris"`
- **Estrutura**: uma lista de pares:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Criando uma lista

Você pode criar uma lista construindo pares manualmente ou programaticamente usando `cons`.

#### Usando aspas simples (`'`)

A aspa simples (`'`) é uma abreviação para **quoting**, o que impede que Scheme avalie a expressão. Isso o torna ideal para criar listas estáticas onde todas as chaves e valores são codificados.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Resultado**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Usando as crases (`` ` ``) and Comma (`,`)

O operador de crase (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Isso é útil para criar listas onde chaves ou valores são calculados em tempo de execução.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Resultado**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Exemplo de comparação

Lista estática usando `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Lista dinâmica usando `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Acessando dados em uma lista

Para recuperar um valor de uma lista, você pode usar a função `assoc`, que procura um par por sua chave.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Extraindo o valor

Depois de recuperar um par usando `assoc`, use `cdr` para extrair o valor:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Resumo dos principais recursos

- **Aspas simples (`'`)**: Cria uma lista estática onde todos os elementos são dados literais.
- **Backquote (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Notação de ponto (`.`)**: Usada para construir pares, associando uma chave a um valor em uma lista.