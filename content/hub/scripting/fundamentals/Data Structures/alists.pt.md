---
title: "Listas de Associação (Alistas)"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
translation_lock: true
url: "hub/scripting/fundamentals/Data Structures/alists"
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

Pode criar uma lista construindo pares manualmente ou programaticamente usando `cons`.

#### Usando aspas simples (`'`)

A aspa simples (`'`) é uma abreviação para **quoting**, o que impede que Scheme avalie a expressão. Isto torna-a ideal para criar listas estáticas em que todas as chaves e valores estão definidos no código.

```scheme
;; Definir manualmente uma alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Adicionar programaticamente um novo par
(define updated-alist (cons '(country . "France") alist))
```

**Resultado**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Usando as crases (`` ` ``) e a vírgula (`,`)

O operador de crase (`` ` ``) é semelhante à aspa simples, mas permite inserir dinamicamente expressões avaliadas com a vírgula (`,`). Isso é útil para criar listas onde chaves ou valores são calculados em tempo de execução.

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

Lista dinâmica usando `` ` `` e `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Acessando dados em uma lista

Para recuperar um valor de uma lista, pode usar a função `assoc`, que procura um par pela respetiva chave.

```scheme
(assoc 'name alist) ; Retorna (name . "Alice")
(assoc 'country alist) ; Retorna #f (chave não encontrada)
```

### Extraindo o valor

Depois de recuperar um par usando `assoc`, use `cdr` para extrair o valor:

```scheme
(cdr (assoc 'name alist)) ; Retorna "Alice"
```

### Resumo dos principais recursos

- **Aspas simples (`'`)**: Cria uma lista estática onde todos os elementos são dados literais.
- **Crase (`` ` ``)**: Permite criar alists dinamicamente, combinando elementos estáticos com expressões avaliadas (usando `,`).
- **Notação de ponto (`.`)**: Usada para construir pares, associando uma chave a um valor em uma lista.