---
title: "Símbolos"
type: docs
weight: 6
---
Os símbolos são um dos principais tipos de dados no Scheme, representando identificadores únicos e imutáveis. Eles são usados ​​principalmente como chaves, marcadores ou espaços reservados em programas, tornando-os essenciais para escrever código limpo e expressivo.

Um símbolo no Scheme é semelhante a uma string, mas difere porque os símbolos são **únicos** e **atômicos**. Isso significa que dois símbolos com o mesmo nome têm a garantia de ser o mesmo objeto, permitindo verificações rápidas de igualdade e uso eficiente em estruturas de dados.

### Sintaxe

Um símbolo é escrito como uma sequência de caracteres:

- Começa com uma letra, seguida por letras, dígitos ou caracteres especiais como `-`, `+` ou `*`.
- Os símbolos diferenciam maiúsculas de minúsculas por padrão.

Exemplos:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## Criando Símbolos

Os símbolos são normalmente criados usando o operador **quote** (`'`), que diz ao Scheme para tratar o nome como um símbolo em vez de avaliá-lo como uma variável ou função.

### Exemplo

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

Você também pode criar símbolos programaticamente usando o procedimento `string->symbol`, que converte uma string em um símbolo.

```scheme
(string->symbol "dynamic-symbol")
```

**Resultado**: `'dynamic-symbol`


## Comparando Símbolos

Como os símbolos são únicos, você pode compará-los de forma eficiente usando `eq?`.

### Exemplo

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

Isso torna os símbolos ideais para uso como chaves em estruturas de dados ou marcadores em seu código.

## Usando símbolos

Os símbolos são frequentemente usados no Esquema para:

1. **Chaves nas listas de associações:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **Identificadores no código:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedimentos para trabalhar com símbolos

O esquema fornece procedimentos integrados para trabalhar com símbolos:

| Procedimento | Descrição |
|--------------------|-----------------------------------------------------------------------------|
| **`symbol?`** | Verifica se um objeto é um símbolo.                                            |
| **`eq?`** | Compara dois símbolos de identidade (comparação rápida).                       |
| **`symbol->string`** | Converte um símbolo em uma string (útil para exibição ou depuração).          |
| **`string->symbol`** | Converte uma string em um símbolo (útil para criação dinâmica de identificadores). |

### Exemplos

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## Resumo

Os símbolos são uma forma leve e eficiente de representar identificadores, chaves e marcadores no Scheme. Sua imutabilidade e rápidas verificações de identidade os tornam ideais para muitas tarefas de programação. Compreender como usar símbolos de maneira eficaz aumentará sua capacidade de escrever código Scheme limpo e expressivo.