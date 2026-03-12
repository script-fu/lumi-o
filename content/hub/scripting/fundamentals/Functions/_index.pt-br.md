---
title: "Funções"
type: docs
weight: 7
---
Funções são um conceito central no Scheme, fornecendo os meios para encapsular a lógica, permitir a reutilização de código e estruturar seus scripts de maneira eficaz. Com funções, você pode criar scripts modulares e de fácil manutenção que lidam com uma ampla variedade de tarefas, desde operações básicas até fluxos de trabalho avançados no Lumi.

Esta seção serve como uma introdução às funções no Scheme e estabelece as bases para a compreensão de seus tipos, definições e usos. As seções subsequentes se aprofundarão em tipos de funções específicas e em seus recursos exclusivos.

## Sintaxe e expressões mínimas

O código do esquema é composto de **expressões**. Uma expressão é avaliada como um valor. A sintaxe é uniforme: os parênteses formam uma chamada, com o nome do operador ou da função primeiro.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Como tudo é uma expressão, o fluxo de controle se ajusta naturalmente ao mesmo estilo das chamadas de função.

## Por que as funções são importantes

As funções desempenham um papel fundamental no Scheme por vários motivos:

- **Reutilização de código:** Evite a repetição encapsulando a lógica em componentes reutilizáveis.
- **Modularidade:** Divida tarefas complexas em partes menores e gerenciáveis.
- **Comportamento dinâmico:** Aceite parâmetros para lidar com diversas entradas ou se adapte a diferentes situações.
- **Abstração superior:** Simplifique a lógica concentrando-se em "o que" uma função faz em vez de "como" ela faz.

## Visão geral dos tipos de função

Scheme oferece uma variedade de construções de funções, cada uma adequada para casos de uso específicos:

1. **Funções nomeadas**
   Estas são funções padrão definidas com `define`. Eles formam a espinha dorsal da maioria dos scripts.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Funções anônimas**
   Também conhecidas como **funções lambda**, são funções sem nome definidas em linha para uso único.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Funções de ordem superior**
   Funções que usam outras funções como argumentos ou retornam funções como resultados, permitindo abstrações poderosas como mapeamento, filtragem e redução.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Sintaxe Geral para Funções

As funções no Scheme possuem uma sintaxe simples e consistente:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** O nome da função.
- **`parameter1, parameter2, ...`:** Os argumentos que a função recebe.
- **`body-expression`:** A lógica executada quando a função é chamada.

Exemplo:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Efeitos colaterais e estado global

No Lumi, muitos procedimentos úteis têm **efeitos colaterais**: eles modificam uma imagem, alteram um drawable, gravam um arquivo ou exibem a saída.

- Isolar os efeitos colaterais em procedimentos pequenos e claramente nomeados.
- Evite mudar o contexto global, a menos que seja necessário.
- Ao alterar o contexto (cores, pincéis, etc), envolva o trabalho com `lumi-context-push` e `lumi-context-pop` para que o estado do usuário seja restaurado.