---
title: "Funções"
type: docs
weight: 7
---
Functions are a core concept in Scheme, providing the means to encapsulate logic, enable code reuse, and structure your scripts effectively. Com funções, você pode criar scripts modulares e de fácil manutenção que lidam com uma ampla variedade de tarefas, desde operações básicas até fluxos de trabalho avançados no Lumi.

Esta seção serve como uma introdução às funções no Scheme e estabelece as bases para a compreensão de seus tipos, definições e usos. As seções subsequentes se aprofundarão em tipos de funções específicas e em seus recursos exclusivos.

## Minimal Syntax and Expressions

Scheme code is made of **expressions**. An expression evaluates to a value. The syntax is uniform: parentheses form a call, with the operator or function name first.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Because everything is an expression, control flow fits naturally into the same style as function calls.

## Por que as funções são importantes

As funções desempenham um papel fundamental no Scheme por vários motivos:

- **Reutilização de código:** Evite a repetição encapsulando a lógica em componentes reutilizáveis.
- **Modularidade:** Divida tarefas complexas em partes menores e gerenciáveis.
- **Comportamento dinâmico:** Aceite parâmetros para lidar com diversas entradas ou se adapte a diferentes situações.
- **Abstração superior:** Simplifique a lógica concentrando-se em "o que" uma função faz em vez de "como" ela faz.

## Overview of Function Types

Scheme oferece uma variedade de construções de funções, cada uma adequada para casos de uso específicos:

1. **Funções nomeadas**
   These are standard functions defined with `define`. Eles formam a espinha dorsal da maioria dos scripts.

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

- **`function-name`:** The name of the function.
- **`parameter1, parameter2, ...`:** The arguments the function takes.
- **`body-expression`:** The logic executed when the function is called.

Exemplo:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Side Effects and Global State

In Lumi, many useful procedures have **side effects**: they modify an image, change a drawable, write a file, or display output.

- Isolate side effects in small, clearly named procedures.
- Avoid changing global context unless you need to.
- Ao alterar o contexto (cores, pincéis, etc), envolva o trabalho com `lumi-context-push` e `lumi-context-pop` para que o estado do usuário seja restaurado.