---
title: Functions
type: docs
weight: 7
---

Functions are a core concept in Scheme, providing the means to encapsulate logic, enable code reuse, and structure your scripts effectively. With functions, you can create modular, maintainable scripts that handle a wide range of tasks, from basic operations to advanced workflows in Lumi.

This section serves as an introduction to functions in Scheme and lays the groundwork for understanding their types, definitions, and uses. Subsequent sections will delve deeper into specific function types and their unique capabilities.

## Minimal Syntax and Expressions

Scheme code is made of **expressions**. An expression evaluates to a value. The syntax is uniform: parentheses form a call, with the operator or function name first.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Because everything is an expression, control flow fits naturally into the same style as function calls.

## Why Functions Matter

Functions play a pivotal role in Scheme for several reasons:

- **Code Reusability:** Avoid repetition by encapsulating logic into reusable components.
- **Modularity:** Break down complex tasks into smaller, manageable pieces.
- **Dynamic Behavior:** Accept parameters to handle various inputs or adapt to different situations.
- **Higher Abstraction:** Simplify logic by focusing on "what" a function does rather than "how" it does it.

## Overview of Function Types

Scheme offers a variety of function constructs, each suited to specific use cases:

1. **Named Functions**
   These are standard functions defined with `define`. They form the backbone of most scripts.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Anonymous Functions**
   Also known as **lambda functions**, these are unnamed functions defined inline for one-off use.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Higher-Order Functions**
   Functions that take other functions as arguments or return functions as results, enabling powerful abstractions like mapping, filtering, and reducing.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## General Syntax for Functions

Functions in Scheme have a simple and consistent syntax:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** The name of the function.
- **`parameter1, parameter2, ...`:** The arguments the function takes.
- **`body-expression`:** The logic executed when the function is called.

Example:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Side Effects and Global State

In Lumi, many useful procedures have **side effects**: they modify an image, change a drawable, write a file, or display output.

- Isolate side effects in small, clearly named procedures.
- Avoid changing global context unless you need to.
- When you do change context (colors, brushes, etc), wrap the work with `lumi-context-push` and `lumi-context-pop` so the user’s state is restored.
