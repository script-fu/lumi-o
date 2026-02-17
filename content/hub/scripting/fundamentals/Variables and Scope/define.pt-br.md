---
title: "definir"
type: docs
weight: 3
---
A instrução `define` em Scheme é uma construção versátil usada para criar ligações globais ou locais. É mais comumente usado para definir variáveis ​​e funções, tornando-as reutilizáveis ​​e acessíveis através de um script ou dentro de um escopo específico. Compreender `define` é crucial para escrever programas Scheme modulares, reutilizáveis ​​e legíveis.

### Objetivo de `define`

A construção `define` serve a vários propósitos:
- **Definindo Variáveis**: Atribui valores aos nomes de variáveis, disponibilizando-os para uso posterior.
- **Definindo Funções**: Cria procedimentos reutilizáveis ​​que encapsulam lógica específica.
- **Definições locais**: Quando usado em uma função, `define` cria ligações locais que não afetam o namespace global.

---

### Definindo variáveis com `define`

Um uso básico de `define` é criar variáveis que contenham valores constantes ou computados.

#### Sintaxe
```scheme
(define variable-name value)
```

#### Exemplo: Definindo uma Constante
```scheme
(define pi 3.14159)
(* pi 2) ;; Computes 2π
```

**Resultado**: `6.28318`

---

### Definindo funções com `define`

Você pode usar `define` para criar procedimentos reutilizáveis.

#### Sintaxe
```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

#### Exemplo: Definindo uma Função Simples
```scheme
(define (square x)
  (* x x))
(square 4) ;; Computes 4²
```

**Resultado**: `16`

---

### Definições locais com `define`

Quando usado dentro de uma função, `define` cria ligações locais que são acessíveis apenas dentro da função envolvente. Isso evita poluir o namespace global e ajuda a organizar seu código.

#### Exemplo: funções auxiliares locais
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultado**: `41` (Calcula \(2^2 + 3^3 + 4^2\))

---

### Principais recursos de `define`

1. **Escopo Global ou Local**:
   - Quando usado no nível superior, `define` cria variáveis ou funções globais.
   - Quando usado dentro de outra função, `define` cria ligações locais.

2. **Reutilização**:
   - Funções definidas com `define` podem ser reutilizadas várias vezes em diferentes contextos.

3. **Legibilidade aprimorada**:
   - Dividir a lógica em funções menores e bem nomeadas melhora a clareza e a capacidade de manutenção do seu código.

---

### Diferenças entre `define` e `let`

| **Aspecto** | **`define`** | **`let`** |
|------------------------------------|--------------------------------------------------|--------------------------------------------------|
| **Objetivo** | Cria ligações globais ou locais para variáveis ​​ou funções. | Cria vinculações temporárias em um escopo localizado. |
| **Escopo** | Global quando no nível superior; local quando dentro de outra função. | Sempre local para o bloco `let`.       |
| **Reutilização** | Funções e variáveis ​​podem ser reutilizadas em vários locais. | As variáveis ​​são vinculadas temporariamente a um único bloco. |
| **Sintaxe** | Define explicitamente variáveis ​​ou funções.       | Combina ligação de variável com avaliação de expressão. |