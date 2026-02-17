---
title: "deixar"
type: docs
weight: 4
---
O nome `let` é usado porque reflete suas origens matemáticas de introdução de ligações temporárias, como em _"Seja \( x = 2 \) e \( y = 3 \)"_.

Uma instrução `let` no Scheme é uma **construção de ligação** usada para definir variáveis ​​dentro de um escopo localizado. Ele permite criar ligações temporárias para variáveis ​​e, em seguida, executar um bloco de código usando essas ligações. Isto é particularmente útil para manter o código modular e evitar a poluição variável global.

Existem três formas principais de `let` no esquema:

- **`let`**: Padrão para criar ligações locais simples.
- **`let*`**: Let sequencial, onde as ligações podem depender dos resultados de ligações anteriores.
- **Nomeado `let`**: Uma forma especial de `let` que cria loops recursivos ou procedimentos nomeados.

Em sua forma mais simples, `let` cria ligações de variáveis ​​locais e avalia uma expressão com essas ligações.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Bindings**: uma lista de pares onde cada par atribui um `value` a um `variable`.
- **Expressão**: O corpo do `let`, que pode usar as variáveis ​​definidas localmente.

### Exemplo

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Isso define duas variáveis locais, `x` (10) e `y` (20).
- Em seguida, calcula `(+ x y)` usando essas variáveis.

**Resultado**: `30`

---

## A construção `let*`

A construção `let*` é semelhante a `let`, mas as ligações são avaliadas **sequencialmente**. Isso significa que as ligações posteriores podem depender das anteriores.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Exemplo

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- A primeira ligação atribui `10` a `x`.
- A segunda ligação calcula `y` como `(+ x 5)`, usando o valor de `x`.
- O corpo calcula `(* x y)`.

**Resultado**: `150`

---

## Nomeado `let`

Um `let` nomeado é uma forma especial de `let` que fornece um nome para o próprio bloco `let`, transformando-o em um procedimento recursivo. Isto é útil para criar loops ou cálculos recursivos.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nome**: O bloco `let` recebe um nome, definindo efetivamente uma função.
- **Bindings**: Valores iniciais para variáveis, semelhantes a um padrão `let`.
- **Body**: A expressão pode chamar o `let` nomeado recursivamente.

### Exemplo: Loop com Nomeado `let`

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- A função `loop` começa com `n = 5` e `result = 1`.
- Se `n` for `0`, ele retornará `result`.
- Caso contrário, ele se chama recursivamente com `n - 1` e `result * n`.

**Resultado**: `120` (fatorial de 5)

---

## Tabela Resumo| Construir | Descrição | Caso de uso |
|------------|------------------------------------------|--------------------------------------------------------------------------|
| **`let`** | Define ligações locais para variáveis.    | Use quando todas as ligações forem independentes e não dependerem umas das outras.     |
| **`let*`** | Define ligações locais sequenciais.       | Use quando as ligações posteriores dependerem dos resultados das anteriores.           |
| **Nomeado `let`** | Define procedimentos locais recursivos. | Use loops for, cálculos iterativos ou recursão em um contexto local. |

---

## Exemplos

### Usando `let` para computação local

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Resultado**: `13` (Calcula `x² + y²`)

---

### Usando `let*` para dependências sequenciais

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Resultado**: `8` (Calcula `x³`)

---

### Usando `let` nomeado para computação recursiva

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Resultado**: `120` (fatorial de 5)

---

Ao usar `let`, `let*` e denominado `let`, Scheme permite programação modular, recursiva e sequencial com regras de escopo claras.