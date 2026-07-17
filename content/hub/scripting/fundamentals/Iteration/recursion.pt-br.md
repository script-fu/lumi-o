---
title: "Recursão simples"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 5aba405f536ffdb990315f13682e0e98b60a6110e3336e628bcfad7cab68161b
url: "hub/scripting/fundamentals/Iteration/recursion"
---
A recursão é um conceito poderoso em Scheme, em que uma função chama a si mesma para resolver subproblemas menores do problema original. Um padrão de **recursão simples** envolve um caso base para parar a recursão e um caso recursivo para reduzir o problema.

A estrutura geral de uma função recursiva é:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condição base:** Para a recursão.
- **Resultado base:** O valor retornado quando a condição base é atendida.
- **Chamada recursiva:** Uma chamada à própria função com argumentos modificados que aproximam o cálculo do caso base.

---

### Exemplo: soma de números (de 1 a n)

Uma função recursiva simples para calcular a soma dos números de 1 a n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Caso base: parar quando n for 0
    0                          ; Resultado base: a soma é 0
    (+ n (sum-to-n (- n 1))))) ; Chamada recursiva: soma n atual com o resultado do subproblema menor
```

---

#### Como funciona: decompondo e remontando

A recursão funciona decompondo o problema original em partes menores. Cada chamada da função trata uma parte e passa o restante adiante. Quando o caso mais simples é alcançado, os resultados são remontados à medida que o cálculo se completa.

#### Rastreamento passo a passo de sum-to-n 3

1. **Chamada inicial:** *sum-to-n 3*
   → *(+ 3 (sum-to-n 2))*

2. **Segunda chamada:** *sum-to-n 2*
   → *(+ 2 (sum-to-n 1))*

3. **Terceira chamada:** *sum-to-n 1*
   → *(+ 1 (sum-to-n 0))*

4. **Caso base:** *sum-to-n 0*
   → *0*

---

#### Remontando o resultado final

Quando o caso mais simples é resolvido, cada camada do cálculo se completa:

1. *sum-to-n 0* retorna *0*
2. *sum-to-n 1* torna-se *(+ 1 0) = 1*
3. *sum-to-n 2* torna-se *(+ 2 1) = 3*
4. *sum-to-n 3* torna-se *(+ 3 3) = 6*

---

### Exemplo: imprimir cada elemento de uma lista

Aqui está uma função recursiva simples para imprimir cada elemento de uma lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Imprime o primeiro elemento
      (print-elements (cdr lst)))))             ; Processa o restante da lista
```

- **Caso base:** Se a lista estiver vazia (*null? lst*), a recursão para.
- **Caso recursivo:** Imprime o primeiro elemento (*car lst*), depois chama a função no restante da lista (*cdr lst*).

#### Exemplo de uso

```scheme
(print-elements (list 1 2 3))
```

Saída:

- *"1"*
- *"2"*
- *"3"*

Resultado: *"done"*

---

#### Como funciona

1. A função obtém o primeiro elemento da lista com *car* e o processa.
2. Em seguida, chama a si mesma com o restante da lista (*cdr*).
3. Esse processo se repete até a lista estar vazia (*null? lst*).

---

### Resumo

- A recursão simples consiste em:
  1. **Caso base:** Para a recursão.
  2. **Caso recursivo:** Reduz o problema em direção ao caso base.
- Cada chamada recursiva aproxima o cálculo da conclusão.
- Quando o caso base é alcançado, os resultados são combinados ao término da recursão.

A recursão reflete a estrutura do problema e oferece um fluxo claro e lógico. Sempre garanta um caso base para evitar recursão infinita.
