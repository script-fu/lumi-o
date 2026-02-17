---
title: "Recursão Simples"
type: docs
weight: 5
---
A recursão é um conceito poderoso em Scheme, onde uma função chama a si mesma para resolver subproblemas menores do problema original. Um padrão de **recursão simples** envolve um caso base para interromper a recursão e um caso recursivo para reduzir o problema.

A estrutura geral de uma função recursiva é assim:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condição Base**: Interrompe a recursão.
- **Resultado Base**: O valor retornado quando a condição base é atendida.
- **Chamada Recursiva**: Uma chamada para a própria função com argumentos modificados que aproximam o cálculo do caso base.

---

### Exemplo: Soma de Números (1 a n)

Uma função recursiva simples para calcular a soma dos números de 1 a n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Como funciona: desmontagem e remontagem

A recursão funciona dividindo o problema original em partes menores. Cada chamada para a função trata uma parte e repassa o restante. Uma vez alcançado o caso mais simples, os resultados são remontados à medida que o cálculo é concluído.

#### Rastreamento passo a passo da soma para n 3

1. **Chamada inicial**: *soma para n 3*
   → *(+ 3 (soma de n 2))*

2. **Segunda chamada**: *soma para n 2*
   → *(+ 2 (soma para n 1))*

3. **Terceira Chamada**: *soma para n 1*
   → *(+ 1 (soma para n 0))*

4. **Caso base**: *soma para n 0*
   → *0*

---

#### Remontando o Resultado Final

Uma vez resolvido o caso mais simples, cada camada do cálculo é concluída:

1. *soma para n 0* dá *0*
2. *soma para n 1* torna-se *(+ 1 0) = 1*
3. *soma para n 2* torna-se *(+ 2 1) = 3*
4. *soma para n 3* torna-se *(+ 3 3) = 6*

---

### Exemplo: Imprimindo cada elemento de uma lista

Aqui está uma função recursiva simples para imprimir todos os elementos de uma lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Caso Base:** Se a lista estiver vazia (*null? lst*), pare a recursão.
- **Caso Recursivo:** Imprima o primeiro elemento (*car lst*) e depois chame a função no restante da lista (*cdr lst*).

#### Exemplo de uso

```scheme
(print-elements (list 1 2 3))
```

Saída:

- *"1"*
- *"2"*
- *"3"*

Resultado: *"concluído"*

---

#### Como funciona

1. A função recupera o primeiro elemento da lista usando *car* e o processa.
2. Em seguida, ele chama a si mesmo com o restante da lista (*cdr*).
3. Este processo se repete até que a lista fique vazia (*null? lst*).

---

### Resumo

- A recursão simples consiste em:
  1. **Caso base**: Interrompe a recursão.
  2. **Caso recursivo**: Reduz o problema em direção ao caso base.
- Cada chamada recursiva avança o cálculo até a conclusão.
- Uma vez atingido o caso base, os resultados são combinados à medida que a recursão é concluída.

A recursão reflete a estrutura do problema e fornece um fluxo claro e lógico. Sempre garanta um caso base para evitar recursão infinita.