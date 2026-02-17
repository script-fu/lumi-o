---
title: "fazer"
type: docs
weight: 5
---
A função `do` em Scheme é um mecanismo de loop que permite iteração com condições de inicialização, atualização e término. É particularmente útil quando você precisa executar uma sequência de operações um número específico de vezes ou até que uma condição seja atendida.

A forma geral de `do` é:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variável**: As variáveis do loop.
- **Valor inicial**: O valor inicial de cada variável do loop.
- **Expressão de atualização**: A expressão para atualizar as variáveis ​​de loop no final de cada iteração.
- **Condição de terminação**: A condição para parar o loop.
- **Expressão de resultado**: O valor a ser retornado quando o loop terminar.
- **Body**: O código a ser executado em cada iteração.

---

### Exemplo: Soma os Números de 1 a 5

```scheme
(do ((i 1 (+ i 1))      ; Initialize i to 1, increment by 1
     (sum 0 (+ sum i))) ; Initialize sum to 0, add i to sum
    ((> i 5) sum)       ; Terminate when i > 5, return sum
  (lumi-message (number->string sum))) ; Print sum at each step
```

- A variável de loop `i` começa em 1 e aumenta 1 em cada iteração.
- A variável `sum` acumula a soma de `i`.
- O loop termina quando `i > 5`, retornando o valor final de `sum`.

**Saída**: `15`

---

### Como funciona

1. **Inicialização**:
   - Cada variável de loop recebe seu valor inicial.

2. **Verificação de rescisão**:
   - No início de cada iteração, a condição de término é verificada. Se for verdade, o loop para e a expressão do resultado é avaliada.

3. **Iteração**:
   - Se a condição de encerramento for falsa, o corpo é executado e as variáveis do loop são atualizadas utilizando suas respectivas expressões de atualização.

---

### Resumo

- A construção `do` fornece uma maneira flexível de implementar loops com múltiplas variáveis e condições de terminação complexas.
- É útil para tarefas que exigem atualizações de estado entre iterações.
- A condição de término determina quando o loop termina e pode retornar um resultado final.

Usando `do`, você pode implementar algoritmos iterativos no Scheme com controle preciso sobre inicialização, atualizações e encerramento. Isso torna `do` uma combinação de um **mecanismo de ligação com escopo** (como `let`) e uma **estrutura de controle iterativa**, permitindo que ele lide com looping e estado temporário de maneira limpa e concisa.