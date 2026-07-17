---
title: "do"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e5e73b5202354e742509c1e3667fc131bcd6fff9f89b029b05e1798e67953219
url: "hub/scripting/fundamentals/Iteration/do"
---
A função `do` em Scheme é um mecanismo de loop que permite iteração com inicialização, atualização e condições de término. É especialmente útil quando você precisa executar uma sequência de operações um número específico de vezes ou até que uma condição seja atendida.

A forma geral de `do` é:

```scheme
(do ((var1 init1 update1)
     (var2 init2 update2)
     (var3 init3 update3))
    (termination-condition result)
  body)
```

- **Variable:** A(s) variável(is) do loop.
- **Valor inicial:** O valor inicial de cada variável do loop.
- **Expressão de atualização:** A expressão que atualiza a(s) variável(is) do loop ao final de cada iteração.
- **Condição de término:** A condição para parar o loop.
- **Expressão de resultado:** O valor retornado quando o loop termina.
- **Body:** O código executado em cada iteração.

---

### Exemplo: somar os números de 1 a 5

```scheme
(do ((i 1 (+ i 1))      ; Inicializar i em 1, incrementar de 1
     (sum 0 (+ sum i))) ; Inicializar sum em 0, adicionar i a sum
    ((> i 5) sum)       ; Terminar quando i > 5, retornar sum
  (lumi-message (number->string sum))) ; Imprime a soma a cada passo
```

- A variável do loop `i` começa em 1 e incrementa de 1 a cada iteração.
- A variável `sum` acumula a soma de `i`.
- O loop termina quando `i > 5`, retornando o valor final de `sum`.

**Saída**: `15`

---

### Como funciona

1. **Inicialização:**
   - Cada variável do loop recebe seu valor inicial.

2. **Verificação de término:**
   - No início de cada iteração, a condição de término é verificada. Se for verdadeira, o loop para e a expressão de resultado é avaliada.

3. **Iteração:**
   - Se a condição de término for falsa, o corpo é executado e as variáveis do loop são atualizadas com suas respectivas expressões de atualização.

---

### Resumo

- A construção `do` oferece uma forma flexível de implementar loops com múltiplas variáveis e condições de término complexas.
- É útil para tarefas que exigem atualizações de estado entre iterações.
- A condição de término determina quando o loop termina e pode retornar um resultado final.

Com `do`, você pode implementar algoritmos iterativos em Scheme com controle preciso sobre inicialização, atualizações e término. Isso faz de `do` uma combinação de um **mecanismo de ligação com escopo** (como `let`) e uma **estrutura de controle iterativa**, permitindo lidar com loops e estado temporário de forma limpa e concisa.
