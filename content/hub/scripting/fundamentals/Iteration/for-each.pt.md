---
title: "para cada um"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_source_sha256: f4fd3b930e681f50286edbc888c747fe8785077655c3c4f326ac505df038e084
translation_lock: true
url: "hub/scripting/fundamentals/Iteration/for-each"
---
A função `for-each` em Scheme é usada para aplicar um procedimento a cada elemento de uma lista (ou múltiplas listas). Ao contrário de `map`, que retorna uma nova lista com os resultados, `for-each` é usado por seus **efeitos colaterais**, como impressão ou atualização de variáveis.

A forma mais simples de `for-each` é assim:

```scheme
(for-each procedure list)
```

- **Procedimento**: Uma função a ser aplicada a cada elemento da lista.
- **Lista**: A lista cujos elementos serão processados.

---

### Exemplo: Imprimir uma lista

```scheme
(define (print-item x)
 (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- Aqui, a função `print-item` é aplicada a cada elemento da lista `(1 2 3 4)`.
- Isso faz com que cada número seja impresso sequencialmente.

**Saída**: `1 2 3 4`

---

### Como funciona

1. **Itera sobre cada elemento**:
 - O procedimento fornecido é executado para cada elemento da lista, em ordem.

2. **Executa efeitos colaterais**:
 - Os efeitos colaterais comuns incluem impressão, registo ou modificação de variáveis externas. Ao contrário de `map`, `for-each` não retorna uma nova lista.

---

#### Exemplo: usando com múltiplas listas

Se forem fornecidas várias listas, `for-each` processa os elementos correspondentes de cada lista.

```scheme
(define (sum-and-print x y)
 (lumi-message (number->string (+ x y))))

(for-each sum-and-print (list 1 2 3) (list 4 5 6))
```

- A função `sum-and-print` soma os elementos correspondentes das duas listas e imprime os resultados.

**Saída**: `5 7 9`

---

### Resumo

- A função `for-each` é útil para realizar efeitos colaterais em cada elemento de uma lista.
- Ao contrário de `map`, `for-each` não produz uma nova lista – concentra-se apenas nos efeitos colaterais do procedimento.
- Pode tratar múltiplas listas simultaneamente, aplicando o procedimento aos elementos correspondentes.

Ao usar `for-each`, pode processar listas de forma eficaz quando o objetivo é executar ações em vez de transformar dados.