---
title: "map"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f8a1536159fb582effce405aaa35ff9404de46b545c7db7eea088a72f551a9ee
url: "hub/scripting/fundamentals/Iteration/map"
---
A função `map` em Scheme é usada para aplicar um procedimento a cada elemento de uma lista (ou várias listas) e **retornar uma nova lista** contendo os resultados. Isso a torna ideal para transformar dados.

A forma mais simples de `map` é:

```scheme
(map procedure list)
```

- **Procedimento:** Uma função a ser aplicada a cada elemento da lista.
- **Lista:** A lista cujos elementos serão transformados.

---

### Exemplo: dobrar cada elemento

```scheme
(define (double x)
  (* x 2))

(map double (list 1 2 3 4))
```

- Aqui, a função `double` é aplicada a cada elemento da lista `(1 2 3 4)`.
- O resultado é uma nova lista com cada elemento dobrado.

**Saída**: `(2 4 6 8)`

---

### Como funciona

1. **Cria uma nova lista:**
   - `map` aplica o procedimento fornecido a cada elemento da lista e coleta os resultados em uma nova lista.

2. **Transforma dados:**
   - É usado principalmente para transformações de dados, em vez de efeitos colaterais.

---

#### Exemplo: com várias listas

Se várias listas forem fornecidas, `map` processa os elementos correspondentes de cada lista.

```scheme
(define (sum x y)
  (+ x y))

(map sum (list 1 2 3) (list 4 5 6))
```

- A função `sum` soma os elementos correspondentes das duas listas e retorna os resultados como uma nova lista.

**Saída**: `(5 7 9)`

---

### Resumo

- A função `map` é uma ferramenta poderosa para transformar listas aplicando um procedimento a cada elemento.
- Diferentemente de `for-each`, `map` **produz uma nova lista** com os resultados da aplicação do procedimento.
- Suporta várias listas, permitindo operações elemento a elemento entre elas.

Com `map`, você pode criar versões transformadas dos seus dados com eficiência, mantendo as listas originais inalteradas.
