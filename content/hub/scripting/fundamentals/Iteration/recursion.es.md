---
title: "Recursión simple"
type: docs
weight: 5
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47fd79f37d5542e30722efaf4f87cd10efb77d825101f2045b191e3640137168
---
En Scheme, la recursión significa que una función se llama a sí misma para resolver subproblemas. Una **recursión simple** tiene caso base para detenerse y caso recursivo que reduce el problema.

Estructura general:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Base Condition:** detiene la recursión.
- **Base Result:** valor en caso base.
- **Recursive Call:** llamada con argumentos reducidos.

---

### Ejemplo: suma de 1 a n

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Caso base: detener cuando n es 0
    0                          ; Resultado base: la suma es 0
    (+ n (sum-to-n (- n 1))))) ; Llamada recursiva: sumar el n actual con el resultado del subproblema menor
```

#### Descomponer y recombinar

La recursión descompone el problema; cada llamada trata una parte. En el caso base, el resultado se recomponen.

#### Paso a paso: sum-to-n 3

1. *sum-to-n 3* → *(+ 3 (sum-to-n 2))*
2. *sum-to-n 2* → *(+ 2 (sum-to-n 1))*
3. *sum-to-n 1* → *(+ 1 (sum-to-n 0))*
4. *sum-to-n 0* → *0*

#### Recombinar el resultado

1. *sum-to-n 0* → *0*
2. *sum-to-n 1* → *1*
3. *sum-to-n 2* → *3*
4. *sum-to-n 3* → *6*

---

### Ejemplo: imprimir cada elemento

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Imprime el primer elemento
      (print-elements (cdr lst)))))             ; Procesa el resto de la lista
```

- **Caso base:** lista vacía → `"done"`.
- **Recursivo:** imprimir `car`, procesar resto con `cdr`.

#### Uso

```scheme
(print-elements (list 1 2 3))
```

Salida: *"1"*, *"2"*, *"3"* — resultado: *"done"*

### Resumen

- Caso base para parar; recursivo para reducir.
- Cada llamada avanza hacia el caso base.
- Siempre un caso base — o recursión infinita.