---
title: "Recursión simple"
type: docs
weight: 5
---
La recursión es un concepto poderoso en Scheme, donde una función se llama a sí misma para resolver subproblemas más pequeños del problema original. Un patrón de **recursividad simple** implica un caso base para detener la recursividad y un caso recursivo para reducir el problema.

La estructura general de una función recursiva se ve así:

```scheme
(define (function-name args)
  (if (base-condition)
    base-result
    (recursive-call)))
```

- **Condición base**: Detiene la recursividad.
- **Resultado base**: el valor devuelto cuando se cumple la condición base.
- **Llamada recursiva**: una llamada a la función misma con argumentos modificados que acercan el cálculo al caso base.

---

### Ejemplo: suma de números (1 a n)

Una función recursiva simple para calcular la suma de números del 1 al n:

```scheme
(define (sum-to-n n)
  (if (= n 0)                  ; Base case: stop when n is 0
    0                          ; Base result: sum is 0
    (+ n (sum-to-n (- n 1))))) ; Recursive call: sum current n with result of smaller problem
```

---

#### Cómo funciona: desmontaje y montaje

La recursividad funciona dividiendo el problema original en partes más pequeñas. Cada llamada a la función maneja una pieza y pasa el resto. Una vez que se llega al caso más simple, los resultados se vuelven a ensamblar a medida que se completa el cálculo.

#### Seguimiento paso a paso de la suma de n 3

1. **Llamada inicial**: *suma de 3*
   → *(+ 3 (suma de 2))*

2. **Segunda llamada**: *suma de 2*
   → *(+ 2 (suma de 1))*

3. **Tercera llamada**: *suma de 1*
   → *(+ 1 (suma de 0))*

4. **Caso base**: *suma de 0*
   → *0*

---

#### Remontando el resultado final

Una vez que se resuelve el caso más simple, cada capa del cálculo se completa:

1. *suma-n 0* da *0*
2. *suma de n 1* se convierte en *(+ 1 0) = 1*
3. *suma de n 2* se convierte en *(+ 2 1) = 3*
4. *suma de n 3* se convierte en *(+ 3 3) = 6*

---

### Ejemplo: imprimir cada elemento de una lista

Aquí hay una función recursiva simple para imprimir cada elemento en una lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ; Print the first element
      (print-elements (cdr lst)))))             ; Process the rest of the list
```

- **Caso base:** Si la lista está vacía (*¿nula? lst*), detenga la recursividad.
- **Caso recursivo:** Imprime el primer elemento (*car lst*), luego llama a la función en el resto de la lista (*cdr lst*).

#### Ejemplo de uso

```scheme
(print-elements (list 1 2 3))
```

Salida:

- *"1"*
- *"2"*
- *"3"*

Resultado: *"hecho"*

---

#### Cómo funciona

1. La función recupera el primer elemento de la lista usando *car* y lo procesa.
2. Luego se llama a sí mismo con el resto de la lista (*cdr*).
3. Este proceso se repite hasta que la lista esté vacía (*null? lst*).

---

### Resumen

- La recursividad simple consta de:
  1. **Caso base**: Detiene la recursividad.
  2. **Caso recursivo**: Reduce el problema hacia el caso base.
- Cada llamada recursiva hace avanzar el cálculo hasta su finalización.
- Una vez que se alcanza el caso base, los resultados se combinan a medida que se completa la recursividad.

La recursión refleja la estructura del problema y proporciona un flujo lógico y claro. Asegúrese siempre de un caso base para evitar la recursividad infinita.