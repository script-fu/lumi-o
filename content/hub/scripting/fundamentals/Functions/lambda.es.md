---
title: "Funciones Lambda"
type: docs
weight: 1
---
Las **funciones Lambda** en Scheme son funciones anónimas, lo que significa que son funciones sin nombre. Estas funciones se definen en línea y normalmente se utilizan para operaciones breves y únicas. La construcción `lambda` es una poderosa herramienta en programación funcional, que le permite crear una lógica concisa y flexible sobre la marcha.

Las funciones Lambda son especialmente útiles cuando:

- Necesita una pequeña función para un propósito específico y temporal.
- Pasar funciones como argumentos a funciones de orden superior como `map`, `filter` o `fold`.
- Devolver funciones de otras funciones.

### Sintaxis de funciones Lambda

Las funciones Lambda se pueden definir por sí solas...

```scheme
(lambda (parameter1 parameter2 ...)
  body-expression)
```

...o invocado inmediatamente:

```scheme
((lambda (parameter1 parameter2 ...)
   body-expression)
 argument1 argument2 ...)
```

- **`parameter1, parameter2, ...`:** Los parámetros que acepta la función.
- **`body-expression`:** La lógica ejecutada cuando se llama a la función.
- **Invocación inmediata:** El segundo formulario muestra que se llama inmediatamente a una lambda con argumentos.

### Ejemplos de funciones Lambda

#### Uso de Lambda para cálculos simples

```scheme
((lambda (x y) (+ x y)) 3 5)  ; Returns 8
```

Aquí:

- Se crea una función lambda para sumar dos números (`x` y `y`).
- La función se invoca inmediatamente con los argumentos `3` y `5`.

#### Funciones Lambda en línea

El siguiente ejemplo demuestra cómo usar `for-each` con una función con nombre y una función lambda:

**Usando una función con nombre:**

```scheme
(define (print-item x)
  (lumi-message (number->string x)))

(for-each print-item (list 1 2 3 4))
```

- **Explicación**:
  - `print-item` es una función con nombre que convierte un número en una cadena (`number->string`) y lo imprime usando `lumi-message`.
  - `for-each` aplica `print-item` a cada elemento de la lista `(1 2 3 4)`.

**Salida**: 1 2 3 4

**Usando una función Lambda:**

La misma lógica se puede escribir en línea con una función lambda, evitando la necesidad de una función con nombre independiente:

```scheme
(for-each (lambda (x) (lumi-message (number->string x)))
  (list 1 2 3 4))
```

- **Explicación**:
  - El `(lambda (x) (lumi-message (number->string x)))` define una función anónima.
  - Esta función se aplica a cada elemento de la lista `(1 2 3 4)` por `for-each`.

**Salida**: 1 2 3 4

#### Funciones Lambda como argumentos

Las funciones Lambda a menudo se pasan directamente a funciones de orden superior como `map` o `filter`.

#### Cuadrar una lista de números

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
```

- La función `lambda` eleva al cuadrado cada elemento de la lista.
- La función `map` aplica el `lambda` a cada elemento.

#### Funciones Lambda como valores de retorno

Puede devolver una función lambda desde otra función para crear un comportamiento dinámico.

#### Generando una función sumadora

```scheme
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)  ; Returns 15
```

- `make-adder` genera una nueva función lambda que agrega un número específico (`n`).
- La lambda devuelta se almacena en `add5`, que agrega `5` a su entrada.

#### Usando Lambda con `let`

Las lambdas se utilizan a menudo con `let` para crear funciones temporales de ámbito local.

#### Lambda local para adición

```scheme
(let ((add (lambda (a b) (+ a b))))
  (add 3 4))  ; Returns 7
```

- `let` vincula una función lambda al nombre `add`.
- Luego, la lambda se usa como una función normal dentro del alcance `let`.

#### Combinando Lambdas con funciones de orden superior

Las lambdas brillan cuando se combinan con funciones de orden superior para realizar transformaciones de datos complejas.

#### Filtrado de números pares

```scheme
(filter (lambda (x) (= (modulo x 2) 0)) '(1 2 3 4 5 6))  ; Returns (2 4 6)
```- El `lambda` comprueba si un número es par.
- La función `filter` utiliza lambda para mantener solo números pares de la lista.

### Beneficios de las funciones Lambda

- **Concisión:** Lambdas reduce el código repetitivo al eliminar la necesidad de definir funciones con nombres separados.
- **Flexibilidad:** Puede definirlos y usarlos donde sea necesario, lo que hace que el código sea más modular.
- **Legibilidad mejorada:** Para tareas breves y específicas, las lambdas aclaran la intención sin saturar el código con funciones con nombre adicionales.

### Cuándo utilizar funciones Lambda

Utilice funciones lambda cuando:

- La lógica es breve y autónoma.
- La función sólo es necesaria temporalmente o dentro de un ámbito específico.
- Estás trabajando con funciones de orden superior como `map`, `filter` o `reduce`.

Evite el uso de lambdas para lógica compleja de varias líneas, ya que esto puede reducir la legibilidad. Para operaciones más extensas, utilice una función con nombre en su lugar.

### Conclusión

Las funciones Lambda en Scheme proporcionan una forma concisa y potente de definir funciones anónimas para tareas específicas. Su flexibilidad y facilidad de uso los convierten en una herramienta esencial para cualquier programador de Scheme. Comprender cómo utilizar `lambda` de forma eficaz le ayudará a escribir scripts más limpios, modulares y eficientes.