---
title: "Let con nombre o definición local"
type: docs
weight: 5
---
Tanto **llamado `let`** como **local `define`** son herramientas poderosas en Scheme para estructurar su código, pero sirven para diferentes propósitos. Comprender cuándo utilizar cada uno ayuda a crear scripts limpios, modulares y eficientes.

### Descripción general

- **Nombrado `let`**: una construcción que combina vinculación de variables y recursividad en un ámbito localizado, que generalmente se usa para cálculos iterativos o recursivos.
- **Local `define`**: una forma de definir funciones auxiliares o variables dentro del alcance de una función adjunta, haciéndolas reutilizables en diferentes partes de esa función.

---

### Nombrado `let`

#### Características:
1. Combina enlaces de variables y recursividad en una sola construcción.
2. Con alcance en el cuerpo del bloque `let`.
3. Ideal para **recursividad localizada** o procesos iterativos específicos de una sola tarea.

#### Sintaxis
```scheme
(let name ((variable1 value1)
           (variable2 value2))
  body-expression)
```

#### Ejemplo: suma de elementos de una lista
```scheme
(define (sum-list lst)
  (let loop ((remaining lst)
             (accum 0))
    (if (null? remaining)
        accum
        (loop (cdr remaining) (+ accum (car remaining))))))
(sum-list '(1 2 3 4))
```

**Resultado**: `10`

- **Cómo funciona**: la función `loop` se define dentro de `let`, lo que permite llamadas recursivas con enlaces actualizados.

---

### Local `define`

#### Características:
1. Permite la creación de funciones auxiliares o variables que son reutilizables dentro de la función adjunta.
2. Limitado a la función envolvente pero visible en todo su cuerpo.
3. Ideal para modularizar código con múltiples pasos o lógica reutilizable.

#### Sintaxis
```scheme
(define (function-name parameters)
  (define (helper-function parameters)
    body-expression)
  body-expression)
```

#### Ejemplo: procesamiento de múltiples valores
```scheme
(define (process-values a b c)
  (define (square x) (* x x))  ;; Local helper function
  (define (cube x) (* x x x))  ;; Local helper function
  (+ (square a) (cube b) (square c)))
(process-values 2 3 4)
```

**Resultado**: `41` (Calcula \(2^2 + 3^3 + 4^2\))

- **Cómo funciona**: las funciones auxiliares `square` y `cube` son reutilizables dentro de la función `process-values`, lo que permite la lógica modular.

---

### Diferencias clave

| **Aspecto** | **Nombrado `let`** | **Local `define`** |
|--------------------------|--------------------------------------------------|------------------------------------------------|
| **Propósito** | Combina recursividad e iteración de forma localizada. | Define funciones o variables auxiliares reutilizables. |
| **Alcance** | Limitado al cuerpo del bloque `let`.           | Visible en toda la función de cerramiento.      |
| **Reutilizabilidad** | No reutilizable fuera del bloque `let`.             | Reutilizable varias veces dentro de la función.    |
| **Mejor caso de uso** | Recursión localizada o iteración ligada a una sola tarea. | Código modularizador con múltiples pasos reutilizables. |
| **Sintaxis** | Combina vinculación y recursividad en una sola construcción.  | Define explícitamente funciones o variables.      |

---

### Cuándo utilizar `let` con nombre

1. **Lógica de un solo uso**: cuando la recursividad o iteración es específica de un único cálculo.
2. **Encapsulación**: Para evitar agregar nombres de funciones adicionales al espacio de nombres de la función adjunta.
3. **Iteración**: cuando se gestionan variables intermedias en una construcción de bucle.

**Ejemplo: cálculo factorial**
```scheme
(define (factorial n)
  (let fact ((i n)
             (accum 1))
    (if (= i 0)
        accum
        (fact (- i 1) (* accum i)))))
(factorial 5)
```

**Resultado**: `120`

---

### Cuándo utilizar `define` local

1. **Ayudantes reutilizables**: cuando es necesario reutilizar la lógica en varias partes de la función.
2. **Diseño modular**: para dividir cálculos complejos en subtareas más pequeñas con nombres.
3. **Múltiples pasos**: cuando se necesitan múltiples funciones auxiliares para diferentes partes del cálculo.**Ejemplo: Procesamiento de entradas**
```scheme
(define (calculate-values a b)
  (define (add-squares x y)
    (+ (* x x) (* y y)))
  (define (multiply-squares x y)
    (* (* x x) (* y y)))
  (list (add-squares a b) (multiply-squares a b)))
(calculate-values 2 3)
```

**Resultado**: `(13 36)` (Calcula \(2^2 + 3^2\) y \(2^2 \cdot 3^2\))

---

### Combinando declaración y entrada en `let` con nombre

Una de las características más poderosas de un `let` con nombre es su capacidad de combinar **declaración de variable local** y **parámetros de entrada** para la recursividad en una sola construcción. Esto hace que el `let` nombrado sea conciso y expresivo para tareas iterativas o recursivas.

#### Declaración de variable local
En un `let` llamado, los enlaces entre paréntesis actúan como **variables locales** que se inicializan con valores específicos. Estas variables tienen como alcance el cuerpo de `let`.

```scheme
(let loop ((x 1)   ;; Declares x with initial value 1
           (y 2))  ;; Declares y with initial value 2
  (+ x y))         ;; Uses x and y in the body
```

- **`x` y `y`** son variables locales definidas e inicializadas como parte de `let`.

---

#### Parámetros de entrada para recursividad
Las mismas variables también actúan como **parámetros de entrada** para las llamadas recursivas al `let`. Cuando el `let` llamado se llama a sí mismo, actualiza estas variables con nuevos valores.

```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))  ;; Recursive call with new x and y
```

- **Primera iteración**: `x = 1`, `y = 2`
- **Segunda iteración**: `x = 2`, `y = 4`
- **Tercera iteración**: `x = 3`, `y = 8`, y así sucesivamente...

---

#### Equivalente al uso local `define`

Un `let` llamado incluye la inicialización de variables como parte de su sintaxis. Esto elimina la necesidad de un paso separado para configurar los valores iniciales. Los dos ejemplos siguientes son equivalentes:

##### Usando `let` con nombre
```scheme
(let loop ((x 1)
           (y 2))
  (if (> x 5)
    y
    (loop (+ x 1) (* y 2))))
```

##### Usando `define` local
```scheme
(define (outer-function)
  (define (loop x y)
    (if (> x 5)
      y
      (loop (+ x 1) (* y 2))))
  (loop 1 2))  ;; Initial call with x = 1, y = 2
```

Ambos realizan el mismo cálculo, pero el llamado `let` combina la declaración de variables y la configuración de recursividad en una construcción concisa.

---

#### Ventajas de combinar declaración y entrada

1. **Concisión**: el nombre `let` reduce el texto repetitivo al fusionar la inicialización de variables y la recursividad en una sola construcción.
2. **Claridad**: deja claro que la recursividad es local para `let` y está vinculada a una tarea específica.
3. **Encapsulación**: la lógica recursiva permanece autónoma y no contamina el espacio de nombres de la función adjunta.

Esta naturaleza de doble propósito de un `let` llamado, como declaración de variable y mecanismo de entrada recursivo, es lo que lo convierte en una característica poderosa y única en la programación de Scheme.

### Resumen

- Utilice **nombre `let`** para **recursividad localizada** o **iteración**, especialmente cuando la lógica está estrechamente acoplada a una sola tarea.
- Utilice **local `define`** para **modularizar código** con funciones o variables auxiliares reutilizables.

Al comprender sus diferencias, podrá escribir programas Scheme más concisos, organizados y fáciles de mantener.