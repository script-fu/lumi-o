---
title: "dejar"
type: docs
weight: 4
---
El nombre `let` se utiliza porque refleja sus orígenes matemáticos de introducir enlaces temporales, como en _"Let \( x = 2 \) and \( y = 3 \)"_.

Una declaración `let` en Scheme es una **construcción vinculante** que se utiliza para definir variables dentro de un alcance localizado. Le permite crear enlaces temporales para variables y luego ejecutar un bloque de código usando esos enlaces. Esto es particularmente útil para mantener el código modular y evitar la contaminación variable global.

Hay tres formas principales de `let` en Scheme:

- **`let`**: Let estándar para crear enlaces locales simples.
- **`let*`**: let secuencial, donde las vinculaciones pueden depender de los resultados de vinculaciones anteriores.
- **Nombrado `let`**: una forma especial de `let` que crea bucles recursivos o procedimientos con nombre.

En su forma más simple, `let` crea enlaces de variables locales y evalúa una expresión con esos enlaces.

```scheme
(let ((variable1 value1)
      (variable2 value2))
  expression)
```

- **Vinculaciones**: una lista de pares donde cada par asigna un `value` a un `variable`.
- **Expresión**: El cuerpo de `let`, que puede utilizar las variables definidas localmente.

### Ejemplo

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Esto define dos variables locales, `x` (10) y `y` (20).
- Luego calcula `(+ x y)` usando estas variables.

**Resultado**: `30`

---

## La construcción `let*`

La construcción `let*` es similar a `let`, pero los enlaces se evalúan **secuencialmente**. Esto significa que las vinculaciones posteriores pueden depender de las anteriores.

```scheme
(let* ((variable1 value1)
       (variable2 expression-using-variable1))
  expression)
```

### Ejemplo

```scheme
(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

- El primer enlace asigna `10` a `x`.
- El segundo enlace calcula `y` como `(+ x 5)`, usando el valor de `x`.
- El cuerpo calcula `(* x y)`.

**Resultado**: `150`

---

## Nombrado `let`

Un `let` con nombre es una forma especial de `let` que proporciona un nombre para el bloque `let`, convirtiéndolo en un procedimiento recursivo. Esto es útil para crear bucles o cálculos recursivos.

```scheme
(let name ((variable1 initial-value1)
           (variable2 initial-value2))
  body-expression)
```

- **Nombre**: El bloque `let` recibe un nombre, lo que efectivamente define una función.
- **Enlaces**: Valores iniciales para variables, similares a un `let` estándar.
- **Cuerpo**: La expresión puede llamar al `let` nombrado de forma recursiva.

### Ejemplo: bucle con `let` con nombre

```scheme
(let loop ((n 5)
           (result 1))
  (if (= n 0)
      result
      (loop (- n 1) (* result n))))
```

- La función `loop` comienza con `n = 5` y `result = 1`.
- Si `n` es `0`, devuelve `result`.
- En caso contrario, se llama a sí mismo de forma recursiva con `n - 1` y `result * n`.

**Resultado**: `120` (Factorial de 5)

---

## Tabla resumen| Construir | Descripción | Caso de uso |
|------------|------------------------------------------|--------------------------------------------------------------------|
| **`let`** | Define enlaces locales para variables.    | Utilícelo cuando todos los enlaces sean independientes y no dependan unos de otros.     |
| **`let*`** | Define enlaces locales secuenciales.       | Úselo cuando las vinculaciones posteriores dependan de los resultados de las anteriores.           |
| **Nombrado `let`** | Define procedimientos locales recursivos. | Úselo para bucles, cálculos iterativos o recursividad en un contexto local. |

---

## Ejemplos

### Usando `let` para el cálculo local

```scheme
(let ((x 2)
      (y 3))
  (+ (* x x) (* y y)))
```

**Resultado**: `13` (Calcula `x² + y²`)

---

### Uso de `let*` para dependencias secuenciales

```scheme
(let* ((x 2)
       (y (* x x))
       (z (* y x)))
  z)
```

**Resultado**: `8` (Calcula `x³`)

---

### Uso de `let` con nombre para cálculo recursivo

```scheme
(let factorial ((n 5)
                (result 1))
  (if (= n 0)
      result
      (factorial (- n 1) (* result n))))
```

**Resultado**: `120` (Factorial de 5)

---

Al utilizar `let`, `let*` y el nombre `let`, Scheme permite la programación modular, recursiva y secuencial con reglas de alcance claras.