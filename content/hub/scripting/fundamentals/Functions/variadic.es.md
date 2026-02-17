---
title: "Funciones variadas"
type: docs
weight: 2
---
**Funciones variables** en Scheme son funciones que aceptan un número variable de argumentos. Estas funciones son muy versátiles y le permiten crear código flexible y reutilizable. En la programación funcional, las funciones variadas simplifican las operaciones que necesitan procesar un número arbitrario de entradas, como sumar una lista de números o concatenar cadenas.

Las funciones variadas son especialmente útiles cuando:

- El número de argumentos no se puede determinar de antemano.
- Debes aplicar la misma operación a una lista dinámica de entradas.
- Redacción de utilidades para agregación o transformación de datos.

### Sintaxis de funciones variables

Las funciones variables se definen utilizando el símbolo `.` antes del último nombre del parámetro. Este último parámetro recopila todos los argumentos restantes en una lista.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** Cualquier argumento fijo requerido que acepte la función.
- **`variadic-parameter`:** Un parámetro especial precedido por `.` que recopila argumentos adicionales como una lista.
- **`body-expression`:** La lógica ejecutada cuando se llama a la función.

### Ejemplos de funciones variables

#### Función variable básica

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **Explicación**:
  - `numbers` recopila todos los argumentos en una lista.
  - `apply` aplica la función `+` a todos los elementos de la lista.

**Uso**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### Función variable con parámetros fijos

Puede combinar parámetros fijos con un parámetro variado para crear funciones más flexibles.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **Explicación**:
  - `prefix` es un argumento fijo.
  - `names` recopila los argumentos restantes en una lista.
  - Cada nombre tiene como prefijo la cadena dada usando `map` y `lambda`.

**Uso**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### Combinando lógica fija y variable

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **Explicación**:
  - `collection-name` es un parámetro fijo.
  - `items` recopila argumentos adicionales en una lista.
  - La función concatena el nombre de la colección y los elementos en una sola cadena.

**Uso**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### Casos de uso avanzados

#### Procesamiento de entradas arbitrarias

Las funciones variadas destacan en el manejo de datos arbitrarios. Aquí hay un ejemplo para sumar solo números positivos:

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- Filtra números no positivos antes de sumar.

**Uso**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### Funciones variadas con lógica recursiva

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **Explicación**:
  - `first` maneja el primer argumento.
  - `rest` recopila los argumentos restantes en una lista.
  - Calcula recursivamente el valor máximo.

**Uso**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### Beneficios de las funciones variables

- **Flexibilidad:** Manejan una amplia gama de casos de entrada.
- **Concisión:** Reduce la necesidad de múltiples funciones sobrecargadas.
- **Operaciones dinámicas:** Habilite el procesamiento de datos en tiempo de ejecución sin conocer el recuento de argumentos de antemano.

### Cuándo utilizar funciones variables

Utilice funciones variadas cuando:

- La función necesita procesar un número desconocido de argumentos.
- Una sola operación se aplica a todas las entradas (por ejemplo, suma, concatenación o mapeo).
- Simplificar la lógica de orden superior con argumentos dinámicos.

Evite funciones variadas cuando:

- La validación de entradas o la verificación de tipos es compleja.
- Los argumentos fijos son suficientes para la lógica requerida.
- La legibilidad se ve comprometida debido a operaciones demasiado complejas.

### ConclusiónLas funciones variadas en Scheme proporcionan un mecanismo sólido para manejar entradas dinámicas. Al comprender su sintaxis y su uso, puede crear scripts flexibles y potentes que se adapten a diversos escenarios. Combinadas con funciones de orden superior, las funciones variadas hacen que su código sea más conciso y expresivo.