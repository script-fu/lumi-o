---
title: "Vectores"
type: docs
weight: 5
---
En Scheme, un vector es otra estructura de datos fundamental que se utiliza para agrupar valores. A diferencia de las listas, los vectores son colecciones de elementos indexados y de tamaño fijo, que proporcionan actualizaciones y acceso aleatorio más rápido. Cada elemento de un vector puede ser de cualquier tipo, incluido otro vector. Los vectores se representan usando # seguido de paréntesis. `#(1 2 3)`

Si bien los vectores y las listas pueden parecer similares, tienen diferentes propósitos en la programación de esquemas:

- Las listas se utilizan más comúnmente para operaciones recursivas y estructuras dinámicas, ya que su implementación de nodo vinculado permite una manipulación eficiente de su inicio y recorrido mediante descomposición recursiva.

- Los vectores, por otro lado, están optimizados para escenarios donde se requiere acceso aleatorio a elementos o actualizaciones en índices específicos, lo que los hace más adecuados para casos de uso como tablas de búsqueda, configuraciones de tamaño fijo u operaciones indexadas críticas para el rendimiento.

En esencia, las listas son la elección natural para algoritmos recursivos y datos de tamaño dinámico, mientras que los vectores brillan cuando los patrones de acceso indexados o de tamaño fijo son primordiales.

### Vectores simples

```scheme
(vector 1 2 3)
```

- Crea un vector de tres elementos: `1`, `2` y `3`.

Resultado: **`#(1 2 3)`**

#### Accediendo a elementos vectoriales

Se accede a los elementos de un vector mediante el procedimiento `vector-ref`, que recupera el elemento en un índice específico (comenzando desde `0`).

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### Iteración: procesamiento de cada elemento en un vector

Puedes iterar a través de un vector usando un bucle o recursividad. Scheme proporciona `vector-length` para determinar el tamaño de un vector. Aquí hay un bucle simple para imprimir cada elemento en un vector:

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **Caso base:** Si el índice `i` alcanza la longitud del vector, detenga el ciclo.
- **Caso recursivo:** Imprime el elemento en el índice `i`, luego incrementa `i`.

#### Ejemplo de uso

```scheme
(print-elements (vector 1 2 3))
```

Resultado:

- `"1"`
- `"2"`
- `"3"`

Resultado: "hecho"

### Vectores mixtos

Los vectores pueden incluir elementos de diferentes tipos, incluidas cadenas, booleanos, números, otros vectores o incluso el resultado de expresiones:

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

Esto crea un vector con:
  - Un número (`42`)
  - Una cadena (`"hello"`)
  - Un booleano (`#t`)
  - Otro vector (`#(1 2)`)
  - El resultado de una expresión (`(+ 3 4)`, que se evalúa como `7`)

Resultado: **`#(42 "hello" #t #(1 2) 7)`**

### Construyendo vectores

Los vectores se crean usando `vector`, o usando `make-vector` para crear un vector de un tamaño fijo con un valor inicial.

```scheme
(make-vector 5 0)
```

Crea un vector de tamaño `5` con todos los elementos inicializados en `0`.

Resultado: `#(0 0 0 0 0)`

### Actualizando vectores

El procedimiento `vector-set!` actualiza un elemento en un vector en un índice específico.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

Resultado: `#(1 42 3)`

### Comprobando vectores

El procedimiento `vector?` comprueba si un valor dado es un vector.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

Resultado:

- `(vector? (vector 1 2 3))` devuelve `#t` (verdadero)
- `(vector? 42)` devuelve `#f` (falso)

### Vectores y comportamiento de paso por referenciaEn Scheme, los vectores son mutables y se pasan por referencia. Esto significa que cuando pasas un vector a una función, la función puede modificar el vector original directamente. Cualquier cambio realizado en el vector dentro de la función también se reflejará fuera de la función. Este comportamiento es útil para compartir y actualizar datos de manera eficiente entre múltiples funciones, pero también requiere precaución para evitar efectos secundarios no deseados.

#### Ejemplo: Modificar un vector en una función

Aquí hay un ejemplo que demuestra cómo los vectores se pasan por referencia y se modifican:

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

Resultado: `#(10 99 30)`

#### Explicación paso a paso

1. **Crear un vector:** `my-vector` se inicializa con los valores `10`, `20` y `30`.
2. **Pasar a una función:** `my-vector` se pasa a `modify-vector` junto con el índice y el nuevo valor a actualizar.
3. **Modificar en función:** El procedimiento `vector-set!` actualiza el valor en el índice especificado directamente en el vector original.
4. **Reflejar cambios:** Dado que los vectores se pasan por referencia, los cambios realizados dentro de la función se reflejan en el vector original.

#### Implicaciones del paso por referencia

- **Rendimiento:** Pasar vectores por referencia es eficiente porque evita copiar estructuras grandes.
- **Efectos secundarios:** Tenga cuidado al compartir vectores entre funciones para evitar modificaciones no deseadas en los datos compartidos.

### Operaciones sobre vectores

Scheme proporciona varios procedimientos integrados para trabajar con vectores, que incluyen:

- `vector-length`: Devuelve el número de elementos de un vector.
- `vector->list`: Convierte un vector en una lista.
- `list->vector`: Convierte una lista en un vector.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

Resultado:

- `(vector-length (vector 1 2 3))` devuelve `3`
- `(vector->list (vector 1 2 3))` devuelve `(1 2 3)`
- `(list->vector (list 1 2 3))` devuelve `#(1 2 3)`

### Vectores anidados

Los vectores en Scheme pueden contener otros vectores como elementos, creando una estructura anidada.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

Crea un vector de tres elementos, cada uno de los cuales es en sí mismo un vector.

Resultado: **`#(#(1 2) #(3 4) #(5))`**

#### Acceso a datos anidados

Para acceder a elementos dentro de un vector anidado, use `vector-ref` varias veces para navegar a través de la estructura.

#### Ejemplo: acceso a elementos

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### Resumen

- Los **Vectores** en Scheme son estructuras de datos indexados de tamaño fijo.
- Utilice `vector` para crear un vector, `vector-ref` para acceder a elementos y `vector-set!` para actualizar elementos.
- Los procedimientos integrados como `vector-length`, `vector->list` y `list->vector` permiten operaciones flexibles.
- Los vectores anidados permiten estructuras de datos jerárquicas y complejas.