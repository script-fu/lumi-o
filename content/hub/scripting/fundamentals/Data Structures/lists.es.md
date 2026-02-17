---
title: "Liza"
type: docs
weight: 4
---
En Scheme, una **lista** es una estructura de datos fundamental que se utiliza para agrupar valores. Las listas son colecciones ordenadas de elementos donde cada elemento puede ser de cualquier tipo, incluida otra lista. Las listas se utilizan ampliamente en Scheme tanto para el almacenamiento de datos como para la estructura del programa.

### Ejemplo 1: Lista simple

```scheme
(list 1 2 3)
```

- Crea una lista de tres elementos: `1`, `2` y `3`.

Resultado: **`(1 2 3)`**

---

#### Acceso a elementos de la lista

Se accede a los elementos de una lista mediante los procedimientos `car` y `cdr`:

- `car` recupera el primer elemento de una lista.
- `cdr` recupera el resto de la lista (todo excepto el primer elemento).

#### Ejemplos

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

Resultado:

- `(car my-list)` devuelve `1`
- `(cdr my-list)` devuelve `(2 3)`

---

#### Recursión simple: iteración a través de una lista

Al llamar recursivamente a `car` en el `cdr` de una lista, puede procesar cada elemento uno por uno hasta recorrer la lista. Esto forma la base de muchos algoritmos de procesamiento de listas.

#### Ejemplo: imprimir cada elemento de una lista

Aquí hay una función recursiva simple para imprimir cada elemento en una lista:

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **Caso base:** Si la lista está vacía (`null? lst`), detenga la recursividad.
- **Caso recursivo:** Imprime el primer elemento (`car lst`), luego llama a la función en el resto de la lista (`cdr lst`).

#### Ejemplo de uso

```scheme
(print-elements (list 1 2 3))
```

Salida:

- `"1"`
- `"2"`
- `"3"`

Resultado: "hecho"

---

#### Cómo funciona

1. La función recupera el primer elemento de la lista usando `car` y lo procesa.
2. Luego se llama a sí mismo con el resto de la lista (`cdr`).
3. Este proceso se repite hasta que la lista esté vacía (`null? lst`).

---

### Ejemplo 2: tipos mixtos

Las listas pueden incluir elementos de diferentes tipos, incluidas cadenas, booleanos, números, otras listas o incluso el resultado de expresiones:

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- Esto crea una lista con:
  - Un número (`42`)
  - Una cadena (`"hello"`)
  - Un booleano (`#t`)
  - Otra lista (`(1 2)`)
  - El resultado de una expresión (`(+ 3 4)`, que se evalúa como `7`)

Resultado: **`(42 "hello" #t (1 2) 7)`**

---

Estos ejemplos demuestran la versatilidad de las listas en Scheme, lo que las convierte en una poderosa herramienta para organizar y manipular datos.

### Construyendo listas

El procedimiento `cons` se utiliza para construir una nueva lista combinando un elemento con una lista existente.

```scheme
(cons new-element existing-list)
```

#### Ejemplo

```scheme
(cons 0 (list 1 2 3))
```

- Agrega `0` al principio de la lista `(1 2 3)`.

Resultado: **`(0 1 2 3)`**

---

### Comprobando listas

El procedimiento `list?` comprueba si un valor determinado es una lista.

```scheme
(list? value)
```

#### Ejemplo: ¿lista?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

Resultado:

- `(list? (list 1 2 3))` devuelve `#t` (verdadero)
- `(list? 42)` devuelve `#f` (falso)

---

### Operaciones en listas

Scheme proporciona varios procedimientos integrados para trabajar con listas, que incluyen:

- `length`: Devuelve el número de elementos de una lista.
- `append`: Combina dos o más listas en una.
- `reverse`: Devuelve una nueva lista con elementos en orden inverso.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

Resultado:

- `(length (list 1 2 3))` devuelve `3`
- `(append (list 1 2) (list 3 4))` devuelve `(1 2 3 4)`
- `(reverse (list 1 2 3))` devuelve `(3 2 1)`#### Usando `list-ref`

El procedimiento `list-ref` recupera el elemento en un índice específico de una lista (índice de base cero).

```scheme
(list-ref lst index)
```

- **`lst`**: La lista de la cual recuperar el elemento.
- **`index`**: un índice de base cero que indica qué elemento devolver.

##### Ejemplo: lista-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

Resultado: `30`

---

### Listas anidadas

Las listas en Scheme pueden contener otras listas como elementos, creando una estructura anidada.

#### Ejemplo: creación de una lista anidada

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- Crea una lista de tres elementos, cada uno de los cuales es en sí mismo una lista.

Resultado: **`((1 2) (3 4) (5))`**

---

#### Acceso a datos anidados

Para acceder a elementos dentro de una lista anidada, puede usar combinaciones de `car` y `cdr` para navegar a través de la estructura.

#### Ejemplo: acceso a elementos

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### Explicación

1. **`car nested-list`**:
   - Recupera el primer elemento de `nested-list`, que es `(1 2)`.

2. **`car (car nested-list)`**:
   - Recupera el primer elemento de `(1 2)`, que es `1`.

3. **`cdr (car nested-list)`**:
   - Recupera el resto de `(1 2)`, que es `(2)`.

4. **`car (cdr (car nested-list))`**:
   - Recupera el primer elemento de `(2)`, que es `2`.

---

#### Ejemplo: acceder a elementos de otras sublistas

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

Este enfoque le permite navegar y acceder sistemáticamente a elementos específicos en una lista anidada, lo que proporciona una gran flexibilidad para trabajar con datos jerárquicos.

### Resumen

- Las **Listas** en Scheme son estructuras de datos versátiles y esenciales.
- Utilice `list` para crear una lista, `car` y `cdr` para acceder a elementos y `cons` para construir listas.
- Los procedimientos integrados como `length`, `append`, `reverse` y `list-ref` hacen que las operaciones de listas sean fáciles y eficientes.
- Las listas se pueden anidar, lo que permite estructuras de datos complejas para casos de uso avanzados.