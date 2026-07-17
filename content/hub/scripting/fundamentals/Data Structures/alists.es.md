---
title: "Listas de asociación (Alists)"
type: "docs"
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d57c000eccd152bbe703156a7d54d2baa9e51cd7b22f3fd1f53c8c820fa5aae5
url: "hub/scripting/fundamentals/Data Structures/alists"
---
Una **lista de asociaciones** (o **alista**) es una estructura de datos fundamental en Scheme que se utiliza para representar colecciones de pares clave-valor. Se implementa como una lista de pares, donde cada par asocia una clave (normalmente un símbolo) con un valor. Las listas Al son simples, flexibles y adecuadas para conjuntos de datos pequeños y medianos.

### Estructura de una lista de asociaciones

Una lista alista es una lista donde cada elemento es un **par** (construido con `cons`). Cada par consta de:

- **Clave**: el primer elemento (normalmente un símbolo).
- **Valor**: El segundo elemento, que puede ser de cualquier tipo de datos.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **Clave**: `'name`, `'age`, `'city`
- **Valor**: `"Alice"`, `30`, `"Paris"`
- **Estructura**: Una lista de pares:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Creando una lista Al

Puede crear una lista al construir pares manualmente o mediante programación usando `cons`.

#### Usando la comilla simple (`'`)

La comilla simple (`'`) es una abreviatura de **comillas**, lo que impide que Scheme evalúe la expresión. Esto lo hace ideal para crear listas estáticas donde todas las claves y valores están codificados.

```scheme
;; Definir manualmente una alista
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Añadir programáticamente un nuevo par
(define updated-alist (cons '(country . "France") alist))
```

**Resultado**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Usando las comillas inversas (`` ` ``) y coma (`,`)

El operador de comillas invertidas (`` ` ``) es similar a la comilla simple, pero permite insertar dinámicamente expresiones evaluadas con la coma (`,`). Esto es útil para crear listas donde las claves o valores se calculan en tiempo de ejecución.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**Resultado**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### Ejemplo de comparación

Lista estática usando `'`:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

Lista dinámica usando `` ` `` y `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Accediendo a datos en una lista Al

Para recuperar un valor de una lista, puede usar la función `assoc`, que busca un par por su clave.

```scheme
(assoc 'name alist)   ; Devuelve (name . "Alice")
(assoc 'country alist) ; Devuelve #f (clave no encontrada)
```

### Extrayendo el valor

Una vez que recupere un par usando `assoc`, use `cdr` para extraer el valor:

```scheme
(cdr (assoc 'name alist))   ; Devuelve "Alice"
```

### Resumen de características clave

- **Comilla única (`'`)**: crea una lista estática donde todos los elementos son datos literales.
- **Cotización inversa (`` ` ``)**: permite crear alists dinámicamente mezclando elementos estáticos con expresiones evaluadas (mediante `,`).
- **Notación de puntos (`.`)**: Se utiliza para construir pares, asociando una clave con un valor en una lista alista.