---
title: "Listas de asociaciones (Alistas)"
type: docs
weight: 6
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
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**Resultado**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### Usando las comillas inversas (`` ` ``) and Comma (`,`)

El operador de comillas invertidas (`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`). Esto es útil para crear listas donde las claves o valores se calculan en tiempo de ejecución.

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

Lista dinámica usando `` ` `` and `,`:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Accediendo a datos en una lista Al

Para recuperar un valor de una lista, puede usar la función `assoc`, que busca un par por su clave.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### Extrayendo el valor

Una vez que recupere un par usando `assoc`, use `cdr` para extraer el valor:

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### Resumen de características clave

- **Comilla única (`'`)**: crea una lista estática donde todos los elementos son datos literales.
- **Cotización inversa (`` ` ``)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **Notación de puntos (`.`)**: Se utiliza para construir pares, asociando una clave con un valor en una lista alista.