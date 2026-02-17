---
title: "Símbolos"
type: docs
weight: 6
---
Los símbolos son uno de los tipos de datos principales en Scheme y representan identificadores únicos e inmutables. Se utilizan principalmente como claves, marcadores o marcadores de posición en programas, lo que los hace esenciales para escribir código limpio y expresivo.

Un símbolo en Scheme es similar a una cadena, pero se diferencia en que los símbolos son **únicos** y **atómicos**. Esto significa que se garantiza que dos símbolos con el mismo nombre serán el mismo objeto, lo que permite realizar comprobaciones de igualdad rápidas y un uso eficiente en estructuras de datos.

### Sintaxis

Un símbolo se escribe como una secuencia de caracteres:

- Comienza con una letra, seguida de letras, dígitos o caracteres especiales como `-`, `+` o `*`.
- Los símbolos distinguen entre mayúsculas y minúsculas de forma predeterminada.

Ejemplos:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## Creando símbolos

Los símbolos normalmente se crean usando el operador **comilla** (`'`), que le indica a Scheme que trate el nombre como un símbolo en lugar de evaluarlo como una variable o función.

### Ejemplo

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

También puede crear símbolos mediante programación utilizando el procedimiento `string->symbol`, que convierte una cadena en un símbolo.

```scheme
(string->symbol "dynamic-symbol")
```

**Resultado**: `'dynamic-symbol`


## Comparando símbolos

Debido a que los símbolos son únicos, puedes compararlos eficientemente usando `eq?`.

### Ejemplo

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

Esto hace que los símbolos sean ideales para usar como claves en estructuras de datos o marcadores en su código.

## Usando símbolos

Los símbolos se utilizan a menudo en Scheme para:

1. **Claves en listas de asociaciones:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **Identificadores en el código:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## Procedimientos para trabajar con símbolos

Scheme proporciona procedimientos integrados para trabajar con símbolos:

| Procedimiento | Descripción |
|--------------------|-----------------------------------------------------------------------------|
| **`symbol?`** | Comprueba si un objeto es un símbolo.                                            |
| **`eq?`** | Compara dos símbolos de identidad (comparación rápida).                       |
| **`symbol->string`** | Convierte un símbolo en una cadena (útil para visualización o depuración).          |
| **`string->symbol`** | Convierte una cadena en un símbolo (útil para la creación dinámica de identificadores). |

### Ejemplos

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## Resumen

Los símbolos son una forma ligera y eficaz de representar identificadores, claves y marcadores en Scheme. Su inmutabilidad y sus rápidos controles de identidad los hacen ideales para muchas tareas de programación. Comprender cómo utilizar símbolos de forma eficaz mejorará su capacidad para escribir código Scheme limpio y expresivo.