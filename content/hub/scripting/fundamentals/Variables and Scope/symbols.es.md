---
title: "Símbolos"
type: docs
weight: 6
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4ae0cc2f5749cbe997d6fa25315ee3fe54646eb065b4dba0114778c75a889ae5
---
Los símbolos son uno de los tipos de datos principales en Scheme y representan identificadores únicos e inmutables. Se utilizan principalmente como claves, marcadores o marcadores de posición en programas, lo que los hace esenciales para escribir código limpio y expresivo.

Un símbolo en Scheme es similar a una cadena, pero se diferencia en que los símbolos son **únicos** y **atómicos**. Esto significa que se garantiza que dos símbolos con el mismo nombre serán el mismo objeto, lo que permite realizar comprobaciones de igualdad rápidas y un uso eficiente en estructuras de datos.

### Sintaxis

Un símbolo se escribe como una secuencia de caracteres:

- Comienza con una letra, seguida de letras, dígitos o caracteres especiales como `-`, `+` o `*`.
- Los símbolos distinguen entre mayúsculas y minúsculas de forma predeterminada.

Ejemplos:

```scheme
'hello       ; Un símbolo llamado `hello`
'foo-bar     ; Un símbolo llamado `foo-bar`
'*special*   ; Un símbolo llamado `*special*`
```

## Creando símbolos

Los símbolos generalmente se crean usando el operador **comilla** (`'`), que le indica a Scheme que trate el nombre como un símbolo en lugar de evaluarlo como una variable o función.

### Ejemplo

```scheme
'my-symbol   ; Crea el símbolo `my-symbol`
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
(eq? 'apple 'apple)   ; #t (mismo símbolo)
(eq? 'apple 'orange)  ; #f (símbolos distintos)
```

Esto hace que los símbolos sean ideales para usar como claves en estructuras de datos o marcadores en su código.

## Usando símbolos

Los símbolos se utilizan a menudo en Scheme para:

1. **Claves en listas de asociaciones:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Devuelve (name . "Alice")
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
(symbol? 'example)            ; #t (verdadero: es un símbolo)
(symbol->string 'example)     ; Resultado: "example"
(string->symbol "new-symbol") ; Resultado: 'new-symbol
```

## Resumen

Los símbolos son una forma ligera y eficaz de representar identificadores, claves y marcadores en Scheme. Su inmutabilidad y sus rápidos controles de identidad los hacen ideales para muchas tareas de programación. Comprender cómo utilizar símbolos de forma eficaz mejorará su capacidad para escribir código Scheme limpio y expresivo.