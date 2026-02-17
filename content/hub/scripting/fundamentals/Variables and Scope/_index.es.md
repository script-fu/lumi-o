---
title: "Variables y alcance"
type: docs
weight: 1
---
En Scheme, la gestión de variables y su alcance es un concepto central para escribir scripts eficientes y fáciles de mantener. Las variables almacenan valores de datos que su secuencia de comandos puede manipular, mientras que el alcance define dónde se puede acceder a esas variables. Comprender cómo definir y utilizar variables de forma eficaz le permite crear código estructurado, reutilizable y sin errores.

### Escritura dinámica

El esquema se escribe dinámicamente: no se declaran tipos por adelantado y una variable puede contener valores de diferentes tipos a lo largo del tiempo.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### El papel de las definiciones de variables y el alcance en el esquema

Definir variables y gestionar su alcance tiene varios propósitos:
- **Organización de datos:** Las variables almacenan información, lo que hace que sus scripts sean más legibles y manejables.
- **Mejora de la reutilización:** Al utilizar variables de ámbito, puede reutilizar secciones de código sin conflictos.
- **Encapsulación:** El alcance localizado evita interacciones no deseadas entre variables en diferentes partes de su secuencia de comandos.
- **Lógica simplificada:** Las variables temporales en un alcance limitado reducen la complejidad en cálculos o flujos de trabajo más grandes.

### Tipos de definiciones de variables y alcance

Scheme proporciona varias construcciones para definir y determinar el alcance de las variables:
- **`let`:** Crea enlaces locales para variables dentro de un bloque de código específico.
- **`let*`:** Una versión secuencial de `let` donde cada vinculación puede depender de las anteriores.
- **Nombrado `let`:** Una construcción poderosa para definir procedimientos o bucles locales recursivos.
- **`define`:** Crea variables o funciones globales a las que se puede acceder desde todo el script.

### Cómo funcionan las definiciones de variables y el alcance

Las definiciones y el alcance de las variables suelen implicar:
1. **Declaración de variables:** Asignar un valor a una variable en un contexto específico.
2. **Alcance limitante:** Controlar dónde se puede acceder a la variable (por ejemplo, dentro de un bloque `let` o globalmente).
3. **Uso de variables:** Acceder y modificar valores de variables para realizar cálculos, operaciones lógicas u procesales.

### Ejemplo: uso de `let` para variables locales

La construcción `let` le permite definir variables temporales que están disponibles solo dentro de un bloque específico:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- Este ejemplo declara `x` y `y` con valores locales y calcula su suma.

### Ejemplo: uso de `define` para variables globales

La construcción `define` crea variables o funciones con alcance global:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- Este script define una constante global `pi` y una función `circle-area` que la usa.

### Comparación de alcance: local versus global

| Característica | Ámbito local (`let`, `let*`) | Alcance global (`define`) |
|------------------|------------------------------------------|-----------------------------------------------|
| **Accesibilidad** | Limitado al bloque en el que está definido | Accesible a lo largo de todo el guión |
| **Encapsulación** | Previene interacciones no deseadas | Puede entrar en conflicto con otras variables definidas globalmente |
| **Caso de uso** | Variables temporales para tareas específicas | Variables o funciones compartidas utilizadas en todo |

### Resumen- **Las definiciones y el alcance de las variables** son fundamentales para organizar y administrar datos en sus scripts de Scheme.
- Utilice **alcance local** (`let`, `let*`, llamado `let`) para encapsular variables temporales y evitar conflictos.
- Utilice **alcance global** (`define`) para funciones reutilizables o constantes compartidas en su secuencia de comandos.
- Una comprensión clara de estas construcciones mejorará la legibilidad, mantenibilidad y confiabilidad de su código.