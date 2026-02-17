---
title: "Funciones"
type: docs
weight: 7
---
Las funciones son un concepto central en Scheme y proporcionan los medios para encapsular la lógica, permitir la reutilización del código y estructurar los scripts de manera efectiva. Con funciones, puede crear scripts modulares y fáciles de mantener que manejen una amplia gama de tareas, desde operaciones básicas hasta flujos de trabajo avanzados en Lumi.

Esta sección sirve como una introducción a las funciones de Scheme y sienta las bases para comprender sus tipos, definiciones y usos. Las secciones siguientes profundizarán en tipos de funciones específicas y sus capacidades únicas.

## Sintaxis y expresiones mínimas

El código del esquema está compuesto de **expresiones**. Una expresión se evalúa como un valor. La sintaxis es uniforme: los paréntesis forman una llamada, con el operador o el nombre de la función primero.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Como todo es una expresión, el flujo de control encaja naturalmente en el mismo estilo que las llamadas a funciones.

## Por qué son importantes las funciones

Las funciones desempeñan un papel fundamental en Scheme por varias razones:

- **Reutilización del código:** Evite la repetición encapsulando la lógica en componentes reutilizables.
- **Modularidad:** Divida las tareas complejas en partes más pequeñas y manejables.
- **Comportamiento dinámico:** Acepte parámetros para manejar varias entradas o adaptarse a diferentes situaciones.
- **Abstracción superior:** Simplifique la lógica centrándose en "qué" hace una función en lugar de "cómo" lo hace.

## Descripción general de los tipos de funciones

Scheme ofrece una variedad de construcciones de funciones, cada una adecuada para casos de uso específicos:

1. **Funciones con nombre**
   Estas son funciones estándar definidas con `define`. Forman la columna vertebral de la mayoría de los guiones.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **Funciones anónimas**
   También conocidas como **funciones lambda**, son funciones sin nombre definidas en línea para un uso único.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **Funciones de orden superior**
   Funciones que toman otras funciones como argumentos o devuelven funciones como resultados, lo que permite abstracciones poderosas como mapeo, filtrado y reducción.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## Sintaxis general de funciones

Las funciones en Scheme tienen una sintaxis simple y consistente:

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** El nombre de la función.
- **`parameter1, parameter2, ...`:** Los argumentos que toma la función.
- **`body-expression`:** La lógica ejecutada cuando se llama a la función.

Ejemplo:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## Efectos secundarios y estado global

En Lumi, muchos procedimientos útiles tienen **efectos secundarios**: modifican una imagen, cambian un elemento de diseño, escriben un archivo o muestran la salida.

- Aislar los efectos secundarios en procedimientos pequeños y con nombres claros.
- Evite cambiar el contexto global a menos que sea necesario.
- Cuando cambies el contexto (colores, pinceles, etc.), envuelve el trabajo con `lumi-context-push` y `lumi-context-pop` para restaurar el estado del usuario.