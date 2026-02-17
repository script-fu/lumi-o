---
title: "Depuración"
type: docs
weight: 5
---
En las secuencias de comandos, ninguna función es infalible. Incluso los comandos más confiables pueden fallar cuando se enfrentan a entradas o condiciones inesperadas. Para protegernos contra esto, podemos implementar un sistema de depuración personalizado y adoptar técnicas de programación defensiva. Al incluir funciones estándar con mecanismos de manejo de errores y proporcionar comentarios informativos, podemos hacer que nuestros scripts sean más sólidos y más fáciles de solucionar.

Una parte clave de esta estrategia es utilizar un indicador de depuración global para controlar la salida detallada, lo que nos permite habilitar información de depuración detallada cuando sea necesario y al mismo tiempo mantener la salida limpia durante la ejecución normal.

## Bandera de depuración global

Un indicador de depuración global es una forma sencilla pero eficaz de controlar el nivel de salida de información durante la ejecución del script. Cuando está habilitado, proporciona mensajes de depuración detallados que pueden ser invaluables para rastrear problemas. Cuando está deshabilitado, mantiene la salida concisa para uso en producción.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

De forma predeterminada, la depuración está desactivada. Para habilitar la salida detallada durante el desarrollo, simplemente establezca el indicador en `#t`:

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

También podemos habilitar o deshabilitar temporalmente la depuración para secciones específicas de código usando funciones auxiliares.

### Control de depuración local

Para un control más preciso, podemos activar o desactivar la depuración dentro de partes específicas del script utilizando funciones auxiliares.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

Esto nos permite controlar la depuración dinámicamente:

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## Sistema de mensajería de depuración

Para manejar eficientemente la salida de depuración en Scheme, utilizamos un enfoque estructurado que involucra múltiples funciones auxiliares. Estas funciones garantizan que los mensajes de advertencia y depuración sean claros, legibles y fáciles de mantener.

### Descripción general del sistema de mensajería de depuración

Nuestro sistema de mensajería de depuración consta de los siguientes componentes:

1. `debug-message`: muestra mensajes de depuración cuando la depuración está habilitada.
2. `serialize-item`: convierte varios tipos de datos de Scheme en una representación de cadena.
3. `concat`: concatena varios elementos en una sola cadena.
4. `list->string`: formatea una lista en una cadena legible.
5. `message`: muestra el resultado en la consola de mensajes de Lumi.
6. `warning-message`: muestra mensajes de advertencia cuando las advertencias están habilitadas.

Cada función desempeña un papel en el formato y visualización de mensajes estructurados.

---

### Función de mensaje de depuración

La función `debug-message` es el método principal para mostrar la salida de depuración. Garantiza que los mensajes solo se muestren cuando la depuración esté habilitada.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- La condición `when debug` garantiza que los mensajes aparezcan solo cuando la depuración está habilitada.
- Los mensajes tienen el prefijo `"> "` para mayor claridad.
- La función utiliza `concat` para formatear el contenido del mensaje.
- Finalmente, llama a `message` para enviar el resultado a la consola de mensajes de Lumi.

Uso de ejemplo:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

Con la depuración habilitada, el resultado podría ser:

```scheme
> item: background-layer has tree position : 3
```

### Serialización de datos para mensajes de depuración

Los mensajes pueden contener diferentes tipos de datos, como listas, vectores y números. Para garantizar que tengan el formato adecuado, utilizamos `serialize-item`.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

Uso de ejemplo:

```scheme
(serialize-item '(1 2 3))
```

Salida:

```scheme
list:
1
2
3
```

### Concatenación de mensajes

Para fusionar varios componentes del mensaje en una sola cadena, usamos `concat`.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

Uso de ejemplo:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### Formatear listas como cadenas

La función `list->string` convierte una lista en una cadena formateada.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### Mensajes de advertenciaLa función `warning-message` funciona de manera similar a `debug-message`, pero muestra advertencias incluso cuando la depuración está deshabilitada.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- Garantiza que los mensajes solo se muestren cuando las advertencias están habilitadas (la bandera `warning` está configurada en `common.scm` como `#t`).
- Llama a `concat` para formatear el contenido del mensaje.
- Utiliza `message` para enviar resultados a Lumi.

## Mejora de las funciones estándar

Una vez implementado un sistema de depuración, podemos mejorar nuestra biblioteca de funciones incorporando mensajes detallados. Esto proporciona información sobre los estados de los elementos, los valores de las variables y las llamadas a funciones.

Un ejemplo común es `item-is-valid?`, que envuelve `lumi-item-id-is-valid` para devolver `#t` o `#f`. Si se devuelve `#f`, podemos activar un `warning-message` en el código de llamada, si la entrada no es un número, podemos dar una advertencia en la función.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## Uso práctico

Al desarrollar complementos de Scheme, empaquetar funciones de esta manera reduce significativamente el tiempo de depuración y garantiza un código sólido y fácil de mantener. Con nuestro sistema de depuración implementado, podemos generar un flujo de depuración estructurado en la consola de errores con solo presionar un interruptor.

En esta secuencia de depuración, las llamadas a funciones están marcadas con un asterisco (*), lo que facilita el seguimiento de la ejecución del script y la identificación de fallas, particularmente en complementos complejos. Esta visibilidad nos ayuda a comprender el flujo de operaciones y diagnosticar comportamientos inesperados de manera eficiente.

Un contenedor para nuestra función de mensaje para usar un `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

Ejemplo de uso de `call` en la práctica:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

Ejemplo de una secuencia de depuración mientras se ejecuta un complemento:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

Este registro estructurado proporciona una línea de tiempo clara de llamadas a funciones y cambios de datos, lo que facilita significativamente la depuración y el análisis del rendimiento.

## Conclusión

Al implementar un sistema de depuración estructurado, creamos scripts más seguros y fáciles de mantener que ofrecen información en tiempo real sobre su ejecución.

### Conclusiones clave

- **Control de detalle**: use un indicador de depuración global para administrar los niveles de salida.
- **Proporcione comentarios claros**: incluya funciones estándar con mensajes de depuración informativos.
- **Mejorar la solidez**: maneje las entradas inesperadas con elegancia para evitar errores.
- **Simplifique la solución de problemas**: los mensajes de depuración estructurados facilitan el diagnóstico y la solución de problemas.

Con este enfoque, nuestros scripts "se explican por sí solos" de manera efectiva a medida que procesan datos, lo que reduce la frustración y mejora la eficiencia del flujo de trabajo. La depuración se convierte en una herramienta proactiva en lugar de una tarea reactiva, lo que hace que nuestro proceso de creación de scripts sea más fluido y gratificante.