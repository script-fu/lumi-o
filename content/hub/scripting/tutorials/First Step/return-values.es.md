---
title: "Valores de retorno"
type: docs
weight: 8
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 80a8f61c6fc7f6b86167f7489f61558b49f3d1d2b7e1e5236406cbca31ff611e
---
Los valores de retorno son importantes porque le permiten controlar el flujo sin un estado adicional. En Scheme, la última expresión evaluada se convierte en el valor de retorno.

Esta página utiliza los ayudantes de validación del ejemplo de mensajería para mostrar cómo los valores de retorno explícitos facilitan la redacción del código.

### ¿Qué es un valor de retorno?

En Scheme, el valor de retorno de una función está determinado por la última expresión que evalúa la función. Esto significa que cualquier cosa que se evalúe en la última línea de código de la función se devolverá como resultado de la función. Si no se devuelve ningún valor explícitamente, la función devuelve `#f` (falso) o `undefined`.

Revisemos la función de validación (¿es-cadena-válida?)

```scheme
;; Propósito: Comprueba que el mensaje es una cadena no vacía
(define (is-valid-string? message)
  ;; Comprobar si el mensaje es una cadena no vacía
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

En esta función, si el mensaje no es válido, se genera un error. Sin embargo, si el mensaje es válido, no se proporciona ningún valor de retorno explícito y la función devuelve `#f` de forma predeterminada.

### Hacer explícitos los valores de retorno

Podemos mejorar esto haciendo que el valor de retorno sea más explícito. Por ejemplo, podríamos devolver `#t` (verdadero) si el mensaje es válido:

```scheme
;; Propósito: Comprueba que el mensaje se envía a una salida válida
(define (is-valid-output-display? output)
  ;; Comprobar si la salida es uno de los destinos de visualización esperados
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

En esta versión, la función devolverá `#t` cuando el mensaje sea válido, proporcionando un resultado claro. Esto permite que la función se use de manera más flexible en otros contextos donde se necesita un resultado booleano.

### Uso eficaz de los valores de retorno

Al decidir qué devuelven nuestras funciones, podemos hacerlas más predecibles y útiles. Devolver valores como `#t`, `#f` o un resultado específico nos da más control sobre cómo interactúa la función con el resto del código. Por ejemplo, puede utilizar el valor de retorno para tomar más decisiones en la función que llama o pasarlo como argumento a otra función.

A continuación se muestra un ejemplo sencillo del uso de un valor de retorno para controlar el flujo de la lógica:

```scheme
;; Propósito: Envía un mensaje al destino de salida adecuado
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

En este caso, (enviar-mensaje) se basa en el valor de retorno de (¿is-valid-output-display?) para decidir si continuar.
La declaración condicional `cond` se omitirá si falla la primera prueba. Además, observe cómo se lee de una manera bastante natural, si la visualización de salida es válida.

## Si la lógica de la declaración en el esquema

Antes del ejemplo de la biblioteca refactorizada, aquí hay una revisión rápida de la lógica condicional. Scheme utiliza `if` para elegir entre dos caminos.

Aquí hay una forma simple de declaración `if`:

```scheme
(if (conditional test)
  do if true
  do if false)
```

Esta estructura verifica la condición y, si la condición es verdadera, ejecuta la primera acción. Si la condición es falsa, ejecuta la segunda acción.

En los casos en los que necesite realizar varias acciones cuando la condición sea verdadera o falsa, puede usar `begin` para agruparlas:

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

Esto le permite manejar situaciones más complejas, donde es necesario ejecutar múltiples expresiones o declaraciones según el resultado de la prueba condicional.

Bien, aquí está el código de la biblioteca con valores de retorno incrustados y utilizados para controlar el proceso de ejecución.

### Refactorizado con valores de retorno

```scheme
;; Propósito: Envía un mensaje a la barra de estado, devuelve #t si tiene éxito
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Propósito: Envía un mensaje al cuadro de diálogo, devuelve #t si tiene éxito
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Propósito: Envía un mensaje a la consola de errores, devuelve #t si tiene éxito
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Propósito: Envía un mensaje al terminal, devuelve #t si tiene éxito
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Propósito: Envía un mensaje a la salida adecuada y devuelve #t si tiene éxito
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Propósito: Comprueba que el mensaje es una cadena no vacía, devuelve #t si es válido
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Propósito: Comprueba que la salida es un destino válido, devuelve #t si es válida
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusión

Los valores de retorno son una parte fundamental para hacer que las funciones sean flexibles y reutilizables. Al decidir cuidadosamente qué debe devolver cada función, podemos asegurarnos de que nuestras funciones interactúen bien entre sí y proporcionen información útil al resto del código. Ya sea devolviendo `#t` o `#f`, o algo más específico, los valores devueltos nos brindan una manera de controlar el flujo de nuestros programas y manejar varios resultados.