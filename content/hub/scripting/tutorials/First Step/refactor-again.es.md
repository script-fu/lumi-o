---
title: "Refactorizar nuevamente"
type: docs
weight: 5
---
A medida que crece la biblioteca auxiliar, se vuelve más difícil seguirla de un vistazo. Refactorice nuevamente para mantener cada función pequeña y con un solo propósito.

### Rompiendo la complejidad

Para que la función sea más fácil de seguir y mantener, divídala en funciones más pequeñas y enfocadas. Comience por separar la validación del enrutamiento de mensajes.

### Crear una función de validación

Podemos tomar la parte de la función que valida los argumentos `message` y `output` y moverla a una función separada. De esta manera, la función principal `send-message` no necesita preocuparse por la validación, lo que facilita su seguimiento.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### Simplifica el envío de mensajes

Ahora que la validación se ha movido a una función separada, la función `send-message` puede centrarse simplemente en enviar el mensaje. Será mucho más sencillo, ya que sólo se encarga de la tarea específica de dirigir el mensaje al destino correcto.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Desglose más: separe cada controlador de salida

Cada tipo de salida de mensaje (GUI, Consola de errores, Terminal) se puede mover a su propia función. Esto permite realizar pruebas, modificaciones y posibles extensiones más fácilmente en el futuro.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Reutilizar la validación en cada función de envío

Dado que la validación es una parte importante para garantizar que tanto el mensaje como la salida sean correctos, tiene sentido que cada función `send-*` realice su propia validación. Esto garantiza que, sin importar qué salida se llame, siempre verificamos primero las entradas.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

Vea que eliminamos la validación de la función de enviar mensaje y cambiamos la responsabilidad a cada función de salida individual. Este cambio garantiza que cada destino (GUI, Consola de errores, Terminal) maneje su propia validación, agilizando la función de envío de mensajes y manteniendo la lógica de validación más cerca de donde se necesita.

Este enfoque puede simplificar la función de envío de mensaje, convirtiéndola en un _dispatcher_, al tiempo que garantiza que cada función de envío a* valide el mensaje correctamente antes de procesarlo.

Al trasladar la validación a cada función de envío a*, las hemos hecho reutilizables como funciones independientes. Esto significa que podemos llamar a cualquiera de las funciones de envío a interfaz gráfica de usuario, de envío a consola de error o de envío a terminal directamente sin depender de la función del despachador de envío de mensajes. Cada una de estas funciones ahora maneja completamente su propia lógica y se puede usar de forma independiente en otras partes del código o en otros complementos, lo que hace que su código sea más modular y flexible.

## Beneficios de la refactorización

- **Separación clara de preocupaciones**: cada función ahora maneja solo una responsabilidad, lo que hace que el código sea más fácil de entender.
- **Extensibilidad**: agregar nuevos tipos de salida es sencillo. Simplemente define una nueva función como `send-to-file` o `send-to-logger`, y luego agrega un caso en la declaración `cond`.
- **Reutilizabilidad**: cada una de estas funciones de manejo de salida se puede reutilizar en otra parte de su proyecto o compartirse entre varios complementos.
- **Consistencia**: al reutilizar la función de validación en cada función `send-to-*`, garantiza que todas las salidas estén validadas correctamente, lo que hace que el código sea más sólido.

Una versión de biblioteca refactorizada:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

¿Eso es todo lo que podemos hacer? ¡No! hay más por hacer, sigue leyendo.