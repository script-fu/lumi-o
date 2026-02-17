---
title: "Reelaboración"
type: docs
weight: 7
---
Este paso corrige un comportamiento sutil en el ejemplo de mensajería.

Estábamos pasando la cadena "¡Hola mundo!\n" como mensaje. El "\n" es un tipo especial de carácter, un carácter de "escape". Le indica a la impresión de salida que comience una nueva línea. En Scheme, también forzará que un mensaje enviado a la barra de estado aparezca como un cuadro GUI.

El ayudante `send-to-gui` envía mensajes a un cuadro de diálogo de Lumi.

Actualice el contenido y los destinos del mensaje para que el ejemplo se comporte de forma coherente.

Eliminando el carácter de escape y ampliando las funciones:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Reemplace los números mágicos con las constantes proporcionadas por Lumi (por ejemplo, `MESSAGE-BOX` y `ERROR-CONSOLE`).

Luego divida la validación en dos funciones para que pueda reutilizarse desde múltiples sitios de llamadas.

- (¿es-cadena-válida?) Para verificar que una cadena sea una cadena y no una cadena vacía, dentro de una función de envío a*.
- (¿es-válida-visualización-de-salida?) Para verificar que un destino de salida determinado sea válido, en la función de enviar-mensaje.

Reelaborar la biblioteca:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusión

Al reelaborar nuestra biblioteca de mensajes, la hemos hecho más sólida y confiable. Solucionamos el problema oculto con el carácter de nueva línea, introdujimos constantes para una mayor claridad y ampliamos la funcionalidad agregando soporte para la barra de estado y las salidas del cuadro de diálogo. Además, separar la lógica de validación en funciones más pequeñas y enfocadas garantiza que nuestro código sea más fácil de mantener y ampliar en el futuro.

Esta revisión demuestra cómo pequeños cambios pueden mejorar la estructura general y la funcionalidad de nuestra biblioteca, allanando el camino para una mayor flexibilidad y reutilización a medida que nuestro proyecto crece.