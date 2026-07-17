---
title: "Validación"
type: docs
weight: 4
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 47e586244c9abbe8fac800157a1a855336389bfaf8ed5396c9413f7e364e2fad
url: "hub/scripting/tutorials/First Step/validation"
---
Al crear complementos sólidos, es importante garantizar que nuestras funciones manejen los errores correctamente y funcionen como se espera, incluso en casos de mal uso o entradas inesperadas. La validación ayuda a proteger la integridad de la función y evitar fallos o comportamientos no deseados.

Veamos cómo podemos mejorar la función `send-message` agregando comprobaciones de validación para garantizar que maneje las entradas correctamente.

### Validar entradas

Antes de enviar un mensaje, debemos asegurarnos de que el argumento `output` pasado a la función `send-message` sea válido. Podemos agregar una verificación para confirmar que el destino de salida es uno de los valores esperados (gui, consola de errores o terminal).

Ejemplo:

```scheme
(define (send-message message output)
  ;; Valida el argumento de salida
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
    (cond
      ;; Enviar a la consola de mensajes
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))

      ;; Enviar al cuadro de diálogo GUI
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))

      ;; Enviar a la ventana de terminal
      ((eq? output 'terminal)
         (display message))))

  ;; Restaurar el controlador de mensajes predeterminado a la consola de mensajes
  (lumi-message-set-handler 2))
```

En este ejemplo, usamos `member` para comprobar si el argumento `output` es válido. De lo contrario, la función genera un error con un mensaje claro, lo que evita que los valores no válidos causen problemas.

### Manejar mensajes vacíos

También es útil asegurarse de que el argumento `message` sea válido. Por ejemplo, si se pasa una cadena vacía o #f (falso) como mensaje, la función debería manejar esto correctamente.

Ejemplo de manejo de un mensaje vacío:

```scheme
(define (send-message message output)
  ;; Comprobar si el mensaje está vacío
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

Este enfoque garantiza que la función siempre reciba entradas válidas, mejorando su confiabilidad y evitando comportamientos inesperados.

### Ejemplo de validación combinada

```scheme
;; Función para gestionar la salida de mensajes a varios destinos
(define (send-message message output)

  ;; Validar los argumentos del mensaje y de la salida
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
      (cond
        ;; Enviar a la consola de mensajes
        ((eq? output 'error-console)
           (lumi-message-set-handler 2)
           (lumi-message message))

        ;; Enviar al cuadro de diálogo GUI
        ((eq? output 'gui)
           (lumi-message-set-handler 0)
           (lumi-message message))

        ;; Enviar a la ventana de terminal
        ((eq? output 'terminal)
           (display message)))))

  ;; Restaurar el controlador de mensajes predeterminado a la consola de mensajes
  (lumi-message-set-handler 2))
```

En esta versión:
- La función comprueba primero si `message` está vacío o no es válido. Si el mensaje es válido, pasa a verificar si `output` es uno de los valores aceptados (`gui`, `error-console` o `terminal`).
- Si ambas comprobaciones pasan, el mensaje se envía a la salida correspondiente. De lo contrario, aparece un mensaje de error con una explicación clara.
- Se realiza una verificación adicional para garantizar que el mensaje también sea una cadena.

Esta función de validación combinada mantiene el código más limpio y garantiza que ambas entradas se validen antes de realizar cualquier acción, lo que hace que la función sea más sólida. Tenga en cuenta que también estamos incorporando un sistema de mensajería de depuración. cuando el
El código falla, obtenemos una razón, una razón por la que escribimos nosotros mismos.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```