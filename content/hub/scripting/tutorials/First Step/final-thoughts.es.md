---
title: "Pensamientos finales"
type: docs
weight: 10
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 1e11221cb3561517da42909b8f115febb9d7430d2715ac9f1b5f4c42d8b80746
url: "hub/scripting/tutorials/First Step/final-thoughts"
---
Ahora tiene un complemento de procedimiento de trabajo y una pequeña biblioteca auxiliar. Esta serie presentó los patrones principales que usarás en la mayoría de los scripts de Lumi:

- Funciones: Los componentes básicos de nuestros complementos.
- Refactorización: mejorar la estructura del código manteniendo la funcionalidad.
- Bibliotecas de Códigos: Centralizando funciones reutilizables para mantener nuestro código limpio y modular.
- Técnicas de Validación: Asegurar que las entradas sean válidas antes de ejecutar nuestra lógica central.

También vio los conceptos básicos del uso de Git para realizar un seguimiento de los cambios y mantener una estructura de proyecto limpia. Ese flujo de trabajo facilita la iteración sin perder versiones funcionales.

Aquí está la versión final de nuestro código de complemento principal:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Código de biblioteca:

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

Al refactorizar los asistentes de mensajería en una pequeña biblioteca, el complemento permanece enfocado en la intención y la biblioteca contiene los detalles de implementación. La validación y el enrutamiento coherente de mensajes mantienen los errores predecibles.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Próximos pasos:

- Mueva los ayudantes reutilizables a un archivo de biblioteca dedicado.
- Mantenga los complementos pequeños y nombre los procedimientos para lo que hacen.
- Agregar validación en los límites (entradas, rutas de archivos, opciones de menú).

Mantenga el resultado final como dos archivos en su repositorio de complementos:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`