---
title: "Biblioteca de mensajería"
type: docs
weight: 6
---
Con el tiempo, lo que comenzó como una única función para enviar mensajes ha evolucionado hasta convertirse en una colección de funciones relacionadas. Estas funciones ahora forman la base de una **Biblioteca de mensajería**, diseñada para manejar la salida a diferentes destinos, como la GUI, la Consola de errores y la Terminal.

### ¿Por qué una biblioteca de mensajería?

A medida que nuestras necesidades crecen, el manejo de mensajes en múltiples salidas requiere un enfoque más modular y extensible. En lugar de que una sola función lo haga todo, hemos dividido el proceso en componentes reutilizables, lo que permite una mayor flexibilidad. Esta biblioteca ahora se puede utilizar como una herramienta de mensajería de uso general de la que se pueden tomar prestados otros complementos o funciones.

### ¿Qué hace la biblioteca de mensajería?

La Biblioteca de mensajería incluye actualmente las siguientes funciones:

- **send-to-gui**: envía mensajes al cuadro de diálogo de Lumi GUI.
- **send-to-error-console**: Envía mensajes a la Lumi Error Console.
- **send-to-terminal**: Envía mensajes a la ventana del terminal.
- **enviar mensaje**: una función de despachador que dirige los mensajes a la salida adecuada.
- **validate-message**: Garantiza que el mensaje y la salida sean válidos antes de enviarlos.

### Ampliando la biblioteca

La **Biblioteca de mensajería** se puede ampliar fácilmente para admitir salidas adicionales. Por ejemplo:

- **enviar a archivo**: guarda mensajes en un archivo de registro.
- **send-to-logger**: Integración con un sistema de registro externo.
- **enviar a notificación**: muestra mensajes como notificaciones del sistema.

Siguiendo el mismo patrón de diseño modular y funciones reutilizables, esta biblioteca puede convertirse en una herramienta integral para manejar todo tipo de tareas de mensajería.

## Beneficios de una biblioteca de mensajería

- **Reusabilidad**: las funciones se pueden reutilizar en diferentes complementos o proyectos.
- **Modularidad**: cada función maneja una tarea específica, lo que hace que el código sea más fácil de mantener y ampliar.
- **Consistencia**: el uso de las mismas funciones de validación y manejo de mensajes garantiza un comportamiento consistente en toda la aplicación.

La **Biblioteca de mensajes** es el comienzo de un marco más amplio que podría simplificar la forma en que se administran los mensajes en su proyecto. A medida que la biblioteca crece, nuevos complementos pueden acceder fácilmente a ella para enviar mensajes a donde sea que necesiten ir.

Podemos ajustar la estructura del archivo:

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

Y recuerda ajustar el `load` en el complemento principal:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
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