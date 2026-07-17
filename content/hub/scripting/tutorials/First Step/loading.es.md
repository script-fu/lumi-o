---
title: "Cargando"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
url: "hub/scripting/tutorials/First Step/loading"
---
Tan pronto como una función auxiliar crezca, muévala a un pequeño archivo de biblioteca. Esto mantiene el complemento enfocado y hace que el asistente sea reutilizable en múltiples complementos.

### Crear una función de biblioteca

Podemos tomar la función de enviar mensaje y crear un nuevo archivo con eso como contenido. Guarde el archivo en la carpeta de su repositorio, no en la parte de complementos, tal vez cerca del nivel superior;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Este es su directorio principal para almacenar su código Scheme.
  - **biblioteca/**: Aquí es donde viven funciones compartidas como `send-message.scm`.
  - **plug-ins/**: aquí es donde se almacenan sus complementos individuales.
    - **hello-world/**: Carpeta para el plug-in específico «Hello World!».
      - **hello-world.scm**: Archivo de script del plug-in.

Ejemplo de una función de biblioteca send-message.scm

```scheme
;; Función para gestionar la salida de mensajes a varios destinos
(define (send-message message output)
  (cond
    ;; Enviar a la consola de mensajes
    ((eq? output 'error-console)
       ;; Establecer el controlador en consola de mensajes
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Enviar al cuadro de diálogo GUI
    ((eq? output 'gui)
       ;; Establecer el controlador en el diálogo GUI
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Enviar a la ventana de terminal
    ((eq? output 'terminal)
       ;; La salida del terminal se gestiona con display
       (display message)))

  ;; Restaurar el controlador de mensajes predeterminado a la consola de mensajes
  (lumi-message-set-handler 2))
```

### Cargar la función de biblioteca

Podemos cargar esa función de biblioteca con el comando Scheme `load`;

Cargando un archivo de biblioteca:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

¡Oye! Ahora tenemos algo más sencillo y breve de leer, que se describe a sí mismo sin comentarios. Ésta es la conclusión satisfactoria de la refactorización.