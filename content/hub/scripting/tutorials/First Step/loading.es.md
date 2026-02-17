---
title: "Cargando"
type: docs
weight: 3
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
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Ejemplo de una función de biblioteca send-message.scm

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### Cargar la función de biblioteca

Podemos cargar esa función de biblioteca con el comando Scheme `load`;

Cargando un archivo de biblioteca:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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