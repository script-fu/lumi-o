---
title: "Refactorización"
type: docs
weight: 2
---
Una vez que tenemos una función funcionando, podemos dar un paso atrás y pensar en la mejor manera de estructurar nuestro código. El objetivo es hacer que nuestro complemento sea lo más claro, comprensible y fácil de mantener posible. Este proceso de mejorar y refinar la estructura del código existente sin cambiar su comportamiento se conoce como refactorización.

Aquí está la función inicial nuevamente:

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

El nombre de la función es el nombre de la función y el parámetro es lo que la función acepta como entrada. El cuerpo es el bloque de código que se ejecuta cuando se llama a la función.

Forma abstracta:

```scheme
(define (function-name parameter)
  body)
```

### Repetición de código

Elimine la repetición temprano. `(lumi-message "Hello world!\n")` se repite dos veces y la cadena del mensaje se repite tres veces. Una variable resuelve la cadena repetida.

### Variables

En Scheme, una variable tiene un "alcance", del que se conoce, y ese alcance se establece mediante una declaración `let`. La variable está vinculada a un valor en la parte vinculante y la variable tiene alcance en el cuerpo let. La variable solo se conoce dentro del bloque let y no se puede acceder a ella fuera de él.

```scheme
(let ((variable value))
  body)
```

Introduciendo una variable llamada "mensaje":

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

En nuestro ejemplo hemos utilizado una variable llamada "mensaje" vinculada a una cadena "¡Hola mundo!\n". Esto nos permite cambiar el contenido del mensaje una vez en lugar de tres veces, lo que reduce la posibilidad de errores y hace que el código sea más flexible.

### Extrayendo funciones

En programación funcional, refactorizar código para extraer lógica reutilizable en funciones separadas es una práctica común. Al hacer esto, la **función principal** se vuelve mucho más simple y más enfocada en su objetivo de alto nivel, mientras que la **función extraída** parece más compleja porque maneja la lógica detallada. Esto es intencional y se alinea con los principios básicos de la programación funcional, como la modularidad, la separación de preocupaciones y la legibilidad. Aquí está el refactorizado.
¡Hola mundo! después de la extracción.

Extrayendo la lógica:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

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

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### Símbolos
En el ejemplo anterior, se utiliza un tipo de datos llamado símbolo, como 'gui. Los símbolos se pasan como parámetros a la función de envío de mensaje y se pueden utilizar para tomar decisiones condicionales simples. Al igual que las claves simbólicas, son identificadores únicos. Para obtener más información sobre los símbolos, visite [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)

### Simplificando la función principal

En la función original (scheme-hello-world), toda la lógica para enviar mensajes a diferentes salidas (GUI, Consola de errores, Terminal) se mezcló en la función principal. Después de la refactorización, la función principal simplemente se centra en **lo que hay que hacer**, enviando el mensaje a diferentes destinos.

La función principal refactorizada es más simple:

- Establece claramente su propósito: enviar el mismo mensaje a múltiples salidas.
- Evita saturar la lógica principal con código repetitivo, como configurar controladores de mensajes para diferentes salidas.
- Es más fácil de leer y entender de un vistazo.

### La complejidad de la función extraída

Por el contrario, la función **(enviar mensaje)** es donde reside la lógica detallada. Ahora maneja las variaciones de comportamiento para cada salida (GUI, Consola de errores, Terminal). La función es un poco más compleja que antes, pero ahora está **centralizada** y **aislada**.

## Relacionando esto con la programación funcional

En la programación funcional, las funciones se consideran **ciudadanos de primera clase**, lo que significa que pueden reutilizarse, transmitirse y combinarse para formar comportamientos más complejos. El objetivo es:- **Descomponer problemas** en partes más pequeñas e independientes.
- **Aislar la complejidad** en funciones más pequeñas que manejan tareas específicas, como `send-message`.
- **Mantenga simples las funciones de nivel superior** para que puedan concentrarse en orquestar el flujo de datos y acciones, sin necesidad de conocer los detalles de cómo se realiza cada tarea.
- **Separación de preocupaciones**: La función se encarga de cómo se envía el mensaje en función del tipo de salida, lo que aísla esta lógica de la función principal.
- **Modularidad**: al manejar toda la lógica de envío de mensajes en un solo lugar, podemos realizar cambios fácilmente (como agregar nuevas opciones de salida) sin alterar la función principal.
- **Reutilizabilidad**: la función `send-message` es reutilizable, lo que significa que si necesitamos enviar un mensaje a múltiples salidas en otras partes de nuestro código, podemos simplemente llamar a esta función en lugar de reescribir una lógica similar.

Al refactorizar, la función principal en este ejemplo se convierte en una declaración **declarativa** de lo que está sucediendo ("enviar un mensaje a tres lugares"), mientras que la complejidad de cómo enviar esos mensajes se abstrae en la función `send-message`.