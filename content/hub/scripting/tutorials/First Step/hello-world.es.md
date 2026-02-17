---
title: "¡Hola Mundo!"
type: docs
weight: 1
---
Este tutorial recorre la estructura mínima de un complemento de Scheme. Algunas líneas son “repetitivas”: son necesarias para que Lumi cargue el archivo, incluso si aún no las entiendes del todo.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

En un nivel alto podrás:

1. Definir una función
2. Regístrelo para que aparezca en la Base de Datos de Procedimientos
3. (Opcional) Agregue una entrada de menú
4. Instale el archivo en una carpeta de complementos.

### Definir una función

Una función, también conocida como _procedimiento_, es un fragmento de código con un nombre y un propósito, toma una entrada y produce una salida.

**Entrada** > **_Función_** > **Salida**

### Registrar la función

Registrarse es el acto de poner el nombre de la función en una lista para que Lumi lo sepa.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### Enlace al menú

Esto le dice a Lumi dónde encontrar su función en su sistema de menú.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

Esto muestra el menú "Funky" en la barra de menú principal. Cambie la ruta para colocar el complemento en otro lugar. La ruta `<Image>/Funky` significa que el complemento aparecerá en la categoría de menú **Imagen**. Puede cambiar `<Image>` a `<Tools>`, `<Filters>`, etc., dependiendo de dónde desee que aparezca el complemento.

### Comentarios

En Scheme, el lenguaje base de Scheme, los comentarios generalmente se realizan precediendo una línea de texto útil con `;;`. El uso de los comentarios dependerá de su fluidez como codificador; si codifica ocasionalmente, más comentarios serán útiles. Si codifica todo el tiempo, el código es tan fácil de leer como lo sería el comentario. Además, cuando se programa funcionalmente, el código tiende a volverse lo suficientemente descriptivo como para leerlo como un script.

### Sintaxis

El código tiende a tener pequeñas reglas sobre cómo colocar elementos en una línea, para que podamos leer la línea fácilmente. Por ejemplo, una oración puede tener un espacio después de una coma o un punto. Ayuda a la legibilidad.

El código puede organizar las cosas de manera similar, lo que puede parecer extraño al principio:

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## Código de ejemplo

Aquí está el ejemplo completo. La mayoría de los procedimientos de Lumi tienen el prefijo `lumi-`. Por ejemplo, `lumi-message` imprime una cadena en el controlador de mensajes configurado.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


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

### Instalar el complemento

1. Vaya a **Lumi -> Editar -> Preferencias -> Carpetas -> Complementos**.
2. Agregue su carpeta de complementos [repo](/hub/scripting/tools/git) a la lista.
3. Cree una carpeta para el complemento y guarde el código de ejemplo anterior como `hello-world.scm`:
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. Haga clic derecho en el archivo `hello-world.scm`.
5. Vaya a **Propiedades -> Permisos -> Permitir ejecutar archivo como programa**.
6. Reinicie Lumi.

### Pruebe el complemento

El complemento debería aparecer ahora en el menú "Funky" en la ventana principal de Lumi. Haga clic en él y debería mostrar "¡Hola mundo!" mensaje. Intente modificar el código, como cambiar el texto del mensaje, y guarde el archivo. Cuando vuelvas a ejecutar el complemento, tus cambios se reflejarán sin reiniciar Lumi.

Intente experimentar cambiando la ruta del menú. Por ejemplo, `"<Image>/File"` lo colocará dentro del menú Archivo y `"<Image>/File/Funky"` creará una nueva sección en el menú Archivo. Esta es una excelente manera de personalizar dónde aparece su complemento y organizar sus herramientas.