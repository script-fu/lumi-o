---
title: "El complemento de filtro"
type: docs
weight: 2
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e8eb69ed9dff7c65cc926ba4bfb4c333fdd8baa3832aa92765ba6bb19b17516d
---
Usamos un complemento _procedure_ para el tutorial [Primer paso](../../first-step/). Esos tipos de complementos funcionan sin necesidad de una imagen o un elemento de diseño como entrada. Por lo general, usamos un complemento para cambiar una imagen y sus elementos de diseño. Los complementos como estos se denominan complementos de filtro.

### ¿Qué es un dibujable?

Un **dibujable** en Lumi se refiere a un elemento de imagen en el que se puede dibujar, como una capa o un canal. Los complementos de filtro suelen funcionar con estos elementos.

### Un ejemplo simple de complemento de filtro

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Usar una instrucción let para definir una variable de mensaje y el código principal
  (let ((message "hello, world"))
    ;; Mostrar el mensaje en la consola de errores de Lumi
    (lumi-message message)
    ;; Invertir los colores del primer drawable seleccionado
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Registra el complemento
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Nombre del procedimiento principal
  "Simple Filter Plug-in Demo"             ;; El nombre tal como aparece en el menú de Lumi
  "Tests a basic Scheme filter plug-in"    ;; Descripción de la información sobre herramientas
  "Author Name"                            ;; Date algo de crédito
  "License"                                ;; Licencia
  "Date written"                           ;; Fecha de escritura
  "*"                                      ;; Indica que este complemento requiere una imagen
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requiere uno o más drawables seleccionados

;; Especificar la ubicación del menú para el complemento
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

Copie el texto y guárdelo como `simple-filter-plug-in.scm` en una carpeta llamada `simple-filter-plug-in` dentro de una de las carpetas de complementos de Lumi. Una carpeta de complementos de Lumi es _cualquier_ carpeta enumerada en:
 **Lumi > Editar > Preferencias > Carpetas > Complementos**

En Linux, haga clic derecho en el archivo `simple-filter-plug-in.scm`, vaya a **Propiedades > Permisos** y marque **Permitir ejecutar el archivo como programa**. Una vez que el archivo esté en el lugar correcto, ejecutable y libre de errores de sintaxis, cuando se reinicie Lumi, aparecerá en la barra de encabezado del menú superior, dentro de un menú llamado **Plug-in**.

### Ejecutando el complemento

1. Abra una imagen (este complemento de filtro requiere una imagen para funcionar).
2. Abra **Herramientas > Depurar > Consola de mensajes** para ver un mensaje.
3. Seleccione **Demostración del complemento de filtro simple** en el menú **Complemento**.
4. Una de las capas seleccionadas tendrá sus colores invertidos y se imprimirá un mensaje en la consola de error.

### Edición del complemento

Puede personalizar el complemento editando su archivo `.scm`. Por ejemplo, para cambiar el mensaje mostrado:

1. Abra el archivo y localice la línea que define `message`.
2. Reemplace `"hello, world"` con su texto personalizado.
3. Guarde el archivo.

En Lumi versión 3, no es necesario actualizar los complementos para que los cambios guardados surtan efecto. Simplemente vuelva a ejecutar el complemento para ver el mensaje actualizado.

### Examen de complemento

#### Línea Shebang

La primera línea garantiza que el script funcione como un complemento en Lumi 3:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

```

#### Definición del procedimiento

El procedimiento acepta dos argumentos: la imagen activa y los dibujables seleccionados.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### Lógica central

Una declaración `let` define una variable y realiza operaciones en el elemento dibujable.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Muestra un mensaje en la consola de errores de Lumi
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Invierte los colores del primer drawable seleccionado
```

### Registro de complemento

El complemento está registrado en Lumi como complemento de filtro:

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Registra el procedimiento principal
  "Simple Filter Plug-in Demo"             ;; El nombre tal como aparece en el menú de Lumi
  "Tests a basic Scheme filter plug-in"    ;; Descripción de la información sobre herramientas
  "Author Name"                            ;; Nombre del autor
  "License"                                ;; Tipo de licencia
  "Date written"                           ;; Fecha de escritura
  "*"                                      ;; Indica que el complemento requiere una imagen
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requiere uno o más drawables seleccionados
```

#### Registro de menú

Esta línea especifica la ubicación del menú para el complemento:

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### Solución de problemas

Si no aparece un complemento, verifique su ubicación, nombre y propiedad ejecutable.

La ubicación debe estar en una ruta de búsqueda de complemento.
El nombre del archivo debe coincidir con el nombre de la carpeta que lo contiene.
El archivo debe configurarse como ejecutable.


La **consola de mensajes** es una herramienta valiosa para solucionar problemas de complementos personalizados. Si su complemento no se comporta como se esperaba, consulte aquí para ver mensajes de error o registros. La ventana **Terminal** también puede proporcionar información de depuración e informar problemas de carga.