---
title: "Envase"
type: docs
weight: 4
---
Los comandos de Scheme funcionan a un nivel bajo, lo que significa que incluso las tareas simples pueden requerir varios pasos. Sin embargo, esta granularidad ofrece flexibilidad: podemos agrupar comandos en funciones pequeñas y reutilizables que hacen exactamente lo que necesitamos. Envolver no es un concepto en blanco y negro; puede variar desde alias simples para comandos utilizados con frecuencia hasta funciones más complejas que administran flujos de trabajo completos. A veces, un contenedor es solo una función conveniente para mejorar la legibilidad, mientras que en otros casos evoluciona hasta convertirse en una utilidad con todas las funciones que encapsula múltiples operaciones.

### ¿Por qué envolver funciones?

Las funciones de envoltura ofrecen varios beneficios clave:

- **Simplifica las tareas repetitivas**: en lugar de repetir comandos de bajo nivel, envuélvalos en una función auxiliar y reutilícela.
- **Mejora la legibilidad**: Dar a nuestras funciones empaquetadas nombres claros y descriptivos hace que nuestro código sea más fácil de entender de un vistazo.
- **Encapsula la complejidad**: en lugar de lidiar con listas largas y crípticas de comandos, bucles profundamente anidados o declaraciones de mensajes complejos, podemos dividirlos en funciones auxiliares más pequeñas y bien estructuradas.
- **Mejora la capacidad de mantenimiento**: si la funcionalidad principal de un comando cambia, solo necesitamos actualizar nuestra función empaquetada una vez, aislando nuestros complementos de los detalles de esos cambios.
- **Fomenta la reutilización de código**: cada ayudante se convierte en parte de su biblioteca, lo que hace que los scripts futuros se escriban y depuren más rápido.

A medida que sus complementos crecen, los contenedores lo ayudan a mantener legible la lógica central y aislar los detalles repetitivos.

Otra ventaja de ajustar funciones es integrarlas en un resaltador de sintaxis como Visual Studio Code. Esto mejora la legibilidad y la navegación, haciendo que los scripts sean más claros. En un complemento que utiliza funciones personalizadas, cualquier función resaltada en verde confirma que se hace referencia a ella correctamente desde nuestra biblioteca.

Si mantiene su propia biblioteca auxiliar, considere agregar los nombres de las funciones de su proyecto al resaltado de sintaxis de su editor. Hace que la navegación y la refactorización sean más rápidas.

Ejemplos:

### Semilla aleatoria

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

Si bien podríamos usar ***msrg-rand*** directamente en nuestro código, incluirlo dentro de una función llamada ***random-seed*** mejora la legibilidad. Al darle a la función un nombre claro y descriptivo, resulta más fácil entender su propósito de un vistazo.

Además, definir ***random-seed*** como una función independiente nos permite usarla en cualquier lugar de nuestros complementos mientras centralizamos la implementación en una única ubicación. Si alguna vez necesitamos cambiar la forma en que se genera la semilla, solo necesitamos actualizar esta función, dejando el resto de nuestro código intacto.

Por ejemplo, si decidimos cambiar a ***aleatorio*** en su lugar:

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

El nombre de la función sigue siendo el mismo, lo que garantiza que nuestros scripts sigan funcionando sin modificaciones. Este enfoque mantiene nuestro código flexible, mantenible y fácil de leer.

### Exportación JPEG

La función de exportación JPEG en Scheme viene con muchos parámetros, lo que ofrece un control preciso sobre cómo se guardan las imágenes. Sin embargo, en la mayoría de los casos, sólo nos preocupamos por algunas configuraciones clave, como el nombre del archivo y la calidad. Para simplificar el proceso, podemos ajustar la función.

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

En esta función contenedora, la mayoría de las opciones de exportación están codificadas, exponiendo solo los parámetros que es probable que ajustemos: nombre del archivo y calidad. Este enfoque mejora la legibilidad y simplifica el almacenamiento de imágenes.Además, si el exportador de Lumi cambia en el futuro, solo necesitamos actualizar esta función en lugar de modificar cada script que exporta un JPEG.

### Usando el contenedor

Para exportar un JPEG en nuestros complementos, simplemente incluimos la biblioteca y llamamos a nuestra función personalizada:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

Esto mantiene nuestro código limpio, legible y adaptable al mismo tiempo que nos permite exportar archivos JPEG de manera eficiente con un mínimo esfuerzo.

### Reemplazo de automóvil

La función ***car*** puede ser críptica y propensa a errores de secuencia de comandos. Es fácil aplicar por error ***car*** a un vector o a un elemento que no está en la lista, lo que genera un comportamiento inesperado. Para hacer que nuestro código sea más robusto y legible, podemos incluir esta funcionalidad en una función más segura.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

Esta función recupera de forma segura el primer elemento de una lista o vector y, al mismo tiempo, proporciona advertencias útiles cuando se encuentran entradas no válidas o vacías. Al utilizar ***first-item*** en lugar de ***car***, reducimos el riesgo de errores accidentales y mejoramos la claridad de nuestros scripts.

#### ¿Por qué utilizar este contenedor?

- **Evita fallos del script**: evita errores causados ​​por la aplicación de ***car*** a personas que no son listas.
- **Admite listas y vectores**: amplía la usabilidad más allá de las listas.
- **Proporciona advertencias significativas**: ayuda a depurar problemas de entrada inesperados.
- **Mejora la legibilidad**: el nombre de la función transmite claramente su propósito.

Al encapsular esta lógica en el primer elemento, hacemos que nuestros complementos sean más sólidos y fáciles de mantener. Por supuesto, esto se reduce a preferencias personales, es posible que se sienta completamente cómodo usando car, caar, cadr y funciones similares de Scheme directamente.

### Envolviendo una función envuelta

Ajustar una función que ya está ajustada puede mejorar aún más la legibilidad y el mantenimiento. Por ejemplo, cuando trabajamos con pares de coordenadas como ***coordenadas de píxeles (lista 100 200)***, podríamos usar:

```scheme
(first-item pixel-coords)
```

para recuperar la coordenada ***x***. Sin embargo, aunque funcional, no es muy expresivo. En su lugar, podemos incluir ***primer elemento*** en una definición más apropiada para aclarar nuestra intención.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### ¿Por qué utilizar este enfoque?

- **Mejora la claridad del código**: en lugar de utilizar funciones genéricas de acceso a listas, definimos explícitamente funciones que describen su propósito.
- **Mejora la mantenibilidad**: si nuestra representación de coordenadas cambia (por ejemplo, usando vectores en lugar de listas), solo necesitamos actualizar estas pequeñas funciones.
- **Fomenta la coherencia**: el uso de ***x-coord*** y ***y-coord*** hace que el guión sea más fácil de leer y comprender de un vistazo.

Ahora, en lugar de escribir en un esquema genérico:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

Podemos escribir en _nuestro_ esquema:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

Al incluir funciones de bajo nivel en nombres significativos, creamos una forma más intuitiva de trabajar con datos, reduciendo la confusión y los posibles errores.

### Envoltorios enviados: la utilidad Stdlib

Lumi envía un conjunto de contenedores listos para usar que se cargan automáticamente al inicio, por lo que están disponibles en cualquier complemento o en Scheme Console sin ninguna llamada `(load ...)`. Estas bibliotecas (`common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm` y `paths.scm`) se basan exactamente en el mismo principio que los ejemplos anteriores: brindan información clara nombres a operaciones de bajo nivel, oculta texto repetitivo y proporciona un lugar único para actualizar si el comando subyacente cambia.Por ejemplo, `images.scm` proporciona `image-get-open-list` como un contenedor legible alrededor de la llamada PDB sin procesar, y `files.scm` expone ayudas de creación de rutas que de otro modo requerirían cadenas `string-append` repetidas.

Puede explorar cada nombre exportado, leer su cadena de documentación y ver de qué biblioteca proviene en **[Utility Browser](@@LUMI_TOKEN_21@@)** (Ayuda → Programación → Explorador de utilidades). Es una demostración práctica de cómo envolver a escala y una fuente útil de patrones que puede tomar prestados al crear su propia biblioteca auxiliar.

### Conclusión

Ajustar funciones es una forma poderosa de simplificar el desarrollo de Scheme, haciendo que los scripts sean más legibles, fáciles de mantener y robustos. Al encapsular la complejidad y exponer solo los detalles necesarios, creamos un enfoque más estructurado para escribir complementos.

Conclusiones clave de este enfoque:

- **Simplifica las tareas repetitivas**: en lugar de repetir manualmente comandos de bajo nivel, creamos funciones reutilizables.
- **Mejora la legibilidad del código**: los contenedores bien nombrados hacen que los scripts sean más fáciles de entender.
- **Encapsula la complejidad**: los detalles de bajo nivel se manejan dentro del contenedor, manteniendo limpio el script principal.
- **Mejora la capacidad de mantenimiento**: si la funcionalidad principal cambia, solo necesitamos actualizar el contenedor, no todos los scripts que dependen de él.
- **Fomenta la reutilización y la coherencia**: nuestra biblioteca personal de funciones crece con el tiempo, lo que hace que el desarrollo sea más rápido y eficiente.

Al utilizar consistentemente el ajuste de funciones, podemos transformar la forma en que escribimos complementos de Scheme, creando un entorno de scripting más modular y expresivo. Con estos principios en mente, podemos continuar perfeccionando nuestro enfoque, desarrollando una versión más eficiente y personalizada de Scheme que satisfaga nuestras necesidades específicas.

Próximos pasos: identifique bloques repetidos en sus scripts y extraiga pequeños ayudantes con nombres claros.