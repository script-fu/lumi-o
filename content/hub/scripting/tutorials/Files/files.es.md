---
title: "Archivos"
type: docs
weight: 7
---
Trabajar con archivos y directorios es esencial para el desarrollo de Scheme. Ya sea que esté guardando resultados, cargando recursos u organizando la estructura de su proyecto, comprender las operaciones de archivos hará que sus scripts sean más sólidos y fáciles de usar.

Esta página cubre tareas comunes de archivos y directorios: leer rutas, crear directorios y recopilar entradas de carpetas a través de parámetros GUI.

## Directorio de inicio del usuario

Lumi es solo para Linux, por lo que el directorio de inicio del usuario proviene de la variable de entorno `HOME`.

Para obtener el directorio de inicio del usuario como una cadena:

```scheme
(getenv "HOME")
```

Salida de ejemplo:

```scheme
"/home/username"
```

## SEPARADOR DIR

También existe la variable global `DIR-SEPARATOR`, que es el separador de ruta específico de la plataforma. En Lumi (Linux), siempre es `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Obtener una ubicación de directorio

Podemos pedirle al usuario una ubicación de directorio en el cuadro de diálogo Esquema para un complemento.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME` proporciona un navegador a un directorio.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Aquí validamos las dos entradas del directorio (origen y destino) y volvemos a los valores predeterminados si las rutas de la GUI están vacías o no son válidas.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Si está interesado en los detalles de implementación, busque en la fuente del complemento `validate-path-and-dir`.

## Crear un directorio

Scheme proporciona el comando ```dir-make``` para crear un directorio. Este comando toma una ruta separada por "/" y crea un directorio único con un parámetro opcional para los privilegios. No le damos rutas específicas de plataforma.

Por lo general, necesitamos crear varios directorios para una ruta práctica. Podemos usar un contenedor para ```dir-make``` para ayudarnos aquí.

```scheme
;; Purpose: A wrapper for (dir-make) that creates a given path from a platform
;;          supplied path. Always emits Linux style separators for dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Root dir
    ;; Create the rest of the directories step-by-step
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; build the path
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Nota: Esta función también utiliza el ```file-exists?``` integrado para omitir llamadas innecesarias. Devuelve #t si el archivo o directorio indicado existe, y #f si no existe o si no es accesible para el usuario solicitante.

## Construyendo un camino

También necesitamos romper y reconstruir caminos en Scheme.

Para dividir una ruta en partes, use ```strbreakup```:

### Ejemplos de rutas de Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Nota: Las barras diagonales iniciales y finales se convierten en elementos de cadena vacíos en la lista resultante.

Para reconstruir una ruta, use ```string-append```:

### Construcción de rutas de Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```