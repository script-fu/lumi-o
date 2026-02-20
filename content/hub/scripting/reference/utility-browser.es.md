---
title: "Navegador de utilidades"
type: docs
---
El Explorador de utilidades le permite explorar la utilidad Scheme stdlib incorporada que se incluye con Lumi, sin tener que salir de la aplicación ni buscar en los archivos fuente.

## Abrir el navegador de utilidades

Vaya a **Ayuda → Programación → Navegador de utilidades**.

La ventana se abre inmediatamente; no es necesario cargar ningún complemento por adelantado.

## Lo que muestra

El navegador enumera todos los procedimientos, variables y formas de sintaxis exportados por las siete bibliotecas de utilidades que Lumi carga automáticamente al inicio:

| Biblioteca | Qué cubre |
|---|---|
| `common.scm` | Ayudantes de uso general (cadenas, números, utilidades de lista) |
| `files.scm` | Ayudantes de archivos y rutas |
| `gegl.scm` | Buffer GEGL y ayudantes de color |
| `images.scm` | Ayudantes a nivel de imagen (`image-get-open-list`, etc.) |
| `layers.scm` | Ayudantes de capas y dibujables |
| `parasites.scm` | Ayudantes de lectura/escritura de parásitos |
| `paths.scm` | Ayudantes de ruta y vectores |

Todos estos están disponibles en cualquier complemento de Scheme o en la Consola de Scheme.

## Búsqueda y filtrado

- **Cuadro de búsqueda**: filtra por nombre a medida que escribe (coincidencia de subcadena que no distingue entre mayúsculas y minúsculas).
- **Filtro de tipo**: limita los resultados a `procedure`, `variable` o `syntax`.

Al hacer clic en una entrada se muestra su cadena de documentación completa y la biblioteca de la que proviene.

## El Stdlib como envoltorios

Las bibliotecas de utilidades son una aplicación práctica del patrón de ajuste: cada ayudante da un nombre claro a una operación de bajo nivel, oculta el texto repetitivo y proporciona un lugar único para actualizar si el comando subyacente cambia. Si desea comprender el enfoque de diseño detrás de ellos, consulte el tutorial **[Wrapping](@@LUMI_TOKEN_11@@)**.

## Relación con el Navegador de Procedimientos

El Explorador de utilidades está separado de **Filtros → Script-Fu → Consola → Explorar** (el Explorador de procedimientos). El Explorador de procedimientos enumera los procedimientos registrados en PDB. El Explorador de utilidades enumera definiciones auxiliares que se encuentran intencionalmente *fuera* del PDB: son solo de Scheme y no tienen enlace C.