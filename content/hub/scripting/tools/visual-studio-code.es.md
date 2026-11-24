---
title: "Cursor"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: d3a7764cd3f00336fdcbb4d09a47fc3cabf4c99767cd005ccdc2fafdfa119117
url: "hub/scripting/tools/visual-studio-code"
---

## ¿Qué es Cursor?

[Cursor](https://cursor.com/) es un editor de código con ayuda de IA en la misma ventana. Se basa en Visual Studio Code, así que el resaltado de sintaxis, las extensiones, los espacios de trabajo y Git se conservan. Cursor es el editor que se usa aquí para los scripts de Lumi, porque esa ayuda de IA resulta mucho más económica.

- **Resaltado de sintaxis**: El resaltado colorea el código. Los errores se ven antes, la estructura se entiende mejor y el código complejo se lee con más claridad.
- **Extensiones**: Cursor ejecuta las extensiones de Visual Studio Code. Idioma, tema o herramienta: hay una extensión que encaja.
- **Integración del control de versiones**: Git viene integrado. Resulta fácil seguir los cambios y gestionar ramas, con servicios como [GitHub](https://github.com/) o [GitLab](https://gitlab.com/).
- **IA en el editor**: Puedes pedir una búsqueda, una refactorización o un cambio pequeño junto al archivo que ya tienes abierto. El coste de esa ayuda es lo bastante bajo para usarla en el trabajo ordinario con scripts.

### Funciones clave para principiantes

- **Espacios de trabajo**: Cursor organiza varios proyectos en «espacios de trabajo», que mantienen juntos los archivos y carpetas relacionados. La navegación y la gestión del proyecto se vuelven más sencillas.

- **Perfiles**: Con los perfiles puedes preparar configuraciones distintas para cada proyecto o tarea. Uno para desarrollo web y otro para Scheme, cada uno con sus extensiones, temas y ajustes.

### Primeros pasos con Cursor

Para empezar a usar Cursor:

- **Descargar e instalar**: Entra en el [sitio de Cursor](https://cursor.com/) y descarga la versión para Linux.

- **Explorar la interfaz**: Al abrir Cursor verás varios paneles. A la izquierda está la **barra de actividades**, desde la que pasas entre archivos, búsqueda, Git y extensiones.

- **Abrir un archivo o una carpeta**: Prepara el espacio de trabajo añadiendo la carpeta del proyecto y guarda la configuración activa en un archivo de espacio de trabajo.

Un espacio de trabajo es solo una vista de carpeta guardada, más los ajustes del editor para el proyecto.

### Extensiones

- **Instalar extensiones**: Pulsa el icono **Extensiones** de la barra de actividades para buscar e instalar herramientas útiles para Scheme. La principal es **«vscode-scheme»**, de Allen Huang. Ofrece resaltado de sintaxis y un soporte básico de Scheme, el lenguaje en el que se escriben los complementos de Lumi.

También puedes adaptar la extensión editando el archivo de lenguaje, para incluir más palabras clave de Scheme o cualquier término del scripting de Lumi.

Si mantienes tu propia biblioteca de ayuda, añadir los nombres de las funciones al resaltado hace más rápidas la navegación y la refactorización.

### Conclusión

Usa un editor que facilite buscar, refactorizar y hacer cambios rápidos, con la IA al alcance. Cursor más una extensión de Scheme es la base que se usa aquí.
