---
title: "El navegador de procedimientos"
type: docs
weight: 1
---
El **Explorador de procedimientos Lumi** le permite buscar los procedimientos disponibles (integrados y complementos incluidos) e inspeccionar sus parámetros y valores de retorno.

### Dónde encontrar el navegador de procedimientos Lumi

Puede acceder al Navegador de procedimientos en Lumi a través del menú **Ayuda**:

- **Ayuda** -> **Navegador de procedimientos**

### Qué hace el navegador de procedimientos

El Explorador de procedimientos enumera todos los procedimientos internos de Lumi, junto con los agregados por complementos, incluido el que acaba de instalar. Cada entrada de procedimiento proporciona información útil, que incluye:

- El nombre del procedimiento.
- Una descripción de lo que hace.
- Los parámetros que acepta (valores de entrada).
- Los valores de retorno (salida).

Busque por palabra clave o nombre de procedimiento cuando necesite verificar la firma de una llamada o confirmar el nombre exacto del procedimiento.

#### (lumi-message) en el Navegador de procedimientos

Busque `lumi-message` para ver sus parámetros y valores de retorno.

### Encontrar su complemento

Una vez que haya instalado "¡Hola mundo!" complemento, puede encontrarlo en la lista del Explorador de procedimientos. Simplemente busque el nombre de la función que registró en Lumi, en este caso, "scheme-hello-world". La entrada mostrará los parámetros y los valores de retorno asociados con el complemento, junto con una breve descripción. También verá dónde se muestran algunas de las líneas de texto que ingresó como parámetros de entrada durante el proceso de registro en la sección **Información adicional**.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

Esto facilita verificar que su complemento esté registrado correctamente y le brinda una manera rápida de revisar cómo interactúa con otros procedimientos en Lumi. El Explorador de procedimientos es una poderosa herramienta para depurar y expandir sus complementos explorando todos los procedimientos disponibles dentro de Lumi.