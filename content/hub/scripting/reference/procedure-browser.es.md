---
title: "Navegador de procedimientos"
type: docs
---
El Explorador de Procedimientos es la principal herramienta de referencia para descubrir los cientos de funciones disponibles en la Base de Datos de Procedimientos (PDB) de Lumi. Debido a que cada herramienta, filtro y script en Lumi debe estar registrado en PDB para poder ser invocado, este navegador es efectivamente un explorador de PDB completo.

## Abrir el navegador de procedimientos

Vaya a **Ayuda → Programación → Explorador de procedimientos**.

También puede acceder a él desde Scheme Console a través de **Examinar**.

## Lo que muestra

El Navegador de Procedimientos puede enumerar todos los procedimientos actualmente registrados en el PDB, independientemente de su origen. De forma predeterminada, busca "interno" para mostrar los procedimientos principales registrados internamente.

- **Procedimientos internos**: Funciones principales para manipulación de imágenes, gestión de capas y control de herramientas.
- **Complementos externos**: procedimientos proporcionados por complementos compilados de C/C++ o extensiones persistentes.

## Búsqueda y filtrado

- **Cuadro de búsqueda**: Filtra procedimientos por nombre, descripción o autor. Al borrar el campo de búsqueda se muestran todos los procedimientos disponibles.
- **Tipo de búsqueda**: el menú desplegable de búsqueda le permite filtrar por campos específicos. Si lo configura en **por tipo** y busca "interno", la lista se reducirá para mostrar solo los procedimientos principales registrados internamente.
- **Vista detallada**: al hacer clic en un procedimiento se muestran sus parámetros, valores de retorno, autor, fecha y una descripción de lo que hace.

Esto es esencial para encontrar el nombre exacto y la firma del argumento de una función que desea llamar desde su secuencia de comandos.