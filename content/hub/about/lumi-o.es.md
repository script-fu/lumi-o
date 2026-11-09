---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
---
Lumi es una aplicación rápida y eficiente, exclusiva de Linux, para crear imágenes raster, desarrollada en abierto. Sus decisiones de diseño, documentación de arquitectura e historial de desarrollo son públicos, para que los usuarios puedan entender cómo evoluciona.

Lumi está orientada a la pintura e ilustración digital, sin dejar de ser capaz de edición estructurada de imágenes y corrección fotográfica. Ajustes correctivos, efectos de estilo de objetivo, gradación de color por rangos tonales y capas de filtro no destructivas admiten una amplia gama de flujos de edición.

Lumi parte de la idea de que un software de pintura digital debe comportarse como una herramienta de estudio fiable: predecible, transparente y centrada en crear imágenes.

## Propósito

Lumi admite un método estructurado y no destructivo de creación de imágenes, ya sean pintadas, dibujadas o fotográficas. Es una alternativa enfocada y decidida tanto a editores de imágenes generales como a software de pintura dedicado: libre y de código abierto, sin suscripciones, bloqueo, dependencia de la nube ni generación de imágenes con IA.

La fiabilidad y el acceso a largo plazo son características centrales. El formato de archivo abierto basado en directorios sigue siendo legible sin software propietario, con importación y exportación XCF y PSD.

## Base artística

Lumi la desarrolla un artista independiente con experiencia en pixel art, dibujo y pintura tradicionales, desarrollo de juegos, arte técnico, ilustración y animación 3D. Ese recorrido moldea su enfoque del color, el trazo, las capas, el rendimiento, la recuperación de datos, los scripts y la experiencia de usuario.

## Filosofía

Lumi combina un sistema de color basado en pigmentos con un flujo de trabajo reactivo, no destructivo y basado en capas. Su sistema cromático se aparta deliberadamente de los deslizadores HSV convencionales y de selectores RGB arbitrarios.

- **Color centrado en pigmentos**: los perfiles de pigmentos del mundo real (códigos Colour Index) se mezclan espectralmente, de modo que las paletas se comportan más como pintura real.
- **Flujo de trabajo guiado por paletas**: paletas guardadas e intercambiables organizan pigmentos, mezclas, bandas de valor y degradados, manteniendo coherentes las decisiones de color en una ilustración o un proyecto.
- **Herramientas táctiles y enfocadas**: los pinceles integran presión, inclinación y velocidad del lápiz para un control directo y matizado; los controles respaldan decisiones deliberadas sin complejidad innecesaria. La textura de superficie puede dar a un trazo el grano y el diente de un soporte, también en los trazos caligráficos, y un giro puede desplazar ese grano. Los pinceles de cerdas se pueden perfilar de cerca y animar a lo largo del trazo.
- **Lienzo ágil**: OpenGL puede dibujar la pila de capas y pintar trazos de pincel para preajustes de herramienta concretos. Los aerógrafos grandes y los sellos de textura grandes siguen a la mano, y moverse por el lienzo se mantiene fluido.
- **Fiabilidad no destructiva**: capas y filtros editables escalan a proyectos complejos sin dejar de ser predecibles. El guardado automático, el guardado rápido, los guardados incrementales y la recuperación protegen largas sesiones de pintura y proyectos grandes.
- **Espacios de trabajo dinámicos**: perfiles con nombre conservan docks, herramientas, presets, paletas y enlaces de dispositivos, y los cambian de forma atómica en tiempo de ejecución.
- **Scripts Scheme**: Lumi amplía la tradición Script-Fu con un lenguaje de complementos basado en Scheme y funciones de utilidad adicionales para crear complementos y automatizar flujos de trabajo.

Los [filtros](/hub/features/filters/), incluidos desenfoque bokeh con forma de objetivo, tilt shift, gradación tonal, nitidez y reducción de ruido, pueden seguir siendo editables junto al trabajo con pincel.

## Límites

- **Enfocado, no exhaustivo**: Lumi no apunta al diseño web, la autoedición ni a todos los nichos que un editor amplio como GIMP intenta cubrir.
- **Solo Linux**: Lumi se desarrolla en Debian con Cinnamon (X11). GNOME en Wayland se ha probado en cuanto al aspecto general de la aplicación y al renderizado OpenGL. Windows y macOS no se admiten.

## Agradecimientos

Lumi se apoya en el GNU Image Manipulation Program (GIMP). Lumi reconoce y agradece profundamente los muchos años de trabajo de desarrolladores, artistas y colaboradores.

![Lumi logo placeholder](/images/lumi.png)
