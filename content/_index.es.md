---
title: "Pintura digital de código abierto para Linux"
toc: false
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 4f453bf4100140e9a2779d4b9806a0ef8f27e68af8a9b6f680d01a6a5d2f2341
---
[![gui-01](/images/screens/gui-00.jpg)](/images/screens/gui-01.jpg)

Lumi-o está diseñado para la pintura ráster por capas: un software deliberadamente ligero y estable. La compatibilidad con tabletas Wacom proporciona la fluidez y rapidez de respuesta que exige la ilustración profesional.

El sistema de color de Lumi, basado en paletas, mezcla espectralmente perfiles de pigmentos reales. Cree paletas a partir de relaciones cromáticas visualizadas, controle los valores tonales y organice los colores como entradas con nombre. Palette Map y Mixer le ayudan a mantener coherentes las decisiones de color en una ilustración o un proyecto.

Basado en la arquitectura central de GIMP, Lumi-o está diseñado y perfeccionado por un artista. Sus herramientas siguen siendo accesibles y, al mismo tiempo, ofrecen la personalización necesaria para flujos de trabajo avanzados. La carga y el guardado rápidos, junto con un sistema de guardado automático totalmente integrado, protegen su trabajo sin interrumpir su flujo creativo.

Los trazos de pincel pueden tomar una textura de superficie, con el grano y el diente de un soporte, también los trazos caligráficos. Un trazo nuevo, o un giro dentro del trazo, puede desplazar ese grano, de modo que el sombreado de ida y vuelta se construye sobre una fase nueva en cada inversión. Los pinceles de cerdas se pueden perfilar con detalle, y sus cerdas pueden desplazarse y renovarse a lo largo del trazo. OpenGL dibuja la pila de capas en la ventana de imagen y puede pintar trazos para preajustes de herramienta concretos. Los aerógrafos grandes y los sellos de textura grandes son mucho más rápidos, y la navegación por el lienzo se mantiene fluida.

La carga diferida permite a Lumi abrir proyectos grandes y complejos utilizando mucha menos RAM. Cuando un grupo está contraído, Lumi carga la imagen compuesta guardada, mientras que sus capas, máscaras y grupos anidados permanecen como marcadores de posición ligeros hasta que el grupo se expande de nuevo.

Lumi-o se desarrolla en Debian 13 con Cinnamon sobre X11. GNOME en Wayland se ha probado en cuanto al aspecto general de la aplicación y al renderizado OpenGL. Si viene de Windows, instalar Debian es el primer paso. Consulte [Cómo instalar Linux](/hub/install-linux/).

{{< cards >}}
  {{< card link="hub/about/" title="Acerca de" icon="information-circle" >}}
  {{< card link="hub/quick-start/" title="Inicio rápido" icon="play" >}}
  {{< card link="hub/features/" title="Funciones" icon="book-open" >}}
{{< /cards >}}
