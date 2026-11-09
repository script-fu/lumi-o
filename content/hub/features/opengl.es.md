---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
translation_provenance: ai-reviewed
translation_source_sha256: af2a57a988eced14801b7c4673253fe79e9a6b6ab4fcf53a075b62d4a9035351
translation_lock: true
---

Lumi puede dibujar la pila de capas con OpenGL y pintar trazos de pincel del mismo modo. La navegación por el lienzo se mantiene fluida. Los aerógrafos grandes y los sellos de textura grandes son mucho más rápidos. Si una herramienta concreta pinta así, esa elección queda guardada en su preajuste de herramienta.

## La pila de capas

La ventana de imagen muestra la pila de capas compuesta. Con OpenGL, esa imagen se mantiene en teselas en el procesador gráfico, ya preparada para la pantalla. El desplazamiento, el zoom, la rotación y el volteo cambian la vista de esas teselas. En un lienzo grande los gestos se mantienen fluidos, porque la navegación es un cambio de vista sobre una imagen que ya está ahí.

Las ediciones actualizan las teselas que tocan. Las guías, el cursor del pincel y el contorno de la selección se dibujan sobre esa imagen.

Mientras el lápiz está apoyado, un trazo que usa OpenGL puede mostrarse en su sitio entre las capas que lo rodean, incluidas las capas por encima de la que se está pintando. La marca se escribe en la capa cuando el lápiz se levanta.

## Trazos por preajuste de herramienta

Pintar con OpenGL es una elección del preajuste de herramienta, junto al pincel y a la dinámica. Un preajuste puede pintar en el procesador gráfico. Otro puede conservar el recorrido de trazo ordinario. Volver a una herramienta trae esa elección consigo.

La ganancia es mayor donde los sellos son grandes. En el recorrido ordinario, un aerógrafo ancho se queda atrás respecto al lápiz a medida que crece la marca: los toques hacen cola y el trazo abre huecos, también mientras el lápiz sigue apoyado y el pincel continúa emitiendo. Con OpenGL ese aerógrafo sigue dejando las marcas al ritmo de la mano.

Un sello de textura grande muestra el mismo cambio. El recorrido ordinario dedica más tiempo a cada toque a medida que el sello crece, hasta que un trazo se atasca. El trazo OpenGL sigue el gesto. Los sellos que llevan un grano de superficie también se pueden dibujar con OpenGL.

Las marcas de cerdas se pueden dibujar así. Un sello de cerdas de punta redonda se puede pintar con OpenGL mientras las cerdas cambian a lo largo del trazo. Un trazo caligráfico se puede dibujar como una sola cinta continua, y la textura de superficie puede enmascarar esa cinta. La [textura de superficie](/hub/features/paintbrush/#textura-de-superficie) y las [cerdas a lo largo del trazo](/hub/features/paintbrush/#cerdas-a-lo-largo-del-trazo) pertenecen al pincel mismo.

Algunos trazos permanecen en el recorrido ordinario. La mezcla espectral, un filtro en la capa pintada y algunos modos de fusión están entre ellos. Esas marcas se pintan de todos modos.

## Cuando permanece el lienzo ordinario

OpenGL se usa cuando el hardware gráfico puede ejecutarlo y la imagen cabe en la memoria reservada para el lienzo. Si no, Lumi dibuja la ventana como antes. El AppImage publicado lleva sus propias bibliotecas gráficas. Con un controlador que esas bibliotecas no pueden usar, incluido el controlador propietario de NVIDIA, el lienzo permanece en ese recorrido ordinario.

## Cinnamon en X11 y GNOME en Wayland

Lumi se desarrolla en Debian con Cinnamon y X11. Ese sigue siendo el entorno de desarrollo. GNOME en Wayland se ha probado en cuanto al aspecto general de la aplicación y a este renderizado OpenGL. La navegación por el lienzo también es fluida allí.
