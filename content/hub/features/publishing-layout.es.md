---
title: "Diseño de publicación"
type: docs
url: "hub/features/publishing-layout"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: dc0367028ed8f6b4e1508c309384967daa43a4148f8d70f00880173a0a1fca7d
---

La ilustración para impresión y publicación a menudo necesita más que un tamaño de lienzo. Las páginas tienen bordes de corte, los pliegos tienen costura central y el contenido importante puede tener que mantenerse alejado de zonas que se recortarán o quedarán en el margen de encuadernación. Las herramientas de diseño de publicación de Lumi mantienen esas preocupaciones visibles mientras pinta, sin aplanarlas en la obra.

Los límites de diseño se almacenan por imagen, se guardan con el proyecto y pueden desactivarse cuando no hagan falta. El objetivo es dar a los flujos de trabajo de libros, cómics e impresión una noción clara de la estructura de página mientras la imagen en capas sigue siendo totalmente editable debajo.

## Sangrado y corte

El sangrado define hasta dónde se extiende la obra más allá del borde final de la página. Lumi muestra el área de corte como el límite activo de la página dentro del lienzo, con el margen de sangrado como superposición sombreada a su alrededor. Así resulta más fácil pintar fondos y detalles de borde que deben sobrevivir al recorte sin adivinar dónde terminará la página acabada.

Las medidas pueden configurarse en las unidades que convengan al trabajo, de modo que el sangrado pueda pensarse en pulgadas, milímetros u otra unidad de impresión habitual, no solo en píxeles.

## Margen interior y pliegos

En pliegos de doble página, el margen interior marca la zona protegida alrededor de la costura central donde conviene evitar contenido importante. Cuando está activado, Lumi muestra bandas de margen interior a lo largo del pliego para que rostros, texto y puntos focales queden fuera del área de encuadernación mientras el pliego completo sigue siendo un lienzo continuo.

Esto es especialmente útil para cómics, álbumes ilustrados y cualquier obra que se imprima como páginas enfrentadas y no como hojas sueltas.

## Guías de composición

Las guías de borde opcionales marcan el área de página recortada con marcas de composición discretas. Pueden seguir divisiones por página o la lectura del pliego completo, y usar tercios, secciones áureas o quintos según cómo deba evaluarse el diseño.

Las guías sirven como referencia silenciosa durante el diseño y el acabado. Ayudan a situar el contenido respecto a la página que realmente se imprimirá, no solo respecto al lienzo digital completo.

## Ver el diseño en el lienzo

Las superposiciones de diseño se controlan desde el menú Ver. Las zonas de sangrado, margen interior y guías pueden mostrarse por separado o juntas, para que el artista pueda centrarse en la parte de la estructura de publicación que importa en cada momento.

Imagen > Activar diseño enciende o apaga los límites de diseño de la imagen actual. Cuando el diseño está desactivado, las superposiciones se ocultan y los conmutadores de vista quedan a un lado, pero la configuración de límites sigue guardada en el archivo para más adelante.

## Guardado con el proyecto

La configuración de diseño viaja con el proyecto `.lum`. Al abrir la imagen más tarde se restauran sangrado, margen interior, apariencia de superposición, opciones de guía y si el diseño está activado para ese archivo. Así la configuración orientada a publicación forma parte del estado de trabajo de la obra y no de una preferencia de visualización temporal.

Para quienes alternan entre boceto, pintura y preparación de impresión, el flujo de trabajo permanece en un solo lugar: la misma imagen en capas, con la estructura de publicación disponible siempre que la página la necesite.
