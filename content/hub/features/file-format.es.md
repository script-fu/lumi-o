---
title: "Formato de archivo (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: c5e414a0d2870f1b111751c8c462e7ac5a4530103d70cb9be52a9fbd11417028
---

El formato nativo `.lum` de Lumi es un directorio de proyecto, no un único archivo cerrado. Está pensado para la ilustración en capas: árboles de capas profundos, lienzos grandes, máscaras, efectos no destructivos y puntos de control que no tienen que duplicar toda la pintura.

La función del formato es conservar intacta esa estructura de trabajo: reabrir un proyecto con fidelidad, inspeccionarlo cuando algo falla y recuperarlo desde un punto de control reciente, sin tratar la obra como un bloque opaco.

## Piezas separadas, a propósito

Un proyecto `.lum` es una carpeta. El árbol de capas y las propiedades de la imagen se guardan en XML legible. Cada capa y cada máscara conserva su propio búfer de píxeles, nombrado según la obra y no según un identificador interno. Los trazados vectoriales se guardan como SVG corriente. Los ajustes de filtros pesados ocupan sus propios archivos, junto a la imagen. Los perfiles ICC se almacenan una sola vez en la raíz del proyecto, para que las instantáneas de recuperación puedan referirse a ellos en lugar de copiarlos.

Esa separación es lo que hace posible el resto del formato. Las capas que no han cambiado pueden permanecer intactas en el disco. Un búfer dañado falla por su cuenta, en vez de llevarse el archivo entero. Los píxeles de capa que faltan se convierten en capas vacías que aún tienen nombre, posición y ajustes de fusión; si falta la vista compuesta de un grupo, se reconstruye a partir de los hijos. El proyecto sigue siendo un mapa de cómo se construyó la pintura.

Las paletas de pigmentos pertenecen a las herramientas de color de Lumi. Un proyecto puede recordar qué paleta estaba asociada a la imagen, pero la biblioteca de paletas en sí queda fuera del `.lum`.

## Estado editable, no un aplanamiento

El archivo guarda la pintura en curso. Las capas siguen siendo capas, los grupos siguen siendo grupos y las máscaras siguen siendo máscaras, incluidos desplazamientos, bloqueos, comportamiento de fusión y pilas de filtros. Los filtros no destructivos se guardan como operaciones y parámetros, no como píxeles ya aplicados. Una capa de un solo color plano no necesita archivo de píxeles.

Los grupos contraídos también conservan una vista compuesta de sí mismos. Esa vista guardada es lo que aparece en el lienzo cuando un grupo está cerrado, así que no hace falta reconstruir los hijos solo para mirar la imagen. Los modos de inspección solo para visualización quedan fuera de esa caché: mostrar una máscara o el alfa para editar se restaura como metadatos, no se graba en el grupo guardado.

## Los archivos grandes pueden quedarse en parte en el disco

Abrir un `.lum` no tiene que cargar todos los píxeles. El contenido de los grupos contraídos puede permanecer en el disco mientras se muestra de inmediato la vista compuesta guardada del grupo. Al expandir un grupo, esas capas, máscaras y grupos anidados pasan a memoria. Los grupos que permanecen cerrados siguen siendo ligeros.

El archivo también registra qué grupos estaban realmente en uso. Los grupos en la ruta de la selección activa pueden reabrirse expandidos; las demás carpetas se almacenan contraídas aunque estuvieran abiertas en la última sesión. Así un archivo profundo no carga en memoria cada rama sin usar en el momento de abrirlo.

Agrupar es, por tanto, una decisión de rendimiento además de una de organización. Fondos grandes, experimentos archivados y variantes sin usar pueden quedarse en grupos cerrados sin ocupar la misma memoria que las capas que se están pintando. El guardado sigue la misma regla: los búferes aún ocultos se copian o se omiten como archivos, no se vuelven a cargar en memoria solo para escribirlos de nuevo.

## Puntos de control que solo escriben lo que cambió

Archivo → Guardar actualiza el proyecto de trabajo. Los guardados incrementales y el autoguardado escriben en un árbol de recuperación, y solo escriben datos modificados: búferes de capa cambiados, no una segunda copia de toda la imagen. Cada punto de control lleva igualmente una descripción completa del árbol de capas, de modo que cualquier momento de ese historial puede abrirse rellenando los píxeles sin cambios desde puntos de control anteriores y, si hace falta, desde el propio archivo de trabajo.

El autoguardado usa el mismo esquema en una caché aparte, así que la protección automática no tiene que reescribir el archivo en el disco. Si se abre un proyecto cuando existen puntos de control más recientes que el último guardado completo, Lumi puede ofrecerlos en lugar de descartar en silencio el trabajo más reciente. Las imágenes recuperadas se abren con un nombre distinto para que un guardado rápido no pueda sobrescribir el original.

## Un formato de trabajo

`.lum` sirve para continuar una pintura en Lumi. Los formatos aplanados o de compatibilidad sirven para publicar, entregar y usar en otras aplicaciones. Como un proyecto es un directorio de muchos archivos, conviene archivarlo si tiene que viajar.

El archivo de trabajo sigue siendo rico y editable. Las exportaciones son el modo en que una imagen terminada o compartida sale de esa estructura.
