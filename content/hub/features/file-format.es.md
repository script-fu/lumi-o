---
title: "Formato de archivo (.lum)"
type: docs
url: "hub/features/file-format"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: f26bd5ecb0cb647cd3180b1ab39402ee6943085b7ad899518a406ee6ae98c4c9
---

El formato de archivo nativo de Lumi está pensado para proyectos de pintura en capas que deben seguir siendo fiables, inspeccionables y recuperables con el tiempo. Se diseña en torno a la realidad del trabajo de ilustración: muchas capas, lienzos grandes, información de color incrustada, máscaras, efectos y datos de recuperación.

En lugar de tratar un proyecto como un único bloque opaco, el formato mantiene visible para la aplicación la estructura de la obra. Así Lumi puede guardar, cargar y recuperar imágenes grandes de forma más inteligente preservando la organización de la que dependen los artistas.

## Estructura abierta del proyecto

Un proyecto de Lumi mantiene separadas las partes de la obra: estructura de imagen, contenido de capas, máscaras, datos de color, metadatos e información de recuperación tienen cada uno un papel claro. Eso hace el formato más fácil de entender y más adecuado para el acceso a largo plazo que un contenedor monolítico cerrado.

El objetivo no es solo almacenar píxeles, sino almacenar el estado de trabajo de una ilustración. Las capas siguen siendo capas, las máscaras siguen siendo máscaras y el archivo sigue reflejando cómo se construyó la obra.

## Pensado para pinturas grandes

Las imágenes grandes en capas pueden volverse pesadas con rapidez. El formato de Lumi admite flujos de trabajo en los que no hace falta cargar en memoria todos los datos de imagen a la vez. Los proyectos pueden seguir siendo ágiles cargando solo las partes necesarias para ver, editar, componer o exportar.

Este enfoque ayuda a que los archivos complejos resulten manejables, sobre todo cuando una obra contiene muchas capas ocultas, archivadas, experimentales o agrupadas.

## Guardar sin interrumpir el flujo

El formato admite tanto el guardado normal del proyecto como instantáneas ligeras de recuperación. Así el artista puede proteger el trabajo con frecuencia sin convertir cada punto de control en un duplicado completo de toda la imagen.

Como la información de recuperación pertenece a la estructura del proyecto, Lumi puede mantener un historial útil cerca de la obra y, al mismo tiempo, permitir que los guardados de seguridad automáticos vivan separados del archivo de trabajo.

## Intercambio y exportación

El formato nativo está pensado para el trabajo continuo en Lumi, mientras que los formatos de exportación sirven para compartir resultados aplanados o centrados en compatibilidad. La importación ayuda a incorporar obras existentes al entorno en capas de Lumi, y la exportación permite que las piezas acabadas salgan del formato de proyecto cuando estén listas para publicación, entrega o procesamiento posterior.

Esa distinción mantiene el archivo de trabajo rico y editable y, al mismo tiempo, permite producir imágenes finales en formatos externos habituales.

## Fiabilidad a largo plazo

En resumen, el formato `.lum` es un contenedor práctico para trabajo de pintura serio: lo bastante abierto para inspeccionarlo, lo bastante estructurado para recuperarlo y lo bastante flexible para manejar imágenes complejas en capas de forma eficiente.
