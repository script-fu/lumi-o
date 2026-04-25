---
title: "Formato de archivo (.lum)"
type: docs
---
El formato de archivo nativo de Lumi está diseñado para proyectos de pintura en capas que deben seguir siendo confiables, inspeccionables y recuperables con el tiempo. Está diseñado en torno a las realidades del trabajo de ilustración: muchas capas, lienzos grandes, información de color incrustada, máscaras, efectos y datos de recuperación.

En lugar de tratar un proyecto como una única mancha opaca, el formato mantiene la estructura de la obra de arte visible para la aplicación. Esto permite a Lumi guardar, cargar y recuperar imágenes grandes de manera más inteligente y al mismo tiempo preservar la organización de la que dependen los artistas.

## Estructura abierta del proyecto

Un proyecto Lumi mantiene separadas las partes de la obra de arte: la estructura de la imagen, el contenido de las capas, las máscaras, los datos de color, los metadatos y la información de recuperación tienen cada uno una función clara. Esto hace que el formato sea más fácil de entender y más adecuado para el acceso a largo plazo que un contenedor monolítico cerrado.

El objetivo no es sólo almacenar píxeles, sino también almacenar el estado de funcionamiento de una ilustración. Las capas siguen siendo capas, las máscaras siguen siendo máscaras y el archivo sigue reflejando la forma en que se creó la obra de arte.

## Diseñado para pinturas grandes.

Las imágenes grandes en capas pueden volverse pesadas rápidamente. El formato de Lumi admite flujos de trabajo en los que no es necesario extraer todos los datos de la imagen en la memoria a la vez. Los proyectos pueden seguir respondiendo cargando las partes de la imagen que realmente se necesitan para ver, editar, componer o exportar.

Este enfoque ayuda a que los archivos complejos parezcan manejables, especialmente cuando una obra de arte contiene muchas capas ocultas, archivadas, experimentales o agrupadas.

## Ahorrar sin interrumpir el flujo

El formato de archivo admite tanto el guardado normal de proyectos como instantáneas ligeras de estilo de recuperación. Esto brinda a los artistas una forma de proteger el trabajo con frecuencia sin convertir cada punto de control en un duplicado completo de la imagen completa.

Debido a que la información de recuperación pertenece a la estructura del proyecto, Lumi puede mantener un historial útil cerca de la obra de arte y al mismo tiempo permitir que los guardados de seguridad automáticos vivan separados del archivo de trabajo.

## Intercambio y exportación

El formato nativo está pensado para el trabajo continuo de Lumi, mientras que los formatos de exportación se utilizan para compartir resultados aplanados o centrados en la compatibilidad. El soporte de importación ayuda a incorporar obras de arte existentes al entorno en capas de Lumi, y el soporte de exportación permite que las piezas terminadas abandonen el formato del proyecto cuando estén listas para su publicación, entrega o procesamiento posterior.

La distinción mantiene el archivo de trabajo rico y editable al mismo tiempo que permite que las imágenes finales se produzcan en formatos externos comunes.

## Fiabilidad a largo plazo

En resumen, el formato `.lum` es un contenedor práctico para trabajos de pintura serios: lo suficientemente abierto para inspeccionarlo, lo suficientemente estructurado para recuperarlo y lo suficientemente flexible para manejar imágenes complejas en capas de manera económica.