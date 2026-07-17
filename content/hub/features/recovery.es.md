---
title: "Recuperación de archivos"
type: docs
url: "hub/features/recovery"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 59495d24302cb3493b90bc61a6dd1ffb9bb9c30b179f7be388882fe4f45a5075
---

El sistema de recuperación de Lumi está diseñado para proteger el trabajo de pintura ante cierres inesperados, errores y sesiones interrumpidas. Ofrece a los proyectos una red de seguridad sin obligar al artista a duplicar archivos a mano de forma constante.

La recuperación se apoya en dos ideas: protección automática en segundo plano y puntos de control intencionales. Juntas ayudan a conservar el trabajo reciente y, al mismo tiempo, permiten volver a momentos anteriores del proyecto.

![recover](/images/screens/recover.jpg)

## Protección automática

Mientras se edita una imagen, Lumi puede mantener los datos de recuperación separados del archivo de trabajo principal. Así no hace falta reescribir el proyecto cada vez que se crea una instantánea de seguridad.

Si algo falla, el estado de recuperación automática puede ofrecer una versión reciente de la obra que quizá sea más nueva que el último guardado deliberado. El objetivo es sencillo: reducir el trabajo perdido cuando una sesión termina de forma inesperada.

## Puntos de control intencionales

Algunos momentos de una pintura merecen conservarse a propósito: antes de un cambio de color importante, tras un boceto acertado, antes de decisiones de aplanado o al probar una dirección arriesgada.

Lumi admite puntos de control a nivel de proyecto para esos momentos. Son más ligeros que guardar una copia completa separada por cada experimento, pero siguen dando al artista una forma de retroceder a puntos significativos del historial de la obra.

## Recuperación con contexto

Los estados de recuperación se presentan como versiones de la obra y no como archivos en bruto que haya que buscar a mano. El artista puede comparar guardados automáticos recientes y puntos de control deliberados, y abrir el estado que mejor coincida con el trabajo desde el que quiere continuar.

Las imágenes recuperadas se abren como documentos de trabajo, de modo que el artista puede revisarlas antes de decidir cómo guardarlas o seguir.

## Mantener la recuperación manejable

Un sistema de recuperación útil también debe seguir siendo manejable. Lumi está diseñado para mantener organizados los datos de recuperación y permitir eliminar estados antiguos cuando ya no hagan falta.

Así la seguridad no se convierte en desorden. La recuperación puede permanecer activa en segundo plano mientras el artista controla cuánto historial se conserva con el tiempo.

## Confianza al trabajar

El propósito de la recuperación de archivos no es sustituir el guardado, sino hacer el trabajo creativo menos frágil. El artista puede pintar, experimentar y arriesgar sabiendo que Lumi mantiene caminos adicionales de vuelta cuando una sesión, un archivo o una decisión sale mal.
