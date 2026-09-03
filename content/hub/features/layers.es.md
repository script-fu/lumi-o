---
title: "Capas"
type: docs
url: "hub/features/layers"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff151a84a2bca18cbd1389f1e7048fda7231ee8c1adf0bc16b1d7513c224f3ce
---

El sistema de capas de Lumi da estructura a la ilustración. Permite separar boceto, color, sombreado, textura, máscaras, ajustes, experimentos y detalle final sin fijar cada decisión directamente en una imagen plana.

Las capas no son solo una pila de píxeles. Llevan visibilidad, fusión, máscaras, bloqueos, efectos, agrupación y comportamiento de composición, lo que las convierte en la base de flujos de trabajo de pintura flexibles y no destructivos.

![layers](/images/screens/layers.jpg)

## Pintura estructurada

Una imagen en capas puede construirse por etapas. Las marcas ásperas pueden quedar bajo líneas limpias, el color puede bloquearse aparte de la iluminación, la textura puede aislarse y las ideas alternativas pueden seguir disponibles sin alterar la composición principal.

Los grupos hacen legible esa estructura. Las piezas relacionadas de una ilustración pueden moverse juntas, fusionarse o tratarse como una parte compartida de la obra mientras sus capas individuales siguen siendo editables.

## Fusión y máscaras

La fusión de capas controla cómo interactúa una parte de la obra con lo que hay debajo. Permite sombrear, aclarar, teñir, texturizar o corregir el color sin repintar las formas subyacentes.

Las máscaras añaden otro nivel de control. Permiten pintar, suavizar, ocultar, restaurar o dar forma a la visibilidad con independencia del contenido de color de la capa. Así las decisiones de borde y las transiciones tonales siguen siendo flexibles durante toda la vida de la pieza.

## Selección y navegación

Las pinturas complejas pueden contener muchas piezas pequeñas. Lumi admite navegación directa orientada a capas para que el artista pueda pasar del lienzo a la pila de capas sin perder el ritmo de la pintura.

La intención es que el trabajo en capas se sienta espacial y no administrativo: si una marca es visible en el lienzo, el sistema de capas debe ayudar a volver a ella con rapidez.

## Protección e intención

Las capas pueden protegerse de distintas maneras para que el trabajo terminado, las máscaras, las posiciones, la transparencia o las decisiones de color no cambien por accidente. Esas salvaguardas son útiles cuando la imagen se vuelve densa y algunas partes deben permanecer estables mientras otras siguen evolucionando.

Esta protección favorece flujos de trabajo deliberados: bocetar con libertad donde el cambio es bienvenido, fijar las zonas resueltas y seguir desarrollando la imagen sin miedo a daños accidentales.

## Color de pintura bloqueado

Una capa rellenada con un único color uniforme puede bloquearse a un color de paleta. Al seleccionar la capa se selecciona el color vinculado en la paleta, y al cambiar ese color de paleta la capa se actualiza de inmediato.

Pintar en una capa con color de pintura bloqueado usa siempre el color de paleta vinculado. Así se crea una conexión viva entre paleta y capa, y los colores pueden ajustarse de forma dinámica mientras se diseña la paleta.

## Efectos no destructivos

Los filtros y efectos pueden formar parte del estado editable de una capa en lugar de convertirse de inmediato en píxeles permanentes. Así los cambios visuales siguen siendo ajustables y la pila de efectos puede permanecer en la composición de trabajo.

Para el artista, eso significa que la experimentación puede seguir siendo reversible. Un acabado puede probarse, ocultarse, reordenarse, refinarse o confirmarse cuando pase a formar parte de la imagen final.

## Rendimiento en archivos complejos

Las ilustraciones en capas pueden volverse complejas, sobre todo cuando interactúan grupos, máscaras y efectos. El sistema de capas de Lumi está diseñado para mantener ágiles las acciones habituales de pintura evitando la recomposición innecesaria siempre que sea posible.

El resultado es un flujo de trabajo por capas orientado al control y a la velocidad: lo bastante detallado para un trabajo cuidadoso y no destructivo, pero aún práctico para la pintura diaria.
