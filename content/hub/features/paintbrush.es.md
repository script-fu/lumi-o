---
title: "Herramienta Pincel"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: a37df7a3325c5a6028907f9584d45fd23746dd345b2d649f0a3ff5c1e03ed657
---

La herramienta Pincel es el instrumento de pintura principal de Lumi: una forma sensible y expresiva de dibujar, pintar, sombrear, texturizar y crear marcas directamente en el lienzo. Está diseñada para sentirse inmediata y, al mismo tiempo, dar al artista margen para definir cómo se comporta un trazo.

Más que un único pincel fijo, actúa como un sistema de pintura. La forma, la textura, el movimiento, la presión, el tiempo y el color del pincel pueden contribuir a la marca final, lo que la hace adecuada para líneas limpias, pintura suave, efectos de medios secos, trazos caligráficos, texturas dispersas y formaciones de pincel con varios cabezales.

![brush-tool](/images/screens/brush-tool.jpg)

## Marcas de pincel expresivas

Los pinceles pueden basarse en sellos de mapa de bits, formas procedimentales o fuentes animadas por fotogramas. Un trazo puede ir desde una marca redonda y suave hasta un cabezal rico en textura o en evolución. El mismo motor de pintura admite dibujo preciso, acumulación pictórica, marcas decorativas y ruptura al estilo de medios naturales.

Cuando un pincel se vuelve visualmente complejo, la vista previa puede seguir simplificada para que la pintura siga siendo ágil y fácil de leer.

![tool-setup](/images/screens/tool-setup.jpg)


## Dinámica y respuesta de entrada

La herramienta Pincel responde a la entrada en directo: presión del lápiz, velocidad, dirección, inclinación y otros valores del controlador. Esas señales pueden influir en el trazo visible de muchas maneras: grosor, opacidad, ángulo, respuesta de textura, comportamiento del color, espaciado y otras cualidades pueden cambiar a medida que se mueve la mano.

Así el pincel se siente menos como un patrón estampado y más como un instrumento de dibujo físico. Un toque ligero puede producir marcas delicadas; un movimiento más rápido puede abrir textura o forma; y el comportamiento sensible a la dirección puede ayudar a que los trazos sigan el gesto de la mano.

![dynamics](/images/screens/dynamics.jpg)

## Comportamiento del trazo

Los trazos pueden ser directos e inmediatos, o asistidos por suavizado y estabilización. Estas funciones ayudan a reducir el temblor no deseado, suavizar cambios bruscos y hacer que los movimientos largos se sientan más controlados sin eliminar el carácter de la entrada del artista.

El Pincel también admite distintos enfoques de acumulación de pintura. Puede comportarse como un trazo continuo, acumular pinceladas repetidas o emitir marcas con el tiempo mientras el puntero permanece en su sitio. Esa flexibilidad lo hace útil tanto para el trabajo de línea deliberado como para la construcción tonal más lenta.

Para marcas caligráficas o al estilo de la tinta, el Pincel puede generar un trazo de forma más continua en lugar de depender solo de sellos repetidos. El resultado son formas fluidas, parecidas a cintas, que responden con naturalidad al gesto y la velocidad.

![stroke](/images/screens/stroke.jpg)

## Captura de trazo y renderizado simulado

El Pincel puede capturar una pequeña muestra de cómo se dibuja normalmente a mano un preajuste y usar ese perfil al renderizar trazos definidos por geometría en lugar de movimiento en directo. Las líneas rectas con Mayús y clic, los trazados con trazo y las selecciones con trazo pueden usar el patrón de presión y velocidad capturado del preajuste de herramienta activo en lugar de comportarse como una línea mecánica plana.

Así los trazos construidos se acercan más al carácter del pincel. Una línea dibujada a partir de un trazado puede empezar suave, ganar presión, atenuarse o variar la respuesta a la velocidad de la misma manera amplia que el trazo de mano muestreado, siguiendo la forma exacta del trazado, el borde de selección o el gesto de línea recta.

## Posprocesamiento

El pincel puede registrar un trazo mientras lo dibuja y reproducir ese gesto capturado al levantar, refinando el trazado antes de que se fije la marca final. Puede bocetar con libertad y aun así llegar a una dirección más clara, esquinas más definidas o una estructura más deliberada sin dibujar con precisión mecánica.

Esto abre el rayado y las marcas de construcción regladas que encajan en ángulos limpios conservando la longitud y el carácter dibujados a mano, trazos de cinta estables en inclinación y reproducción consciente de esquinas que trata curvas y tramos rectos de forma distinta. Los pinceles con varios cabezales pueden compartir un trazado corregido mientras cada cabezal conserva su variación, y la dinámica puede seguir modelando el trazo a lo largo de su curva final durante la reproducción. El posprocesamiento se aplica a trazos dibujados, no a la emisión continua de aerógrafo.

## Color y textura

Las pinceladas pueden usar el color de pintura activo, responder a degradados o variar el color mediante dinámicas. El manejo de textura permite alternar entre cobertura sólida y marcas rotas que rozan la superficie, útil para pincel seco, grano y sombreado expresivo.

Como el color y la textura pueden formar parte del mismo sistema dinámico que la forma y la opacidad, un solo trazo puede evolucionar al moverse por el lienzo en lugar de permanecer visualmente uniforme.

## Cabezales y formaciones de pincel

La herramienta Pincel puede pintar con más de un cabezal a la vez. Varios cabezales pueden disponerse alrededor del recorrido del trazo para crear marcas de punta, trazos en abanico, comportamiento tipo cerda, patrones de pulverización, formaciones texturizadas o rayado estructurado.

Esos cabezales pueden seguir la dirección del recorrido, variar entre sí y dispersarse de modo que el trazo se sienta orgánico y no mecánicamente repetido. Es especialmente útil para pinceles de medios naturales, trazos decorativos, follaje, pelo, rayado y otras marcas que se benefician de una irregularidad controlada.

![brush-heads](/images/screens/brush-heads.jpg)

## Carga del pincel y recogida de pintura

El Pincel también puede simular cuánta pintura o material lleva el pincel en cada momento. A medida que continúa el trazo, esa carga puede ir disminuyendo, de modo que las marcas se vuelvan más claras, secas, finas, ásperas o más rotas según la dinámica del pincel.

La carga puede reintroducirse entre pasadas, mantenerse en un nivel elegido o usarse como señal de control en directo para otros comportamientos del pincel. Así es posible crear pinceles que se sientan más como medios reales: húmedos al inicio del trazo, agotándose progresivamente con la distancia y volviendo a cargarse para la siguiente pasada.

![material-state](/images/screens/material-state.jpg)

## Contacto con la superficie del pincel

El pincel también puede simular la pérdida intermitente de contacto con la superficie de pintura: las marcas rotas que aparecen cuando un lápiz, un carboncillo, un pincel seco o un rotulador casi agotado solo se apoya parcialmente en el papel.

Cuando la simulación de contacto está activada, el pincel está en contacto o levantado. En contacto, las marcas se depositan con normalidad. Levantado, no se deposita material y el trazo deja un hueco cuya longitud se elige al azar entre una distancia mínima y máxima. La transición es binaria: el efecto no cambia opacidad, tamaño, dureza, espaciado ni flujo, solo si se aplica pintura.

La facilidad con la que se pierde el contacto depende de un umbral de contacto, la presión del lápiz y, opcionalmente, la carga del pincel. Valores de umbral más altos hacen las pausas más frecuentes. La presión actúa como fuerza estabilizadora: una presión ligera aumenta la probabilidad de perder contacto, mientras que una presión firme hace más probable que el trazo se mantenga apoyado. Cuando la carga del pincel está activada, una carga baja puede hacer la marca más rota y una carga alta puede ayudar a mantener el contacto, como una herramienta que aún lleva material suficiente para adherirse a la superficie.

La pérdida se evalúa por la distancia recorrida del trazo y no por el número de pinceladas, de modo que los pinceles con espaciado denso o disperso se comportan de forma coherente. La función funciona tanto con renderizado basado en sellos como caligráfico, produciendo huecos coherentes a lo largo del trazo en lugar de pinceladas aisladas omitidas.

## Animación y variación

Las fuentes de pincel animadas pueden cambiar de fotograma a medida que avanza el trazo, dando a los pinceles sensación de movimiento y variedad. La aleatorización y la variación por trazo evitan que las marcas repetidas parezcan idénticas, mientras que una semilla estable puede conservar un carácter coherente cuando hace falta repetibilidad.

Estos comportamientos son útiles para pinceles que deben sentirse vivos: cerdas que se desplazan a lo largo del trazo, sellos texturizados que cambian sutilmente con el tiempo o herramientas con varios cabezales en las que cada uno tiene su propia personalidad.

## Flujo de trabajo centrado en el artista

La herramienta Pincel está organizada para que las decisiones habituales de pintura queden a mano y las opciones de configuración menos frecuentes permanezcan apartadas. La intención es mantener la herramienta accesible durante la pintura y, al mismo tiempo, admitir una personalización profunda del diseño del pincel.

En conjunto, el Pincel cubre tanto la pintura cotidiana como la creación de marcas especializada: bocetos rápidos, ilustración pulida, renderizado texturizado, trabajo expresivo con tinta y efectos procedimentales complejos comparten la misma base flexible.
