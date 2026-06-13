---
title: "Herramienta Pincel"
type: docs
---
La herramienta Pincel es el instrumento de pintura principal de Lumi: una forma sensible y expresiva de dibujar, pintar, sombrear, texturizar y crear marcas directamente en el lienzo. Está diseñado para parecer inmediato y al mismo tiempo dar a los artistas espacio para dar forma a cómo se comporta un trazo.

Más que ser un único pincel fijo, actúa como un sistema de pintura. La forma, la textura, el movimiento, la presión, el tiempo y el color del pincel pueden contribuir a la marca final, lo que lo hace adecuado para trabajos de líneas limpias, pintura suave, efectos de medios secos, trazos caligráficos, texturas dispersas y formaciones de pinceles de múltiples cabezas.

![brush-tool](/images/screens/brush-tool.jpg)

## Marcas de pincel expresivas

Los pinceles pueden basarse en sellos de mapas de bits, formas procedimentales o fuentes animadas basadas en cuadros. Esto permite que un trazo varíe desde una simple marca redonda y suave hasta un cabezal de pincel con una textura rica o en evolución. El mismo motor de pintura puede admitir dibujos precisos, acumulaciones pictóricas, marcas decorativas y rupturas de estilo de medios naturales.

Cuando un pincel se vuelve visualmente complejo, la vista previa puede seguir siendo simplificada para que la pintura siga siendo responsiva y fácil de leer.

![tool-setup](/images/screens/tool-setup.jpg)


## Dinámica y respuesta de entrada.

La herramienta Pincel responde a entradas en vivo, como la presión del lápiz, la velocidad, la dirección, la inclinación y otros valores del controlador. Estas señales pueden influir en el trazo visible de muchas maneras: el grosor, la opacidad, el ángulo, la respuesta de la textura, el comportamiento del color, el espaciado y otras cualidades pueden cambiar a medida que se mueve la mano.

Esto hace que el pincel se sienta menos como un patrón estampado y más como un instrumento de dibujo físico. Un toque ligero puede producir marcas delicadas, un movimiento más rápido puede abrir texturas o formas, y el comportamiento sensible a la dirección puede ayudar a que los trazos sigan el gesto de la mano.

![dynamics](/images/screens/dynamics.jpg)

## Comportamiento del trazo

Los trazos pueden ser directos e inmediatos, o pueden ser asistidos por suavizado y estabilización. Estas características ayudan a reducir la vibración no deseada, suavizar los cambios abruptos y hacer que los movimientos más largos se sientan más controlados sin eliminar el carácter de la entrada del artista.

The Brush también admite diferentes enfoques para la acumulación de pintura. Puede comportarse como un trazo continuo, acumular pinceladas repetidas o emitir marcas a lo largo del tiempo mientras el puntero se mantiene en su lugar. Esta flexibilidad lo hace útil tanto para el trabajo de línea deliberado como para la construcción tonal más lenta.

Para marcas caligráficas o similares a tinta, el Pincel puede generar un trazo de forma más continua en lugar de depender únicamente de sellos repetidos. Esto produce formas fluidas, parecidas a cintas, que responden naturalmente al gesto y la velocidad.

![stroke](/images/screens/stroke.jpg)

## Captura de trazos y renderizado simulado

El pincel puede capturar una pequeña muestra de cómo se dibuja normalmente a mano un ajuste preestablecido y luego usar ese perfil al representar trazos definidos por geometría en lugar de movimiento en vivo. Las líneas rectas con Mayús y clic, los trazados con trazos y las selecciones con trazos pueden utilizar el patrón de presión y velocidad capturado del ajuste preestablecido de herramienta activa en lugar de comportarse como una línea mecánica plana.

Esto mantiene los trazos construidos más cerca del carácter del pincel. Una línea dibujada a partir de un camino puede comenzar suavemente, generar presión, disminuir o variar la respuesta de velocidad de la misma manera amplia que el trazo de la mano muestreado, sin dejar de seguir la forma exacta del camino, el borde de selección o el gesto de línea recta.

## PostprocesamientoEl pincel puede registrar un trazo a medida que lo dibujas y luego reproducir ese gesto capturado una vez que lo levantas, refinando el trazado antes de que se establezca la marca final. Puede dibujar libremente y aun así llegar a una dirección más clara, a esquinas más definidas o a una estructura más deliberada sin tener que dibujar con precisión mecánica.

Esto abre marcas de construcción rayadas y rayadas que se ajustan a ángulos limpios mientras mantienen la longitud y el carácter dibujados a mano, trazos de cinta estables en inclinación y repetición consciente de las esquinas que trata las curvas y los tramos rectos de manera diferente. Los pinceles de múltiples cabezales pueden compartir una ruta corregida mientras cada cabezal mantiene su propia variación, y la dinámica aún puede dar forma al trazo a lo largo de su curva final durante la reproducción. El posprocesamiento se aplica a los trazos dibujados en lugar de a la emisión continua de aerógrafo.

## Color y textura

Las pinceladas pueden utilizar el color de pintura activo, responder a degradados o variar el color mediante dinámicas. El manejo de texturas permite que el pincel cambie entre una cobertura sólida y marcas discontinuas que rozan la superficie, lo cual es útil para efectos de pincel seco, vetas y sombreados expresivos.

Debido a que el color y la textura pueden ser parte del mismo sistema dinámico que la forma y la opacidad, un solo trazo puede evolucionar a medida que se mueve por el lienzo en lugar de permanecer visualmente uniforme.

## Cabezales y formaciones de cepillos.

La herramienta Pincel puede pintar con más de un cabezal a la vez. Se pueden disponer varios cabezales alrededor del recorrido del trazo para crear marcas de punta, trazos en abanico, comportamiento similar a cerdas, patrones de pulverización, formaciones texturizadas o sombreados estructurados.

Estas cabezas pueden seguir la dirección del recorrido, variar entre sí y dispersarse de manera que el trazo se sienta orgánico en lugar de repetido mecánicamente. Esto es especialmente útil para pinceles de medios naturales, trazos decorativos, follaje, pelaje, sombreado y otras marcas que se benefician de una irregularidad controlada.

![brush-heads](/images/screens/brush-heads.jpg)

## Carga de pincel y recogida de pintura

El Pincel también puede simular cuánta pintura o material se encuentra actualmente en el pincel. A medida que continúa el trazo, esa carga puede disminuir gradualmente, permitiendo que las marcas se vuelvan más claras, más secas, más delgadas, más ásperas o más fragmentadas dependiendo de cómo se establezca la dinámica del pincel.

La carga se puede reintroducir entre pasadas, mantener en un nivel elegido o utilizar como señal de control en vivo para otros comportamientos del cepillo. Esto hace posible crear pinceles que se sienten más como medios reales: húmedos al comienzo de un trazo, agotados progresivamente a lo largo de la distancia y luego sumergidos nuevamente para la siguiente pasada.

![material-state](/images/screens/material-state.jpg)

## Contacto con la superficie del cepillo

El pincel también puede simular una pérdida intermitente de contacto con la superficie de la pintura: las marcas rotas que aparecen cuando un lápiz, un carboncillo, un pincel seco o un marcador parcialmente agotado se engancha solo parcialmente con el papel.

Cuando la simulación de contacto está habilitada, el cepillo está en contacto o levantado. Mientras están en contacto, las marcas se depositan normalmente. Mientras está levantado, no se deposita material y el golpe deja un espacio cuya longitud se elige aleatoriamente entre una distancia mínima y máxima. La transición es binaria: el efecto no cambia la opacidad, el tamaño, la dureza, el espaciado o el flujo, solo si se aplica la pintura.La facilidad con la que se pierde el contacto depende del umbral de contacto, la presión del lápiz y, opcionalmente, la carga del cepillo. Los valores de umbral más altos hacen que las pausas sean más frecuentes. La presión actúa como una fuerza estabilizadora: una presión ligera aumenta la posibilidad de perder el contacto, mientras que una presión firme hace que sea más probable que el golpe se mantenga bajo. Cuando la carga del cepillo está habilitada, una carga baja puede hacer que la marca se rompa más y una carga alta puede ayudar a mantener el contacto, similar a una herramienta que todavía lleva suficiente material para agarrar la superficie.

La pérdida se evalúa a partir de la distancia recorrida del trazo en lugar del número de pinceladas, por lo que los pinceles con espacios densos o escasos se comportan de manera consistente. La característica funciona tanto con renderizado caligráfico como basado en sellos, produciendo espacios coherentes a lo largo del trazo en lugar de pinceladas omitidas aisladas.

## Animación y variación.

Las fuentes de pincel animadas pueden cambiar de marco a medida que avanza el trazo, dando a los pinceles una sensación de movimiento y variedad. La aleatorización y la variación por trazo pueden evitar que las marcas repetidas parezcan idénticas, mientras que la siembra estable puede preservar un carácter consistente cuando se necesita repetibilidad.

Estos comportamientos son útiles para pinceles que deberían sentirse vivos: cerdas que se mueven a lo largo de un trazo, sellos texturizados que cambian sutilmente con el tiempo o herramientas con múltiples cabezales donde cada cabezal tiene su propia personalidad.

## Flujo de trabajo centrado en el artista

La herramienta Pincel está organizada para que las decisiones comunes de pintura estén al alcance de la mano, mientras que las opciones de configuración menos frecuentes permanecen fuera del camino. La intención es mantener la herramienta accesible durante la pintura y al mismo tiempo permitir una personalización profunda para el diseño del pincel.

En general, Brush está diseñado para cubrir tanto la pintura cotidiana como la creación de marcas especializadas: bocetos rápidos, ilustraciones pulidas, renderizado texturizado, trabajos de tinta expresivos y efectos de pincel de procedimiento complejos, todos comparten la misma base flexible.