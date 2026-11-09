---
title: "Herramienta Pincel"
type: docs
url: "hub/features/paintbrush"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 467281fdb592ef90257d2dc951c59516e623a5ae34360c4be446a8d487529330
---

La herramienta Pincel es el instrumento de pintura principal de Lumi: una forma sensible y expresiva de dibujar, pintar, sombrear, texturizar y crear marcas directamente en el lienzo. Está diseñada para sentirse inmediata y, al mismo tiempo, dar al artista margen para definir cómo se comporta un trazo.

Más que un único pincel fijo, actúa como un sistema de pintura. La forma del pincel, la textura, el grano de la superficie, el movimiento, la presión, el tiempo y el color pueden contribuir a la marca final, lo que la hace adecuada para líneas limpias, pintura suave, efectos de medios secos, trazos caligráficos, texturas dispersas, mechones de cerdas y formaciones de pincel con varios cabezales.

## Marcas de pincel expresivas

Los pinceles pueden basarse en sellos de mapa de bits, formas procedimentales o fuentes animadas por fotogramas. Un trazo puede ir desde una marca redonda y suave hasta un cabezal rico en textura o en evolución. El mismo motor de pintura admite dibujo preciso, acumulación pictórica, marcas decorativas y ruptura al estilo de medios naturales.

Cuando un pincel se vuelve visualmente complejo, la vista previa puede seguir simplificada para que la pintura siga siendo ágil y fácil de leer.

## Dinámica y respuesta de entrada

La herramienta Pincel responde a la entrada en directo: presión del lápiz, velocidad, dirección, inclinación y otros valores del controlador. Esas señales pueden influir en el trazo visible de muchas maneras: grosor, opacidad, ángulo, respuesta de textura, comportamiento del color, espaciado y otras cualidades pueden cambiar a medida que se mueve la mano.

Así el pincel se siente menos como un patrón estampado y más como un instrumento de dibujo físico. Un toque ligero puede producir marcas delicadas; un movimiento más rápido puede abrir textura o forma; y el comportamiento sensible a la dirección puede ayudar a que los trazos sigan el gesto de la mano.

## Comportamiento del trazo

Los trazos pueden ser directos e inmediatos, o asistidos por suavizado y estabilización. Estas funciones ayudan a reducir el temblor no deseado, suavizar cambios bruscos y hacer que los movimientos largos se sientan más controlados sin eliminar el carácter de la entrada del artista.

El Pincel también admite distintos enfoques de acumulación de pintura. Puede comportarse como un trazo continuo, acumular pinceladas repetidas o emitir marcas con el tiempo mientras el puntero permanece en su sitio. Esa flexibilidad lo hace útil tanto para el trabajo de línea deliberado como para la construcción tonal más lenta.

Para marcas caligráficas o al estilo de la tinta, el Pincel puede generar un trazo de forma más continua en lugar de depender solo de sellos repetidos. El resultado son formas fluidas, parecidas a cintas, que responden con naturalidad al gesto y la velocidad.

## Captura de trazo y renderizado simulado

El Pincel puede capturar una pequeña muestra de cómo se dibuja normalmente a mano un preajuste y usar ese perfil al renderizar trazos definidos por geometría en lugar de movimiento en directo. Las líneas rectas con Mayús y clic, los trazados con trazo y las selecciones con trazo pueden usar el patrón de presión y velocidad capturado del preajuste de herramienta activo en lugar de comportarse como una línea mecánica plana.

Así los trazos construidos se acercan más al carácter del pincel. Una línea dibujada a partir de un trazado puede empezar suave, ganar presión, atenuarse o variar la respuesta a la velocidad de la misma manera amplia que el trazo de mano muestreado, siguiendo la forma exacta del trazado, el borde de selección o el gesto de línea recta.

## Posprocesamiento

El pincel puede registrar un trazo mientras lo dibuja y reproducir ese gesto capturado al levantar, refinando el trazado antes de que se fije la marca final. Puede bocetar con libertad y aun así llegar a una dirección más clara, esquinas más definidas o una estructura más deliberada sin dibujar con precisión mecánica.

Esto abre el rayado y las marcas de construcción regladas que encajan en ángulos limpios conservando la longitud y el carácter dibujados a mano, trazos de cinta estables en inclinación y reproducción consciente de esquinas que trata curvas y tramos rectos de forma distinta. Los pinceles con varios cabezales pueden compartir un trazado corregido mientras cada cabezal conserva su variación, y la dinámica puede seguir modelando el trazo a lo largo de su curva final durante la reproducción. El posprocesamiento se aplica a trazos dibujados, no a la emisión continua de aerógrafo.

## Color y textura

Las pinceladas pueden usar el color de pintura activo, responder a degradados o variar el color mediante dinámicas. El manejo de textura permite alternar entre cobertura sólida y marcas rotas que rozan la superficie, útil para pincel seco, grano y sombreado expresivo.

Como el color y la textura pueden formar parte del mismo sistema dinámico que la forma y la opacidad, un solo trazo puede evolucionar al moverse por el lienzo en lugar de permanecer visualmente uniforme.

La textura de la punta del pincel rompe el sello mismo. La textura de superficie, que se describe a continuación, es el grano y el diente de la superficie que encuentra el trazo.

## Textura de superficie

La textura de superficie da al trazo el grano y el diente de un soporte: papel, lienzo u otro patrón. Lumi incluye granos pensados para ello. Las zonas claras del patrón dejan pasar la pintura. Las zonas oscuras la retienen, de modo que la marca se rompe como se rompe un medio seco sobre el diente.

Dónde se sitúa ese grano puede cambiar con el trazo. Cada trazo puede empezar en su propio lugar del patrón, y un giro puede hacer rodar un desplazamiento nuevo. El sombreado de ida y vuelta es donde esto se nota: cada inversión toma una fase nueva del diente, y las pasadas superpuestas construyen un sombreado roto y verosímil. Una curva gradual puede conservar la fase que ya está bajo el trazo, de modo que el diente se mantiene coherente a lo largo del recorrido.

Los pinceles estampados y los trazos caligráficos pueden adoptar este grano. Una fila de toques y una cinta continua de tinta pueden romperse sobre el mismo tipo de diente. En un trazo caligráfico la línea sigue siendo una sola marca con forma, y el grano decide dónde esa cinta queda abierta y dónde queda retenida.

Lo abierto que esté el diente puede cambiar a lo largo del trazo. La presión y otras entradas pueden pasar de una lectura más seca a otra más llena. En la lectura más seca los huecos permanecen despejados. A medida que el trazo se abre, las islas pintadas pueden crecer hacia esos huecos, de modo que el grano se llena desde sus bordes. En la lectura plena el trazo coincide con el patrón más abierto.

Los toques superpuestos dentro de un mismo trazo pueden acumular opacidad en las islas. También pueden depositar una sola capa de cobertura que un bucle posterior de ese trazo reescribe, de modo que el grano no se apila sobre sí mismo.

## Cabezales y formaciones de pincel

La herramienta Pincel puede pintar con más de un cabezal a la vez. Varios cabezales pueden disponerse alrededor del recorrido del trazo para crear marcas de punta, trazos en abanico, comportamiento tipo cerda, patrones de pulverización, formaciones texturizadas o rayado estructurado.

Esos cabezales pueden seguir la dirección del recorrido, variar entre sí y dispersarse de modo que el trazo se sienta orgánico y no mecánicamente repetido. Es especialmente útil para pinceles de medios naturales, trazos decorativos, follaje, pelo, rayado y otras marcas que se benefician de una irregularidad controlada.

Los pinceles generados también pueden construirse como campos de cerdas. La forma y la animación en el trazo de esos campos se describen más abajo.

## Forma de las cerdas

Un pincel de cerdas es un campo de marcas separadas contenidas en una forma generada. El contorno de esa forma decide dónde pueden situarse las cerdas: un círculo, un cuadrado, un rombo, un triángulo, una forma con púas, esquinas suavizadas o una proporción estirada. Cada cerda conserva su propia marca dentro de ese contorno.

El espaciado y el tamaño fijan lo apretados que van los pelos y lo grande que es cada uno. La variación de tamaño deja que las cerdas vecinas difieran, desde un cabezal uniforme hasta uno cuyos pelos van del tamaño completo hacia el vacío. La regularidad mueve el campo entre una retícula pareja y una dispersión más suelta. El agrupamiento atrae las cerdas hacia menos centros, de modo que el cabezal se lee como mechones. El campo también se puede ralear, y deja un conjunto más escaso de las mismas marcas.

Las puntas pueden seguir redondas. También pueden volverse polígonos pequeños que permanecen dentro de cada cerda. Las puntas pueden espaciarse de manera uniforme alrededor del círculo, o dejarse irregulares, y pueden situarse en el borde o retraerse hacia dentro. Donde las puntas se solapan, la marca que se ve es la más fuerte.

Cada cabezal de un pincel de varios cabezales puede llevar su propia disposición, de modo que un abanico de plumas es un conjunto de mechones emparentados.

El tamaño de las cerdas puede seguir la presión del lápiz y las demás entradas que dan forma a un trazo. Cuando ese tamaño cambia, el cabezal puede redibujarse, de modo que un toque ligero y una presión firme difieren en los pelos mismos.

## Cerdas a lo largo del trazo

Un pincel de cerdas puede cambiar mientras el trazo aún se está dibujando. Las cerdas individuales crecen y menguan. Unas se desvanecen y otras crecen en otro lugar, de modo que el carácter del cabezal viaja a lo largo de la marca. La densidad del mechón se mantiene estable mientras los pelos se reordenan.

La animación avanza con los toques del trazo. Puede empezar después de que la marca haya recorrido una distancia, o cuando la mano gira. Los estados intermedios se desarrollan a lo largo del recorrido, y el mismo gesto produce la misma secuencia.

Un aerógrafo es otro tipo de movimiento. Puede depositar una disposición de cerdas nueva a medida que emite, de modo que un trazo sostenido renueva la pulverización. Un trazo caligráfico conserva su pluma continua, y esa pluma puede seguir tomando textura de superficie. La animación de las cerdas pertenece al pincel de cerdas estampado.

## Carga del pincel y recogida de pintura

El Pincel también puede simular cuánta pintura o material lleva el pincel en cada momento. A medida que continúa el trazo, esa carga puede ir disminuyendo, de modo que las marcas se vuelvan más claras, secas, finas, ásperas o más rotas según la dinámica del pincel.

La carga puede reintroducirse entre pasadas, mantenerse en un nivel elegido o usarse como señal de control en directo para otros comportamientos del pincel. Así es posible crear pinceles que se sientan más como medios reales: húmedos al inicio del trazo, agotándose progresivamente con la distancia y volviendo a cargarse para la siguiente pasada.

## Contacto con la superficie del pincel

El pincel también puede simular la pérdida intermitente de contacto con la superficie de pintura: las marcas rotas que aparecen cuando un lápiz, un carboncillo, un pincel seco o un rotulador casi agotado solo se apoya parcialmente en el papel.

Cuando la simulación de contacto está activada, el pincel está en contacto o levantado. En contacto, las marcas se depositan con normalidad. Levantado, no se deposita material y el trazo deja un hueco cuya longitud se elige al azar entre una distancia mínima y máxima. La transición es binaria: el efecto no cambia opacidad, tamaño, dureza, espaciado ni flujo, solo si se aplica pintura.

La facilidad con la que se pierde el contacto depende de un umbral de contacto, la presión del lápiz y, opcionalmente, la carga del pincel. Valores de umbral más altos hacen las pausas más frecuentes. La presión actúa como fuerza estabilizadora: una presión ligera aumenta la probabilidad de perder contacto, mientras que una presión firme hace más probable que el trazo se mantenga apoyado. Cuando la carga del pincel está activada, una carga baja puede hacer la marca más rota y una carga alta puede ayudar a mantener el contacto, como una herramienta que aún lleva material suficiente para adherirse a la superficie.

La pérdida se evalúa por la distancia recorrida del trazo y no por el número de pinceladas, de modo que los pinceles con espaciado denso o disperso se comportan de forma coherente. La función funciona tanto con renderizado basado en sellos como caligráfico, produciendo huecos coherentes a lo largo del trazo en lugar de pinceladas aisladas omitidas.

## Animación y variación

Las fuentes de pincel animadas pueden cambiar de fotograma a medida que avanza el trazo, dando a los pinceles sensación de movimiento y variedad. La aleatorización y la variación por trazo evitan que las marcas repetidas parezcan idénticas, mientras que una semilla estable puede conservar un carácter coherente cuando hace falta repetibilidad.

Estos comportamientos son útiles para pinceles que deben sentirse vivos: sellos texturizados que cambian de forma sutil con el tiempo, o herramientas con varios cabezales en las que cada uno tiene su propia personalidad. Los pinceles de cerdas tienen su propia animación en el trazo, descrita más arriba.

## Trazos OpenGL

Un preajuste de herramienta puede pintar sus trazos con OpenGL. La diferencia se nota sobre todo en los aerógrafos grandes y en los sellos de textura grandes, que siguen a la mano a medida que crece la marca. La ventana de imagen también puede dibujar la pila de capas con OpenGL, lo que mantiene fluida la navegación por el lienzo. Ambos se describen en [OpenGL](/hub/features/opengl/).

## Flujo de trabajo centrado en el artista

La herramienta Pincel está organizada para que las decisiones habituales de pintura queden a mano y las opciones de configuración menos frecuentes permanezcan apartadas. La intención es mantener la herramienta accesible durante la pintura y, al mismo tiempo, admitir una personalización profunda del diseño del pincel.

En conjunto, el Pincel cubre tanto la pintura cotidiana como la creación de marcas especializada: bocetos rápidos, ilustración pulida, renderizado texturizado, trabajo expresivo con tinta y efectos procedimentales complejos comparten la misma base flexible.
