---
title: "Herramienta Pincel"
type: docs
---
Paintbrush es la herramienta de pintura principal, diseñada para una pincelada inteligente y receptiva con control total sobre la presión, la velocidad, la inclinación y la dinámica de espaciado.

## Overview

La herramienta Pincel admite tipos de pincel rasterizados, generados por procedimientos y animados. Los trazos se pueden estabilizar, suavizar y posprocesar. La dinámica del pincel responde a la entrada del lápiz, brindando un control preciso sobre la opacidad, el tamaño, el color, el ángulo y otras propiedades durante un trazo.

## Tipos de pincel

### Pinceles ráster (.raster)

Imágenes de pincel de mapa de bits que admiten transparencia alfa.

### Pinceles generados (.param)

Formas procesadas (círculo, cuadrado, diamante, triángulo) con parámetros ajustables: dureza, relación de aspecto, ángulo, redondez y radio de esquina. Los pinceles generados son livianos y escalables.

### Pinceles animados (.anim)

Secuencias de cuadros secuenciales que avanzan durante los trazos. Los fotogramas pueden ciclarse de forma incremental (avances de fotograma por toque), seleccionarse aleatoriamente por toque o indexarse ​​por dinámica (presión, velocidad, inclinación, ángulo).

## Cursor de pintura

El cursor se adapta al estado actual de la herramienta para proporcionar información contextual clara:

- **Contorno del pincel**: el cursor sigue la forma y el tamaño exactos del pincel, brindando una vista previa en vivo de dónde aterrizará la pintura.
- **Modo de borrado**: cuando el borrado está activo, el contorno cambia a un círculo discontinuo para distinguir visualmente los trazos de borrado de los trazos de pintura.
- **Límite de pincel simple**: para pinceles complejos o muy grandes donde representar el contorno preciso es costoso, habilite **Límite de pincel simple** (en Opciones adicionales) para usar un círculo simple en su lugar.

## Opciones de herramienta

### Controles de nivel superior

Presente en todo momento, fuera de cualquier expansor:
- **Modo**: Modo de fusión de pintura (Normal, Multiplicar, Trama, etc.)
- **Opacidad**: opacidad general del trazo (0–100).

### Opciones de pincel

En el expansor **Opciones de pincel** (ampliado de forma predeterminada):
- **Tamaño**: Diámetro del pincel en píxeles.
- **Relación**: Aplasta o estira la forma del pincel (-1,0–1,0). 0 = sin modificar; los valores negativos rotan la calabaza 90°.
- **Ángulo**: Gira el sello del pincel (-180–180°). Independiente de la dinámica de la dirección del trazo.
- **Espaciado**: Distancia entre pinceladas pintadas como porcentaje del tamaño del pincel. Inferior = trazos más suaves; superior = patrón disperso.
- **Dureza**: Desvanecimiento suave (0,0) a borde afilado (1,0).
- **Fuerza**: Fuerza de aplicación con brocha (0,0–1,0). Oculto para la herramienta Lápiz.
- **Jitter**: compensa aleatoriamente cada posición de dab hasta esta cantidad de píxeles (0–1024).
- **Borrador**: multiplicador de tamaño que se aplica cuando este pincel se utiliza como borrador (0,1–10,0). No se muestra en la propia herramienta Borrador.

### Efectos del trazo

En el expansor **Efectos de trazo**:
- **Postproceso**: aplica estabilización, compresión de velocidad y corrección de repetición una vez completado el golpe, lo que mejora la consistencia sin latencia.
  - **Umbral de giro**: Umbral de ángulo (0–180°) para corrección de dirección en esquinas cerradas. 0 = corrección de dirección de salto.
  - **Velocidad de vista previa**: suprime la vista previa posterior al proceso cuando la velocidad del trazo excede este valor (0 = siempre vista previa).
- **Acumulación**: cuando está activado, cada toque acumula opacidad en lugar de componerse como un solo trazo.#### caligráfico
Cuando está activo, el estampado dab se reemplaza por un corredor geométrico continuo:
- **Ancho** y **Alto**: Dimensiones del corredor caligráfico.
- **Ángulo**: Orientación de la punta (grados).
- **Opacidad dinámica**: Modula la opacidad dentro del trazo según los cambios de velocidad y dirección. Funciona mejor con trazos finos y controlados; Los resultados son menos predecibles en garabatos rápidos. Experimental.
- **Crecimiento de velocidad** (0–100%): aumento de tamaño máximo permitido por muestra como porcentaje del tamaño de la muestra anterior. Limita la rapidez con la que puede crecer una dinámica de tamaño impulsada por la velocidad, evitando saltos repentinos cuando se acelera el golpe.
- **Reducción de velocidad** (0–100%): disminución máxima permitida del tamaño por muestra. Limita la rapidez con la que el tamaño puede disminuir cuando el trazo se desacelera.

#### Suavizado

Habilita el suavizado de entrada en tiempo real aplicado a la ruta del trazo mientras pinta. Se expande para revelar:
  - **Profundidad** (2–256): número de muestras de entrada anteriores consideradas al calcular la posición suavizada. Los valores más altos producen un retraso más prolongado y comprometido.
  - **Posición** (0–100): Intensidad del alisado aplicado a la posición del cepillo. Los valores más altos completan cambios bruscos de dirección.
  - **Presión** (0–100): Suavizado aplicado a la señal de presión del lápiz, reduciendo los picos de presión y la fluctuación.
  - **Dirección** (0–100): Suavizado aplicado a la dirección del trazo, estabilizando la dinámica sensible al ángulo.

#### Dinámica

Asigne entrada de lápiz u otros valores en vivo a los parámetros de pintura:

- **Presión** (lápiz óptico): controla el tamaño, la opacidad, la velocidad, la dureza, el color y más según la presión del lápiz.
- **Velocidad**: asigna la velocidad del trazo a las propiedades del pincel.
- **Inclinación**: los ángulos de inclinación X e Y del lápiz afectan el ángulo y otros parámetros.
- **Rueda**: entrada de la rueda del mouse o del lápiz óptico.
- **Dirección**: Ángulo de dirección del trazo.
- **Desvanecimiento**: desvanece la opacidad o el tamaño en un número fijo de toques.

Cada entrada dinámica se puede asignar a varias propiedades de forma independiente. Abra **Opciones de herramienta** → **Dinámica** para configurar.

#### Desvanecimiento y color

En el expansor **Fundido y color** (anidado dentro de Efectos de trazo; solo visible cuando **Sistema dinámico** está habilitado):

- **Ángulo inicial relativo**: el valor del **Ángulo inicial** se interpreta en relación con la dirección del trazo en lugar de como un ángulo absoluto del lienzo.
- **Ángulo inicial de desvanecimiento**: se desvanece desde el **Ángulo inicial** al inicio del trazo hacia el ángulo dinámico en vivo a lo largo del trazo. Al habilitar esto se activa **Ángulo inicial relativo**.
- **Ángulo inicial** (-180–180°): el ángulo del pincel al comienzo de un trazo, antes de que la dinámica tome el control.
- **Factor de fusión de ángulo** (0,0–1,0): controla la rapidez con la que el ángulo del pincel pasa del ángulo inicial al ángulo dinámico. 0 = mantiene el ángulo inicial; 1 = utiliza inmediatamente el ángulo completamente dinámico.
- **Estabilización de dirección** (0–100 px): retrasa la dinámica sensible a la dirección al requerir que el puntero recorra esta cantidad de píxeles antes de actualizar la dirección del trazo. Solo está activo cuando **PostProceso** está desactivado (PostProceso proporciona su propia estabilización). 0 = deshabilitado (dirección inmediata, puede saltar al inicio de la carrera).
- **Duración del desvanecimiento**: Distancia en unidades de lienzo sobre la cual se desarrolla el desvanecimiento.
- **Repetir**: Cómo se repite el desvanecimiento una vez que se agota la duración del desvanecimiento (Ninguno, Bucle, Diente de sierra, Triángulo).


### Cabezales de cepilloCabezales de cepillo coloca varios cabezales de cepillo independientes en un **anillo de órbita** circular centrado en la ruta del trazo. Cada cabezal pinta una pincelada completa en su propia posición cada vez que avanza el trazo, produciendo múltiples trazos paralelos o en abanico simultáneamente.

El radio de la órbita está determinado por el tamaño global del cepillo menos el tamaño de la cabeza: las cabezas más grandes se sitúan más cerca del centro; las cabezas más pequeñas orbitan más lejos. Las cabezas se espacian uniformemente alrededor del ring. Con dos cabezas, obtienes una a cada lado del trazo, creando una extensión simétrica que se comporta como una punta de caligrafía. El control deslizante **Seguir dirección** gira todo el anillo para permanecer perpendicular al trazo, de modo que la punta sigue la dirección de forma natural mientras pintas. Agregar más cabezas las abanica progresivamente alrededor del anillo, hasta un círculo de rociado completo en 16.

Los controles aparecen en el expansor **Cabezales de cepillo** en el panel de opciones de herramientas.

- **Count**: Número de cabezales de cepillo simultáneos (1–16).
- **Tamaño**: tamaño renderizado de cada cabeza en relación con el tamaño global del pincel (0,1–1,0).
- **Rigidez de las cerdas**: La rigidez con la que el radio de la órbita sigue el tamaño del pincel en escala dinámica. 0 = la órbita se expande y contrae con la presión; 1 = la órbita permanece fija al tamaño base.
- **Ángulo** (0–360°): Orientación estática del anillo de formación, utilizada cuando **Seguir dirección** es inferior a 1,0.
- **Seguir dirección** (0,0–1,0): con qué fuerza el anillo de formación sigue la dirección de desplazamiento del trazo. En 1,0, el anillo siempre es perpendicular a la dirección de desplazamiento; en 0.0 se bloquea en el valor estático **Ángulo**.
- **Variación de presión**: variación del tamaño por cabeza aplicada como un sesgo de presión independiente a través de las curvas dinámicas.
- **Variación de opacidad**: variación de opacidad por cabeza, independiente de la variación de tamaño.
- **Semilla de personaje** (0–255): Semilla fija para carácter por encabezado (tamaño, posición de relleno de espacio). La misma semilla reproduce la misma formación en cada golpe. Insensibilizado cuando **Aleatorizar personaje principal** está activado.

#### Scatter

Desplaza las cabezas a lo largo y alrededor del trazo en cada toque, creando efectos de difuminado y rociado.

- **Rellenar espacio** (0,0–1,0): extiende los cabezales a lo largo del espacio entre posiciones de dab consecutivas. El valor del carácter estable de cada cabeza determina su dirección de inclinación; a 1,0 cabezales llene todo el intervalo de espaciado. El carácter es estable por semilla.
- **Ángulo de dispersión** (0–90°, predeterminado 10°): ventila cada cabeza hacia afuera desde la dirección del trazo en un nuevo ángulo aleatorio hasta este valor. Sujetado a 90° para que ninguna cabeza mire hacia atrás.
- **Dispersión hacia adelante** (0–4000 px): dispersión aleatoria máxima delante de la dirección del trazo. Se vuelve a enrollar de forma independiente en cada toque.
- **Dispersión hacia atrás** (0–4000 px): dispersión aleatoria máxima detrás del trazo. Las cabezas todavía miran hacia adelante; sólo se invierte la dirección del desplazamiento. Tanto Forward como Backward pueden ser distintos de cero simultáneamente.
- **Equilibrio del tamaño de dispersión** (0,0–1,0): peso de dispersión mínimo para cabezas grandes. En 0 las cabezudas aterrizan cerca del trazo; en 1 todas las cabezas se dispersan por igual sin importar el tamaño.
- **Umbral de tamaño de dispersión** (1–100 px): las cabezas más pequeñas que este radio de píxel se dispersan a toda la distancia; las cabezas más grandes se acercan progresivamente al trazo.

#### Aleatorización

- **Randomize Head Character**: Re-draws per-head character values (size, scatter position) every stamp so the formation is fully chaotic along the stroke. Anula **Semilla de personaje**.
- **Aleatorizar cuadros de animación**: Para pinceles animados: cada cabezal avanza su cuadro de animación de forma independiente.

### Opciones adicionalesEn el expansor **Opciones adicionales** (contraído de forma predeterminada):

- **Bloquear para ver**: Mantiene la apariencia del pincel fija en relación con la vista del lienzo: cuando giras el lienzo, el pincel gira con él.
- **Límite de pincel simple**: utiliza un círculo simple para el contorno del cursor del pincel en lugar de representar la forma completa del pincel. Útil para pinceles complejos o grandes donde resulta costoso dibujar el límite preciso.
- **Vibración uniforme**: cuando está activado, los desplazamientos de dab del control deslizante **Vibración** se extraen de una distribución uniforme (cada desplazamiento es igualmente probable dentro del rango). Cuando está desactivado, la distribución es gaussiana (desplaza el grupo hacia el centro).
- **Restaurar los últimos colores utilizados**: Restaura los colores de primer plano y de fondo de la sesión anterior al inicio, en lugar de usar el blanco y negro de forma predeterminada.
- **Horizontal aleatorio**: 50 % de probabilidad de reflejar cada sello de izquierda a derecha por toque.
- **Vertical aleatorio**: 50 % de probabilidad de voltear cada sello al revés por toque.
- **Rotación aleatoria**: rota aleatoriamente cada sello 0°, 90°, 180° o 270° por toque.
- **Restablecer animación**: para pinceles animados: cuando está activado, la animación se reinicia desde el fotograma 0 con cada nuevo trazo; cuando está apagado, continúa desde donde terminó el trazo anterior.