---
title: "Mapa de paleta"
type: docs
---
El Palette Map responde a una pregunta práctica para los pintores: dado un conjunto de pigmentos, ¿qué colores se pueden mezclar realmente a partir de ellos? A partir de los pigmentos de entrada de la paleta, explora de manera procedimental cada combinación (mezclas de dos pigmentos, mezclas de tres, variaciones tonales) y asigna los resultados a una rueda de colores. El resultado es una imagen del espacio de color alcanzable para ese conjunto específico de pigmentos.

El mapa también es una herramienta de navegación basada en coordenadas. Organiza cada mezcla generada por tono y luminosidad en una cuadrícula circular, de modo que toda la paleta se pueda leer de un vistazo y cada color tenga una dirección estable.

## Estructura de cuadrícula

El Mapa está dividido en una cuadrícula de 36 × 15:

- **36 sectores de tonos**: pasos de 10° alrededor de la rueda, centrados en los nombres de tonos principales.
- **15 celdas de luminosidad**: 3 celdas por banda de valor × 5 bandas (Clave alta, Media alta, Media, Media baja, Profunda), que van desde el blanco en el exterior hasta el negro en el centro.

Cada celda es una pequeña cuña en la rueda. Se dice que una entrada colocada en una celda tiene esa celda como **origen**: su dirección particular lógica en el mapa.

## Colores en celdas

Cuando varios colores compiten por la misma celda, solo un **ganador** se muestra de manera destacada:

1. Las entradas **principales** siempre ganan su celda, independientemente de los demás ocupantes.
2. Si no hay ningún Primario presente, gana la mezcla generada (Secundario o Terciario) con el **croma más alto**.

Las inscripciones que no ganan quedan en segundo lugar y permanecen accesibles mediante ciclos de clics (ver más abajo).

Las entradas personalizadas (mezclas guardadas) se representan como puntos cuadrados; Las mezclas generadas y los primarios se representan como puntos redondos.

## Haga clic en Ciclismo

Al hacer clic en una celda ocupada, se selecciona el ganador como color de primer plano. Al hacer clic en la misma celda nuevamente, se pasa al siguiente ocupante (mezclas generadas en segundo lugar, luego cualquier entrada personalizada guardada en esa dirección de la cuadrícula). Cada clic avanza un paso a través de la pila.

**Haga clic con el botón izquierdo** para dirigirse al primer plano. Cuando el color objetivo se establece en fondo (desde la caja de herramientas), en su lugar, los clics en ruta al fondo.

## Shift-Select: Cargando puntos finales del mezclador

Mantenga presionada **Shift** para ingresar al modo de carga de endpoints:

- **Clic izquierdo** asigna la entrada en la que se hizo clic como **Padre A (CCW)** en el Mezclador de paleta.
- **Haga clic derecho** para asignarlo como **Padre B (CW)**.

En este modo solo se pueden seleccionar las entradas de Clase A (primarias y mezclas personalizadas con procedencia intacta). Los terciarios están ocultos y los puntos que no son de Clase A están atenuados. Una breve superposición confirma que el modo está activo.

## Aspectos destacados de los padres de Mixer

Cuando el Mezclador de paleta tiene puntos finales Padre A y Padre B activos, ambos se marcan en el Mapa con **anillos de diamantes** (una forma de diamante con un borde negro). Estos aspectos destacados permanecen visibles incluso cuando se alternan otros elementos de visualización, por lo que los padres de combinación activos siempre son identificables.

## Origen vs Posición Visual

Cada entrada tiene dos posiciones en el Mapa:

- **Origen (celda de origen)**: la dirección de cuadrícula lógica a la que pertenece la entrada, fijada durante toda su vida.
- **Posición visual del punto**: donde el color realmente se representa en función de su tono y luminosidad perceptivos.

Con **Reubicación de mejor coincidencia**, cuando se guarda una mezcla, el sistema calcula la receta óptima para el color final y establece el origen para que coincida con la posición visual del color. Esto mantiene los colores guardados cerca de su ubicación visual en la rueda y hace que el mapa sea espacialmente coherente.

## Arrastrar mezclas guardadas

Las entradas personalizadas (mezclas guardadas) se pueden reposicionar arrastrando:1. Haga clic y mantenga presionada una entrada personalizada (punto cuadrado) y arrastre más allá del umbral de 5 píxeles.
2. El cursor cambia para indicar el modo de arrastre. Los aspectos destacados de los padres se actualizan en vivo a medida que se mueve por el mapa para mostrar los nuevos padres combinados en cada puesto de candidato.
3. El punto arrastrado se ajusta a la posición de muestra válida más cercana.
4. Liberación para comprometerse. La entrada adopta la receta de la celda de destino: sus padres, mezcla, tono y croma se actualizan para que coincidan, y su origen se actualiza para que coincida con la nueva posición visual.

Los movimientos de arrastre se pueden deshacer mediante **Editar → Deshacer**.

## Doble clic: alternar el espacio de trabajo del mapa

En el **Editor de paletas**, al hacer doble clic en cualquier entrada de paleta se activa y desactiva la vista del espacio de trabajo del Mapa de paletas. Esta es una forma rápida de cambiar entre explorar colores guardados y mezclarlos en el mapa sin usar un menú. El comportamiento de un solo clic (restaurar la receta de la entrada en el Mezclador) no se ve afectado.

## Superposición de lienzo

El Mapa de paleta se puede invocar directamente en el lienzo de la imagen como una superposición de pantalla completa haciendo clic en la muestra **Primer plano/Fondo** en la caja de herramientas. Esto proporciona una gran superficie de mezcla sin dedicar un panel permanente al Mapa.

## Muestra de color central

Una muestra circular se encuentra en el centro del agujero del anillo y refleja el color de cualquier celda sobre la que se encuentre el cursor:

- **Color al pasar el cursor**: cuando el cursor se posa sobre una entrada del mapa, la muestra se actualiza inmediatamente para mostrar el color de esa entrada.
- **Color seleccionado como respaldo**: cuando no se coloca el cursor sobre ninguna celda, la muestra muestra el resultado calculado del Mezclador de paletas para la entrada actualmente seleccionada. Si el mezclador aún no se ha resuelto, utiliza el color de visualización base de la entrada para que el espacio nunca quede en blanco.
- Un fino borde oscuro delimita la muestra en todo momento.
- Después de que el cursor se detiene brevemente sobre la muestra central, aparece un anillo exterior blanco y negro para indicar que el área es interactiva.
- **Al hacer clic en la muestra central** se cierra la superposición del lienzo y se vuelve a la vista de imagen normal (lo mismo que hacer clic fuera del anillo exterior).

## Tecla Alt: Modo de comparación de lienzos

Cuando la superposición del lienzo del Mapa de paleta está abierta, al mantener presionado **Alt** se revela temporalmente la imagen debajo:

- Toda la interfaz de usuario del mapa de paleta se vuelve invisible (su opacidad cae a cero), dejando al descubierto el lienzo.
- Una muestra circular de 64 píxeles sigue al cursor, rellena con el color de muestra actual del Mezclador de paletas, para que puedas estar al tanto de la mezcla activa mientras inspeccionas la imagen.
- Al soltar Alt se restaura el mapa de paleta con total opacidad.

Una etiqueta de sugerencia, *"Mantenga presionada la tecla Alt para ver la imagen"*, se muestra dentro de la vista del espacio de trabajo como recordatorio.