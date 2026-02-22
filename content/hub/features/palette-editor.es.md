---
title: "Editor de paleta"
type: docs
---
El Editor de paletas es donde creas y administras una paleta Lumi. Contiene su conjunto de pigmentos, almacena las mezclas que guarda desde el Mezclador de paletas, registra los colores que realmente usó mientras pintaba y le permite configurar la estructura de valores y los degradados de la paleta.

## Seleccionar una paleta

Una paleta es más que una colección de pigmentos: es un compromiso estilístico. Muchos artistas trabajan con un conjunto pequeño y fijo de pigmentos que conocen íntimamente: la forma en que se mezclan, los neutros que producen, los cambios de temperatura entre ellos. Esa familiaridad se convierte en parte de su voz visual. Un pintor puede mantener una paleta cálida y de bajo croma para el trabajo de figuras y una paleta separada de alto nivel para los paisajes, o puede hacer todo su trabajo dentro de un solo conjunto de cuatro pigmentos como una restricción deliberada que unifica un cuerpo de trabajo.

Lumi apoya esta forma de trabajar. Cada paleta tiene sus propios pigmentos, mezclas, estructura de valores y degradados. Al cambiar de paleta se cambia todo el sistema de colores: el mapa, el mezclador y las mezclas disponibles se actualizan para reflejar el nuevo conjunto.

Un menú desplegable en la parte superior del Editor de paletas selecciona la paleta activa. Lumi se envía con tres paletas en el grupo **Estándar**:

| Paleta | Personaje |
| :--- | :--- |
| **Predeterminado** | Una paleta versátil y de tendencia cálida que cubre toda la rueda de tonos. Buen punto de partida para la mayoría de las materias. |
| **Maestro** | Una paleta grande de espectro completo para pintores que desean una máxima cobertura de tonos y un control explícito sobre los ejes de gris. |
| **Zorn** | Una paleta limitada de cuatro pigmentos basada en el enfoque de Anders Zorn. Cubre una gama sorprendentemente amplia de tonos carnales cálidos y neutros de croma baja a partir de un conjunto de pigmentos mínimo. |

Las paletas también se pueden crear, importar o duplicar desde la pestaña Paletas.

## pigmentos de paleta

La sección **Pigmentos de paleta** en la parte superior de la vista de paleta enumera sus entradas principales: los pigmentos base a partir de los cuales se construye el resto de la paleta. Estas son las entradas al sistema de mezcla espectral. Los secundarios y terciarios se generan a partir de ellos automáticamente y se utilizan para completar el mapa de paletas.

## Mezclas guardadas

La sección **Mezclas guardadas** contiene colores que has conservado explícitamente del Mezclador de paleta usando **Agregar a paleta**. Estos son sus colores derivados: los resultados de la mezcla espectral, los ajustes de tono y croma guardados para su reutilización.

Las mezclas guardadas se subdividen en cinco bandas de valores:

| Banda | Rango de luminosidad predeterminado |
| :--- | :--- |
| Clave alta | 80 – 100% |
| Medio superior | 60 – 80% |
| Medio | 40 – 60% |
| Medio Bajo | 20 – 40% |
| Profundo | 0 – 20% |

Lumi coloca cada mezcla guardada en la banda apropiada automáticamente según su luminosidad perceptiva (CIE L\*). Esto organiza sus mezclas por valor en lugar de buscar en una lista plana y, por lo general, coincide con la forma en que un artista piensa sobre el color.

Se puede cambiar el nombre de las mezclas guardadas mediante el botón **Cambiar nombre personalizado** o el menú contextual.

## Mezclas usadas

La sección **Mezclas usadas** es un historial activado por pintura. Cada vez que se aplica un color de la paleta al lienzo, se registra aquí. Las mezclas usadas se ordenan de más a menos reciente.

Esta sección es útil para recuperar un color con el que pintó pero que no guardó explícitamente. Para conservar una mezcla usada de forma permanente, selecciónela y haga clic en **Promocionar** y pasará a Mezclas guardadas en la banda de valores adecuada.

Las mezclas utilizadas se almacenan por paleta y persisten entre sesiones.

## Bandas de valorLas bandas de valores definen dónde se ubican los límites entre las cinco zonas de luminosidad. De forma predeterminada, dividen la luminosidad de manera uniforme en el rango de 0 a 100%, pero puedes ajustarlos para que coincidan con la estructura tonal del sujeto. Es útil para los pintores definir y gestionar bandas de valores _y_ los espacios entre ellas.

### El control deslizante de banda de valor

El **expansor Bandas de valor** en el Editor de paleta contiene un control deslizante con cinco divisores que se pueden arrastrar. Arrastre cualquier divisor para cambiar el límite entre bandas adyacentes. La etiqueta encima del control deslizante muestra el nombre y el rango de porcentaje exacto de la banda activa.

**Botones:**

| Botón | Efecto |
| :--- | :--- |
| **Cancelar** | Revierte el control deslizante al último estado aplicado |
| **Copiar** | Copia la configuración de banda actual al portapapeles |
| **Pegar** | Pega una configuración de banda copiada de otra paleta |
| **Valores predeterminados** | Restaura los valores predeterminados de división igual de fábrica |
| **Aplicar** | Confirma los cambios y regenera la paleta |

**Se requiere Aplicar** para que los cambios sean permanentes. Activa una regeneración completa de la paleta y eliminará cualquier mezcla guardada cuya luminosidad ya no esté dentro de ninguna banda. Lumi muestra un cuadro de diálogo de confirmación que enumera cuántas mezclas se eliminarán antes de continuar.

### Bandas de valor y el mapa de paleta

El Mapa de paleta muestra la paleta como una rueda de tonos con 36 sectores de tonos (de 10° cada uno) y 15 celdas de luminosidad dispuestas como anillos concéntricos. Cada banda corresponde a tres anillos: las cinco bandas × 3 anillos = 15 celdas en total.

Al ajustar las bandas de valores se cambian los valores de luminosidad que aparecen en cada nivel de anillo. Una banda comprimida hacia el extremo oscuro hace que sus tres anillos abarquen una gama tonal más estrecha; una banda ancha le da a sus tres anillos una mayor distribución tonal. Así es como la misma estructura del Mapa de paletas se adapta a paletas ajustadas a diferentes prioridades tonales.

## Degradados de paleta

Cada paleta puede almacenar uno o más **Degradados**: progresiones suaves derivadas de entradas de paleta que se pueden aplicar al lienzo como rellenos degradados o usarse como franjas de referencia.

Los degradados se gestionan en el **Expansor de degradados**. El combo en la parte superior enumera los degradados en la paleta actual. **Agregar** crea un nuevo degradado. **Eliminar** elimina el seleccionado. **Cambiar nombre** le cambia el nombre.

### Editor de degradado

El **expansor del Editor de degradado** configura el degradado seleccionado. Cada degradado tiene tres puntos finales (**A**, **B** y **C**) que se muestran como muestras de color. Haga clic en una muestra para convertirla en el punto final activo para la edición.

Cada punto final se puede configurar haciendo clic en **Seleccionar** y luego haciendo clic en una entrada de paleta en el Mapa de paleta o en la vista de paleta. El punto final está vinculado a esa entrada de paleta mediante UID; si la entrada cambia, el gradiente se actualiza.

**Controles por punto final:**

| Controlar | Efecto |
| :--- | :--- |
| **Fuerza** | En qué medida contribuye el color del punto final en relación con sus vecinos |
| **Opacidad** | Alfa del color del punto final en el degradado |
| **Curva** | Ajuste de gamma para la caída del color desde este punto final |

**Controles deslizantes de distribución** (S1, S2, S3) establecen dónde caen los tres puntos medios entre los puntos finales a lo largo de la franja de degradado. Al restablecerlos, los puntos medios vuelven al mismo espacio.

La franja de vista previa del degradado en la parte superior del bloque del Editor de degradado muestra el resultado de la configuración actual del punto final y la distribución.

## Paleta acoplableLa **Paleta** acoplable (**Paneles > Paleta**) es un panel más sencillo centrado en la lectura para explorar y seleccionar colores de cualquier paleta. Muestra la misma vista de tres secciones (Pigmentos de paleta, Mezclas guardadas, Mezclas usadas) sin los expansores Bandas de valor y Degradados.

Un menú desplegable de selección de paleta en la parte superior le permite cambiar entre todas las paletas disponibles. Haga clic en cualquier entrada para establecerla como color de primer plano. Haga doble clic para abrir el editor de nombres de colores. Para paletas grabables, las acciones Editar color, Nuevo color desde FG y Eliminar color están disponibles en la barra de botones.

La paleta acoplable está diseñada para un acceso rápido al color durante la pintura cuando el editor de paleta completo ocuparía demasiado espacio.

## Pestaña Paletas

La **Pestaña Paletas** (disponible como pestaña acoplable) muestra la paleta activa en modo compacto. Excluye los pigmentos para centrarse en las mezclas guardadas.