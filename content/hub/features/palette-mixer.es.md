---
title: "Mezclador de paleta"
type: docs
---
El Mezclador de paletas obtiene nuevos colores a partir de pares de entradas de paleta mediante un proceso fijo de tres etapas. Debido a que la mezcla ocurre en el dominio espectral en lugar de RGB, los resultados se comportan como pigmentos físicos: el azul y el amarillo producen verde, los colores saturados cambian hacia neutros a medida que se mezclan.

## El oleoducto

Cada color producido por el mezclador pasa por tres etapas en un orden fijo:

1. **Mezcla**: WGM espectral entre el Padre A (CCW) y el Padre B (CW).
2. **Croma**: Difumina hacia el espectro neutro de la paleta, reduciendo la saturación.
3. **Tono**: Difumina para mezclar blanco (tinte) o mezclar negro (sombra).

El tono siempre se aplica al final. Esto hace que la luminosidad sea dominante: un ajuste de tono llega exactamente al nivel de luminosidad deseado sin verse diluido por el ajuste de croma que lo precede.

## Seleccionar padres

Padre A y Padre B son las dos entradas entre las que se mezcla el control deslizante de mezcla. Se cargan desde el Mapa de paleta:

- Mantenga presionada **Shift** en el mapa de paleta y **haga clic izquierdo** para configurar Parent A (CCW).
- Mantenga presionada **Shift** y **haga clic derecho** para configurar Parent B (CW).

Solo se aceptan como padres las entradas **Clase A** (primarias y mezclas personalizadas con procedencia intacta). Se excluyen los terciarios y las entradas con ascendencia perdida.

Las posiciones Principal A y Principal B del mezclador se muestran en el mapa como **anillos de diamantes** resaltados para que siempre puedas ver qué entradas están cargadas.

## Los controles deslizantes

| Control deslizante | Efecto |
| :--- | :--- |
| **Mezcla** | Se mueve entre el padre A (extremo CCW) y el padre B (extremo CW). En 0,0 el resultado coincide con el Padre A; en 1,0 coincide con el padre B. |
| **Croma** | Desatura la mezcla hacia el neutro de la paleta. Los valores más altos producen resultados más apagados y terrosos. |
| **Tono** | Cambia la luminosidad hacia la mezcla de blanco (dirección del tinte) o la mezcla de negro (dirección del tono). |

## Controles de valor

**Bloqueo de valor** congela la luminosidad perceptual (CIE L\*) en su nivel actual mientras los otros controles deslizantes se mueven. Utilícelo para explorar la variación de croma o tono sin cambiar el valor de una mezcla.

**Band Clamp** limita el resultado para que permanezca dentro de los límites de su banda de valor actual (por ejemplo, dentro de Lower Mid). El control deslizante de tono aún se puede arrastrar, pero la luminosidad de salida está fijada.

El control deslizante Tono también refleja cualquier brecha de valor configurada en el Editor de paleta. Los rangos de luminosidad que caen dentro de un espacio se muestran como bandas grises semitransparentes en el canal deslizante. El control deslizante salta automáticamente sobre estos espacios: al arrastrar a través de una región gris salta al límite de banda válido más cercano en el otro lado.

## Mezcla de puntos finales (blanco, negro, neutro)

Las etapas de tono y croma requieren puntos finales de referencia: una mezcla de blanco, una mezcla de negro y un neutro. Lumi los descubre automáticamente buscando en la paleta activa los mejores candidatos:

- **Mixing White**: el primario con mayor croma más cercano al blanco puro.
- **Mixing Black**: el primario con menor luminosidad.
- **Neutral**: el Primario más cercano a lo acromático (croma más bajo).

Estos se pueden anular manualmente haciendo clic derecho en una entrada en el Editor de paleta.

## Guardar una mezclaHaga clic en **Agregar a la paleta** para guardar el resultado actual del mezclador como una **Mezcla guardada** (entrada personalizada). Antes de guardar, el sistema aplica **Reubicación de mejor coincidencia**: busca en la paleta la receta óptima que produce el mismo color final con el mejor ajuste espacial en el mapa de paleta. Si se encuentra una receta más cercana, los controles deslizantes del mezclador saltarán para reflejarla, confirmando que el sistema encontró un origen mejor y la posición de la entrada guardada se alineará con su punto visual en el mapa.

Las mezclas guardadas almacenan su receta completa (UID A/B principal, factor de mezcla, tono, croma) para que puedan reproducirse exactamente.

## Recuperación de recetas

Al hacer un solo clic en una entrada personalizada en el Editor de paleta, se restaura la receta de esa entrada en el Mezclador:

- Se recargan los padres A y B.
- Los controles deslizantes de mezcla, tono y croma vuelven a sus posiciones originales.
- Se vuelve a habilitar cualquier bloqueo de valor o abrazadera de banda que estuviera activo durante la creación.

Esto hace que sea sencillo volver a un color y ajustarlo más, o usarlo como punto de partida para una nueva mezcla.