---
title: "Capas y edición no destructiva"
type: docs
---
El sistema de capas de Lumi permite flujos de trabajo complejos y no destructivos con control total sobre la combinación, el enmascaramiento y la composición.

## Descripción general

Las capas son la base de la ilustración estructurada. Cada capa es independiente, con su propio modo de fusión, opacidad y máscara de capa opcional. Los grupos pueden anidar capas jerárquicamente con sus propias propiedades de fusión y recorte.

## Acceso

**Paneles** → **Capas**, o el panel **Capas** predeterminado a la derecha.

## Tipos de capa

### Capas de pintura

Capas ráster estándar para contenido pintado. Almacene datos de píxeles como buffers GEGL con transparencia alfa opcional.

### Agrupar capas

Contenedores jerárquicos para organizar capas relacionadas. Los grupos pueden tener su propio modo de fusión, opacidad y máscaras de recorte. Las proyecciones grupales se componen según demanda.

### Máscaras de capa

Máscaras de escala de grises adjuntas a cualquier capa, controlando la opacidad por píxel. Pintar una máscara con blanco vuelve opacos los píxeles; el negro los hace transparentes; el gris proporciona opacidad parcial.

## Modos de fusión

Cada capa tiene un modo de fusión que determina cómo se combina con las capas siguientes:

- **Normal**: fusión directa de opacidad.
- **Multiplicar**: oscurece multiplicando los valores de color.
- **Pantalla**: Aclara invirtiendo, multiplicando y volviendo a invertir.
- **Superposición**: Combinación de Multiplicar y Tramar.
- **Agregar**: Mezcla aditiva (suma valores de color).
- **Restar**: Mezcla sustractiva.
- **Color, Tono, Saturación, Luminosidad**: combinación de componentes HSL.

## Recorte y enmascaramiento

- **Modo compuesto: Recortar al fondo**: configurar el modo compuesto de una capa en **Recortar al fondo** restringe la composición a áreas donde las capas acumuladas de **Unión** debajo tienen opacidad establecida. La capa se pinta solo donde esas capas tienen contenido y no pueden expandir la huella alfa. Esto se establece por capa en el cuadro de diálogo Atributos de capa (menú desplegable **Modo compuesto**). Cuando el modo compuesto efectivo de una capa no es Unión, el ícono del ojo en el panel Capas se reemplaza con un ícono compuesto para indicar el comportamiento de composición no estándar.

  **Ejemplo: forma alfa compartida:** En un grupo, la capa inferior contiene un círculo relleno sobre un fondo transparente, configurado en el modo compuesto **Unión** predeterminado. Cada capa superior en el mismo grupo está configurada en **Recortar al fondo**. Esas capas solo se pueden pintar donde el círculo proporciona opacidad (una forma, muchas capas). Este es un patrón común para colorear, sombrear y detallar dentro de una silueta definida sin preocuparse por el derrame.
- **Máscaras de capa**: aplique una máscara en escala de grises para controlar la visibilidad de la capa píxel por píxel. Pintar de blanco la máscara revela; ocultaciones negras; el gris proporciona opacidad parcial.
- **Máscaras puramente infantiles**: las máscaras se almacenan como elementos secundarios dentro de la pila dibujable, lo que evita la pérdida de datos durante las transformaciones.

## Selección de capas (tecla Alt)

Al tocar **Alt** (Alt izquierda) mientras se desplaza sobre el lienzo, se selecciona la capa con píxeles visibles debajo del cursor, sin cambiar de herramienta ni hacer clic.

### Cómo funciona

- **Presione Alt**: el cursor cambia a una cruz, lo que indica que el modo de selección está activo.
- **Release Alt**: Lumi elige la capa no transparente más superior en la posición del cursor (opacidad > 25%) y la selecciona. La capa está resaltada en el panel Capas y la barra de estado muestra **"Capa seleccionada: 'nombre de capa'"**.
- Se dibuja un tirador en el punto central de la capa seleccionada en el lienzo. El controlador disminuye y se desvanece a medida que se aleja el cursor.

### Ciclismo a través de capasCada toque posterior de Alt en la misma ubicación selecciona la **siguiente capa hacia abajo** en la pila en ese punto. Lumi recuerda la última capa elegida y pasa a la que está debajo. Una vez que se llega al final de la pila, el siguiente toque regresa a la capa más superior en esa posición. Esto hace que sea sencillo llegar a capas anidadas en escenas complejas tocando Alt repetidamente.

### Reglas de cancelación

La selección se cancela (no se activa al soltar Alt) si ocurre cualquiera de las siguientes situaciones mientras se mantiene presionada Alt:

- Se presiona un botón del mouse (clic izquierdo o derecho).
- Se presiona cualquier otra tecla.

Esto garantiza que los gestos de arrastre Alt (como el ajuste del tamaño del pincel) y los atajos modificados con Alt funcionen sin cambiar accidentalmente la capa activa.

### Limitaciones

- La selección de capas no se activa durante las operaciones de la herramienta **Transformar**; Alt tiene un significado diferente allí.
- La selección no se produce si hay una selección flotante presente.
- Sólo la opción Alt izquierda activa la selección; right Alt se trata como un modificador estándar.

## Operaciones

En el panel Capas:

- **Crear capa**: haga clic derecho → **Nueva capa**, o use el menú **Capa**.
- **Duplicar**: haga clic derecho → **Duplicar**, o **Capa** → **Duplicar**.
- **Eliminar**: haga clic derecho → **Eliminar**, o seleccione y presione **Eliminar**.
- **Reordenar**: arrastre las capas hacia arriba o hacia abajo para cambiar el orden de apilamiento.
- **Cambiar nombre**: haga doble clic en el nombre de la capa.
- **Fusionar hacia abajo**: haga clic derecho → **Fusionar hacia abajo** para combinar con la capa siguiente.
- **Aplanar imagen**: **Imagen** → **Aplanar imagen** para fusionar todas las capas visibles.

## Propiedades de capa

- **Opacidad**: 0–100%, controla la transparencia general de la capa.
- **Modo de fusión**: menú desplegable para seleccionar cómo se combina la capa con las capas siguientes.
- **Visible/Oculto**: el icono del ojo alterna la visibilidad de la capa.

## Bloqueos de capa

Los iconos de candado se muestran en la fila del encabezado del panel Capas. Cada bloqueo se puede activar de forma independiente. Al hacer clic con el botón derecho en un ícono de candado, se configura exclusivamente (bloquea solo ese tipo y desbloquea todos los demás en la misma capa).

- **Lock Alpha**: Evita pintar en áreas transparentes. Las pinceladas sólo afectan a los píxeles que ya tienen opacidad; Los píxeles totalmente transparentes no se modifican. Útil para pintar dentro de formas existentes sin derramar fuera de ellas.

- **Bloquear máscara**: Impide editar la máscara de capa. La máscara permanece visible y activa, pero no se puede pintar ni modificar mientras este bloqueo está activado.

- **Color de bloqueo**: bloquea la pintura en un color específico: el color de primer plano actual en el momento en que se aplica el bloqueo. Los trazos posteriores en esta capa usan ese color almacenado independientemente del color de primer plano activo. Al desbloquearlo se descarta el color almacenado.

- **Bloquear contenido** (Bloquear píxeles): evita todas las ediciones de píxeles en la capa. La capa no se puede pintar, rellenar, transformar ni modificar de ningún otro modo. Útil para proteger capas acabadas.

- **Bloquear posición**: Evita que la capa se mueva o transforme. La capa aún se puede editar; sólo se bloquean los cambios posicionales (herramienta Mover, herramienta Transformar).

- **Bloquear visibilidad**: evita que el icono del ojo alterne la visibilidad de la capa. Protege capas que siempre deben permanecer visibles (u ocultas) durante la edición.

Todos los bloqueos se guardan con el proyecto y persisten entre sesiones.

## Efectos de capa (fx)

Los filtros GEGL no destructivos aplicados a través del menú **Filtros** se almacenan como efectos comprometidos en la capa en lugar de modificar los píxeles inmediatamente. Cuando una capa tiene al menos un efecto confirmado, aparece un icono **fx** en el panel Capas junto a esa capa.### Accediendo a la ventana emergente de efectos

Haga clic en el icono **fx** en una fila de capa en el panel Capas para abrir la ventana emergente **Efectos de capa** para esa capa.

La ventana emergente muestra la pila de filtros para la capa, con cada efecto confirmado enumerado por nombre con un interruptor de visibilidad al lado.

### Controles

- **Alternar visibilidad del ojo** (parte superior de la ventana emergente): activa o desactiva todos los efectos simultáneamente.
- **Alternancia de visibilidad por filtro**: cada fila de filtro tiene su propio ícono de ojo para habilitar o deshabilitar ese efecto de forma independiente.
- **Editar**: abre el cuadro de diálogo de configuración para el filtro seleccionado, lo que permite ajustar sus parámetros de forma no destructiva.
- **Subir/Bajar**: Mueve el filtro seleccionado hacia arriba o hacia abajo en la pila, cambiando el orden en que se aplican los efectos.
- **Fusionar**: confirma todos los efectos visibles actualmente en los píxeles de la capa, lo que hace que los cambios sean permanentes. El icono fx se elimina si se combinan todos los efectos. La combinación no está disponible en capas de grupo.
- **Eliminar**: Elimina por completo el filtro seleccionado. La ventana emergente se cierra automáticamente si no quedan efectos.

Al hacer doble clic en un filtro de la lista también se abre su cuadro de diálogo de edición.

**Editar** y **Eliminar** se bloquean si Bloquear píxeles está activo en la capa. Los filtros no se pueden reordenar mientras uno se está editando activamente.

### Agregar efectos

Aplicar un filtro de **Filtros** → (cualquier categoría). Si se apunta a la capa activa y la operación se ejecuta de forma no destructiva, el resultado se almacena como un efecto de capa en lugar de integrarse en los datos de píxeles. El icono fx aparece en la capa cuando hay al menos un efecto presente.

## Diálogo de atributos de capa

Haga doble clic en una capa en el panel Capas para abrir el cuadro de diálogo Atributos de capa.

### Identidad

- **Etiqueta de color**: Etiqueta de color para la organización visual en el panel Capas.

### Espacio y modo compuestos

- **Espacio compuesto**: el espacio de color utilizado al componer esta capa con las capas inferiores. Opciones: Automático, Lineal (RGB), Perceptual (RGB).
- **Modo compuesto**: controla cómo interactúa la capa alfa con el fondo. Las opciones incluyen Unión (afecta a todas las áreas, el valor predeterminado para el modo Normal), Recortar al fondo (solo afecta áreas con contenido existente, el valor predeterminado para la mayoría de los otros modos de fusión) e Intersección.

### Tamaño y compensaciones

Para una capa existente, **Tamaños** muestra las dimensiones de la capa y las dimensiones de la máscara (si hay una máscara adjunta) como etiquetas de solo lectura.

**Desplazamientos de capa**: los controles giratorios X e Y controlan la posición de la capa en el lienzo. Los cambios se aplican inmediatamente en lugar de cerrar el cuadro de diálogo.

Si la capa tiene una máscara, a continuación se muestran **Desplazamientos de máscara** (controladores X e Y para la posición independiente de la máscara).

Al crear una nueva capa, los campos Ancho y Alto y un menú desplegable **Rellenar con** (Primer plano, Fondo, Blanco, Transparente) reemplazan la visualización del tamaño de solo lectura.

### Atributos de capa (parásitos persistentes)

La sección inferior del cuadro de diálogo contiene una tabla desplazable de Nombre/Valor para parásitos persistentes (metadatos de valores-clave arbitrarios adjuntos a la capa). Estos valores se almacenan con el proyecto y se puede acceder a ellos desde la interfaz de secuencias de comandos de Scheme.

- Haga clic en cualquier celda de la columna Nombre o Valor para editarla en línea.
- **Agregar**: Agrega una nueva fila vacía.
- **Eliminar**: Elimina la fila seleccionada y su parásito de la capa.

Si la capa no tiene parásitos persistentes, se muestran tres filas iniciales vacías.

### Estado del contenidoUna línea de información de solo lectura en la parte inferior muestra el estado actual del contenido de la capa (y la máscara, si está presente): **Borrar**, **Uniforme** o **Mixto**. Un prefijo `*` indica que la capa tiene cambios sin guardar desde la última vez que se guardó.

## Rendimiento

- **Modo rápido**: al pintar en una sola capa anidada dentro de un grupo, Lumi cambia temporalmente los grupos de ancestros a renderizado de paso durante la duración del trazo, omitiendo la recomposición de proyección del grupo completo. Esto elimina el retraso en la actualización de la proyección anidada durante el entintado y el pintado. La composición completa se reanuda cuando termina el trazo, cambia la capa activa o antes de guardar.

  El modo rápido se desactiva cuando cualquiera de las siguientes condiciones se aplica a un grupo de antepasados:
  - El grupo tiene filtros visibles no destructivos (los filtros necesitan el búfer de proyección).
  - El modo de combinación del grupo no es **Normal** o **Paso a través**.
  - El grupo tiene un hijo directo que usa el modo compuesto **Recortar al fondo** o **Intersección** (estos requieren datos de fondo del búfer de proyección).

  El modo rápido tampoco se activa para capas de nivel superior, selecciones flotantes o cuando se apuntan a varias capas simultáneamente.

  Estructurar archivos para evitar estas condiciones en grupos de pintura, utilizando modos de fusión normales en capas, garantiza que el modo rápido permanezca activo durante toda una sesión de entintado o pintura.
- **Carga diferida**: los proyectos grandes se cargan rápidamente; los datos de la capa se cargan solo cuando son necesarios (por ejemplo, cuando se hacen visibles o se pintan).

## Formato de archivo

Todas las capas, máscaras y propiedades se almacenan en el formato abierto `.lum` de Lumi. El archivo es un directorio que contiene metadatos y buffers de capas individuales, lo que garantiza la compatibilidad y la accesibilidad a largo plazo.