---
title: "Almacenamiento en caché de pincel"
type: docs
---
El almacenamiento en caché de pinceles está diseñado para que tus pinceles favoritos se sientan rápidos lo antes posible. En lugar de volver a calcular el mismo sello de pincel transformado una y otra vez, Lumi puede mantener un caché guardado de las formas de pincel que realmente usas y recargar ese caché automáticamente más tarde.

## Descripción general

La función se basa en la idea de que muchos pinceles expresivos todavía utilizan las mismas combinaciones prácticas de tamaño, ángulo, dureza y relación de aspecto durante la pintura. Cuando esas combinaciones se reutilizan, Lumi puede entregar el sello de pincel transformado directamente desde el caché en lugar de reconstruirlo.

El resultado es:

- inicio de trazo más rápido después de guardar un caché
- uso repetido más suave de los ajustes preestablecidos favoritos
- menos cálculos desperdiciados durante largas sesiones de pintura
- restauración automática de cachés guardados cuando se vuelve a utilizar el valor preestablecido

## Intención

El almacenamiento en caché de pinceles está diseñado para pinceles que utiliza con frecuencia: ajustes preestablecidos de pintura central, herramientas de entintado favoritas, pinceles secos texturizados y otros pinceles cuyos sellos transformados son lo suficientemente caros como para notarlos.

El objetivo no es precocer todos los estados teóricos del cepillo. El objetivo es permitir que el uso real de pintura complete primero los estados más valiosos y luego guardar ese caché lleno para que el pincel ya esté caliente la próxima vez que lo use.

## Cómo funciona

El almacenamiento en caché de pinceles funciona junto con la cuantización de pinceles.

Cuando la cuantización está habilitada para un ajuste preestablecido de dinámica, las salidas que afectan a la transformación se ajustan a pasos discretos. Eso le da a Lumi un conjunto finito de estados de pincel reutilizables. Mientras pintas:

1. Lumi comprueba si el sello transformado ya existe en la caché.
2. Si es así, el sello se reutiliza inmediatamente.
3. Si no es así, Lumi lo construye una vez y lo almacena.
4. Con el tiempo, el caché se llena con los estados del pincel que realmente utilizas.

Si guarda ese caché, Lumi puede cargarlo automáticamente más tarde para que el pincel comience más cerca de un estado de calentamiento en lugar de reconstruir todo desde cero.

## Flujo de trabajo típico

1. Elija un pincel preestablecido que utilice con frecuencia.
2. Habilite la cuantificación por su dinámica.
3. Pinte normalmente durante un tiempo para que el caché se llene orgánicamente.
4. Abra el **Editor de ajustes preestablecidos de herramientas** e inspeccione la sección **Caché de ajustes preestablecidos**.
5. Mire las métricas en vivo:
   - **Tasa de aciertos**
   - **Cobertura**
   - **Memoria**
6. Haga clic en **Guardar** cuando el caché parezca útil.
7. En sesiones posteriores, Lumi carga automáticamente el caché guardado cuando el ajuste preestablecido se activa.

Esto hace que el ajuste preestablecido se sienta rápido antes, especialmente para pinceles con transformaciones costosas o sellos grandes.

## Dónde encontrarlo

### Editor de dinámica

Utilice el **Editor dinámico** para controlar la cuantización:

- habilitar la cuantificación
- elige el recuento global de pasos
- opcionalmente anular el recuento de pasos por eje de salida

La cuantificación es lo que hace que el caché sea práctico al reducir la variación continua en contenedores reutilizables.

### Editor de ajustes preestablecidos de herramientas

Utilice el **Editor de ajustes preestablecidos de herramientas** para administrar el caché del ajuste preestablecido actual:

- **Guardar**: conserva la caché en memoria actual en el disco
- **Cargar**: restaura un caché previamente guardado
- **Memoria libre**: libera la memoria caché sin eliminar la copia guardada.
- **Eliminar**: elimina el caché guardado del disco.

El expansor **Preset Cache** también muestra la tasa de aciertos en vivo, la cobertura y el uso de memoria.

## Qué se almacena en caché

El almacenamiento en caché de pincel apunta a sellos de pincel transformados: se han resuelto los costosos resultados rasterizados después de que se hayan resuelto el tamaño, el ángulo, la dureza, la relación de aspecto y las entradas de transformación relacionadas.

Es más útil cuando:- el cepillo tiene un costoso trabajo de transformación
- el mismo preset se utiliza en muchas sesiones
- el pincel vuelve a visitar estados dinámicos similares repetidamente
- quick startup responsiveness matters

Es menos útil para pinceles cuyo estado de transformación cambia enormemente y rara vez se repite.

## Carga automática

Los cachés guardados están destinados a ayudar desde el inicio de una sesión, no sólo después de haber pintado por un tiempo.

Cuando existe un caché guardado para el ajuste preestablecido activo, Lumi puede cargarlo automáticamente para que su pincel favorito comience con muchos estados útiles ya disponibles. Esto reduce el período de arranque en frío y acerca el cepillo a la máxima capacidad de respuesta de inmediato.

## Seguridad de la memoria

El almacenamiento en caché de pinceles está diseñado para mejorar la velocidad sin hacerse cargo de la máquina.

Lumi rastrea el uso de la memoria caché, la expone en la interfaz de usuario y aplica límites de tiempo de ejecución bajo presión de memoria. Si el sistema tiene poca RAM disponible, el crecimiento de la caché se restringe automáticamente.

## Mejores casos de uso

El almacenamiento en caché de pinceles es especialmente bueno para:

- cepillos diarios favoritos
- pinceles texturizados utilizados en toda una pintura
- pinceles grandes y expresivos con un alto coste de transformación
- ajustes preestablecidos de pincel compartidos en flujos de trabajo de ilustración repetidos
- ajustes preestablecidos que deseas sentir "listos" tan pronto como los selecciones

## En resumen

El almacenamiento en caché del pincel le permite a Lumi conocer los estados del pincel que realmente usa, guardarlos y recuperarlos automáticamente más tarde. Es una función de velocidad práctica para los ajustes preestablecidos favoritos: pinte con el pincel, deje que el caché se llene, guárdelo y las sesiones futuras comenzarán más rápido.