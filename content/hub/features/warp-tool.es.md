---
title: "Herramienta de deformación"
type: docs
---
La herramienta Deformar empuja, tira y hace fluir píxeles libremente por el lienzo. En Lumi va más allá que la mayoría de las implementaciones: puede deformar un grupo de capas completo (sin importar cuántas capas y máscaras anidadas contenga) como un único objeto unificado, sin aplanarlo ni perder ninguna estructura.

## Descripción general

Seleccione una capa y arrástrela para desplazar los píxeles en cualquier dirección. La deformación no es destructiva mientras trabaja: puede deshacer y rehacer trazos individuales, cambiar el tamaño del pincel o el comportamiento entre trazos y continuar refinando hasta que se comprometa. La confirmación aplica el mapa de desplazamiento acumulado de forma destructiva a los datos de píxeles de la capa.

Cuando se selecciona una **capa de grupo**, la herramienta opera en el grupo como un todo. Podrás ver e interactuar con una vista previa en vivo de todo el grupo compuesto. Al confirmar, la misma deformación se aplica de forma precisa e independiente a cada capa secundaria y máscara dentro del grupo, preservando la estructura de capas completa.

## Deformación de grupo

Deformar un grupo es la capacidad principal que distingue a la herramienta de deformación de Lumi.

### El problema que resuelve

En la mayoría de los programas de pintura, deformar una ilustración de varias capas requiere aplanar el grupo primero (destruyendo la estructura de capas) o deformar cada capa por separado e intentar unirlas a simple vista (tedioso e impreciso). Ninguno de los enfoques conserva la estructura original para una mayor edición no destructiva.

Lumi deforma todo el grupo como un solo elemento y luego distribuye exactamente la misma transformación a cada capa dentro de él.

### Cómo funciona

Cuando seleccionas un grupo y comienzas un trazo de deformación, Lumi crea una **capa de vista previa flotante** a partir de la proyección compuesta del grupo. Si el grupo tiene una máscara, la máscara se integra en la vista previa para que ésta represente con precisión la apariencia final. Pintas tus trazos de deformación directamente en esta vista previa; lo que ves es precisamente lo que obtienes.

Al confirmar, Lumi:

1. Aplica el desplazamiento a cada capa básica dentro del grupo (incluidas las capas profundamente anidadas en subgrupos), expandiendo el lienzo de cada capa lo suficiente para capturar el área de deformación completa.
2. Aplica el mismo desplazamiento a todas las máscaras dentro del grupo en la misma pasada.
3. Reanuda el cálculo automático de los límites del grupo para que el grupo cambie de tamaño para adaptarse a sus hijos recién deformados.
4. Recorta cada capa deformada a su contenido pintado real para mantener el tamaño de los archivos compactos.
5. Elimina la capa de vista previa y regenera la proyección grupal a partir de los hijos actualizados.

Todo esto sucede en un solo paso de deshacer. Después de confirmar, el grupo se ve exactamente como se veía en la vista previa, con todas las capas y máscaras intactas.

### Máscaras

La opción **Máscaras de deformación** (habilitada de forma predeterminada) hace que las máscaras en cada capa y grupo dentro del objetivo de deformación reciban la transformación de desplazamiento idéntica. Las máscaras de capa se mueven con sus capas: una máscara que estaba recortando el contorno de un personaje continúa recortando ese mismo contorno después de deformarse.

Cuando **Máscaras de deformación** está desactivado, sólo se desplaza el contenido de la capa; Las máscaras conservan sus posiciones originales.

## Opciones de herramienta

### Comportamiento

| Modo | Efecto |
| :--- | :--- |
| **Mover** | Empuja los píxeles en la dirección del trazo. El modo principal para la mayoría de los trabajos de deformación. |
| **Crecer** | Expande los píxeles hacia afuera desde el centro del pincel. |
| **Reducir** | Atrae los píxeles hacia el centro del pincel. |
| **Remolino en el sentido de las agujas del reloj** | Gira los píxeles en el sentido de las agujas del reloj alrededor del centro del pincel. |
| **Girar en sentido antihorario** | Gira los píxeles en el sentido contrario a las agujas del reloj alrededor del centro del pincel. |
| **Borrar** | Elimina el desplazamiento de deformación, restaurando los píxeles a sus posiciones originales. |
| **Suave** | Difunde el desplazamiento, suavizando las transiciones abruptas entre áreas deformadas y no deformadas. |

### Controles de pincel

- **Tamaño**: Diámetro del pincel de deformación en píxeles. Los pinceles más grandes desplazan áreas más amplias con una caída más suave; Los cepillos más pequeños brindan un control preciso y localizado.
- **Dureza**: Caída desde el centro hasta el borde. La alta dureza produce un desplazamiento uniforme en toda el área del cepillo; La baja dureza concentra el efecto en el centro.
- **Fuerza**: Qué tan lejos se desplazan los píxeles por trazo. Una fuerza más baja permite una forma sutil y gradual; una mayor fuerza produce movimientos dramáticos y rápidos.

### Sincronización del trazo

- **Trazo durante el movimiento** (solo modo de movimiento): aplica la deformación continuamente a medida que se mueve el mouse, en lugar de solo con un pulso de temporizador. Utilícelo para trazos fluidos, similares a pinceles, en los que desee que el desplazamiento siga el cursor directamente.
- **Trazo periódico**: Aplica deformación en un intervalo de tiempo fijo mientras se mantiene presionado el botón del mouse. Úselo para los modos Crecer, Reducir y Remolino donde la intención es una aplicación circular continua.
- **Tasa**: La frecuencia de aplicación de trazos periódicos.

### Calidad

- **Interpolación**: el método de muestreo utilizado al realizar la confirmación. Linear es rápido y fluido para la mayoría de los trabajos; Cubic y Nohalo brindan mayor fidelidad para los detalles más finos.
- **Vista previa de alta calidad**: utiliza la muestra de calidad de confirmación durante la vista previa interactiva. Más lento, pero la vista previa coincide exactamente con el resultado confirmado.

### Opciones de grupo

- **Expandir área de deformación** (solo deformación de grupo): la cantidad de píxeles agregados como un margen transparente alrededor de la vista previa del grupo en todos los lados. Esto le da al contenido desplazado espacio para moverse. El valor predeterminado de 256 px es suficiente para la mayoría de los trabajos; redúzcalo para imágenes grandes donde la memoria es importante, o auméntelo para trazos de desplazamiento muy grandes.
- **Máscaras de deformación**: si se aplica la misma deformación a las máscaras de capa y de grupo. Activado de forma predeterminada.

## Deshacer y Rehacer

Cada trazo es un paso de deshacer discreto dentro de la sesión de deformación. **Ctrl+Z** elimina el último trazo y restaura el mapa de desplazamiento a su estado anterior. **Ctrl+Y** (o **Ctrl+Shift+Z**) lo vuelve a aplicar. Puede retroceder por todo el historial de accidentes cerebrovasculares antes de cometerlo.

Al presionar **Escape** o cambiar de herramienta se descartan todos los trazos no confirmados y se restauran las capas a su estado original. No se escriben cambios hasta que usted los confirme explícitamente.

## Comprometerse

Haga clic en el botón **Confirmar** (o presione **Entrar**) para aplicar la deformación acumulada de forma destructiva. Para deformaciones grupales, esto activa la aplicación multicapa completa descrita anteriormente. El historial de deshacer de la deformación comprometida es entonces una única entrada en la pila de deshacer de la imagen, reversible con el estándar **Editar → Deshacer**.