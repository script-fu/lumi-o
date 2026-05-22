---
title: "Filtros"
type: docs
---
El menú Filtros de Lumi reúne ajustes correctivos, efectos de lentes estilizados, generadores de texturas procesales, tratamientos inspirados en impresiones y herramientas de análisis en un solo lugar. El orden del menú es más práctico que académico: las herramientas de desenfoque y mejora se encuentran una al lado de la otra, los efectos de iluminación y distorsión se agrupan por apariencia, y los generadores de texturas o patrones se mantienen juntos cuando el objetivo es construir material de origen en lugar de modificar una imagen existente.

Los cuadros de diálogo de filtro siguen el mismo flujo de trabajo general. Los ajustes preestablecidos, la vista previa, la vista dividida y los controles de opacidad o fusión permiten ajustar un efecto rápidamente y, en las capas, el resultado puede permanecer como un filtro editable no destructivo en lugar de fusionarse inmediatamente. Lumi también mantiene un historial reciente del uso de filtros, por lo que repetir el último efecto o reabrir el último diálogo es parte del ritmo normal de pintura y no una tarea separada.

## Desenfoque

### Desenfoque gaussiano

Gaussian Blur es el filtro suavizante estándar de Lumi: un desenfoque limpio y uniforme con controles de tamaño horizontales y verticales separados, manejo de bordes y opciones de kernel. Es la opción de uso general para enfoque suave, máscaras suavizadas, profundidad atmosférica y cualquier flujo de trabajo en el que el desenfoque en sí deba permanecer neutral.

### Pixelizar

Pixelize reduce los detalles en estructuras de bloques deliberadas en lugar de un desenfoque suave. Debido a que el cuadro de diálogo expone el ancho y la altura del bloque, los desplazamientos, la forma del píxel y el comportamiento del relleno, funciona como un efecto de censura grueso y como un mosaico controlable o un tratamiento gráfico de baja resolución.

### Desenfoque gaussiano selectivo

El desenfoque gaussiano selectivo se suaviza dentro de las regiones al tiempo que intenta preservar los bordes más fuertes. Es útil cuando una imagen necesita una textura más tranquila o menos ruido sin perder los límites de forma más grandes que aún deben leerse con claridad.

### Desenfoque de lente

Lens Blur es uno de los filtros de desenfoque más centrados en la ilustración de Lumi. Sus controles se basan en la forma del iris poligonal, la curvatura de la hoja, el estiramiento anamórfico, el aumento de luces y una región de enfoque configurable, por lo que se comporta menos como un suavizador genérico y más como una herramienta estilizada de profundidad de campo con bokeh en forma.

### Cambio de inclinación

Tilt-shift mantiene nítida una banda de enfoque controlable mientras desenfoca progresivamente la imagen encima y debajo de ella. El ángulo de banda, la pluma, el sesgo de perspectiva, la forma del iris y el realce en miniatura del diálogo lo hacen muy adecuado para escenas de apariencia en miniatura, vistas arquitectónicas y cualquier composición donde el enfoque deba leerse como una franja diseñada en lugar de una señal de profundidad circular.

### Desenfoque de movimiento circular

El desenfoque de movimiento circular difumina los detalles alrededor de un punto central, convirtiendo los bordes en senderos de rotación. Es la elección natural para sujetos que giran, energía similar a una turbina o ilustraciones que necesitan una sensación de movimiento orbital.

### Desenfoque de movimiento lineal

Linear Motion Blur estira los detalles en una dirección, simulando viajes, movimientos de la cámara o gestos rápidos a lo largo del encuadre. Es especialmente útil cuando el movimiento debe parecer direccional y gráfico en lugar de difuso.

### Desenfoque de movimiento con zoom

Zoom Motion Blur irradia detalles hacia afuera desde un centro, produciendo la sensación de una carrera hacia o alejándose del espectador. Funciona bien para momentos de impacto, líneas rápidas y composiciones que necesitan energía de zoom de la cámara sin tener que volver a pintar toda la imagen.

## Mejorar

### Pase altoHigh Pass aísla un fino contraste local en lugar de un amplio cambio tonal. Con solo gestionar la escala y el contraste, es una herramienta sencilla para extraer detalles de los bordes, crear superposiciones nítidas o preparar pases de nitidez que deberían enfatizar la estructura más que el color.

### Reducción de ruido

La reducción de ruido es el movimiento opuesto: suprime las variaciones finas no deseadas para que los formularios más grandes se lean con mayor claridad. Es útil cuando es necesario simplificar material escaneado, texturas comprimidas o pasajes sobrecargados antes de seguir pintando o filtrando.

### Afilar

Sharpen utiliza un modelo de máscara de enfoque, en el que el radio, la cantidad y el umbral controlan la fuerza con la que se aplica el contraste local. En la práctica, esto lo hace adecuado para restaurar la claridad después del desenfoque, cambiar el tamaño de la exportación o realizar pases de acabado sutiles donde los detalles deben resaltar sin convertir cada píxel en ruido.

## Distorsionar

### Aberración cromática

La aberración cromática separa los canales de color hacia afuera desde un centro elegido, con controles para la dirección radial o tangencial, polarización entre pares de canales, caída y preservación de la luminancia. Tanto el código como el diálogo lo tratan como una herramienta bidireccional: puede agregar franjas de lente estilizadas para obtener energía o invertir el signo para corregir una leve aberración en el material original.

### Distorsión de la lente

La distorsión de la lente cambia la forma de la imagen a través de una curvatura estilo barril o alfiletero, términos de borde, compensación de zoom, desplazamientos centrales y brillo de esquinas. Eso lo hace útil tanto para corregir una imagen que se siente ópticamente doblada como para empujarla deliberadamente hacia un carácter de lente gran angular o retro.

## Iluminación

### florecer

Bloom convierte las áreas brillantes en un brillo controlado, con umbral, suavidad, radio y fuerza que definen hasta dónde se propaga la luz y con qué fuerza eleva la imagen. El control adicional de limitación de exposición lo mantiene utilizable como un efecto de resaltado en lugar de un lavado automático.

### Cielo

Sky es más que una superposición de tinte o degradado: representa un cielo analítico utilizando modelos de Preetham, Hosek/Wilkie o Nishita. Debido a que el diálogo expone la proyección, el ángulo del sol, la turbidez, la densidad atmosférica, la altitud, los controles del disco solar y la exposición, puede construir cualquier cosa, desde un simple fondo claro hasta una puesta de sol o un cielo crepuscular más físicamente fundamentado.

### Viñeta

La viñeta oscurece, colorea o incluso borra hacia los bordes de la imagen, con controles de forma, radio, suavidad, gamma, proporción, compresión, rotación y posicionamiento en el lienzo. Funciona como un tratamiento de bordes fotográfico clásico, pero es lo suficientemente flexible como para actuar como una máscara de encuadre o un foco de composición irregular.

## Ruido

### Ruido HSV

HSV Noise aleatoriza el tono, la saturación y el valor de forma independiente. Eso lo hace útil cuando una imagen necesita vivacidad de color o inestabilidad analógica sin romper completamente la estructura local.

### Lanzar

Hurl es la versión extrema del ruido: reemplaza los píxeles con colores completamente aleatorios. Es mejor considerarlo como una fuente de caos destructivo para trabajos de fallas, texturas desgastadas o máscaras que necesitan una ruptura agresiva.

### Escoger

Pick reemplaza cada píxel con un vecino elegido al azar, por lo que la imagen permanece relacionada con su fuente en lugar de volverse puramente estática. El resultado es una variación granular barajada que puede parecer más orgánica que un ruido completamente aleatorio.

### DesparramarLa dispersión dispersa los píxeles desplazándolos aleatoriamente dentro de un radio. Es útil cuando desea una interrupción inmóvil: una superficie rota, un borde manchado o una textura desgastada que aún conserva las relaciones de color de la imagen original.

### Difusión fractal

Fractal Spread es una versión más coherente y pictórica de spread creada para sangrado de tinta fibrosa. En lugar de mover cada píxel en una dirección aleatoria no relacionada, sigue un campo de fibra fractal de modo que los píxeles vecinos tiran en direcciones relacionadas, creando zarcillos, líneas de tinta difuminadas y difusión de granos de papel que aún pueden estar sesgados por el ángulo y la fuerza.

### Ruido de pigmento

Pigment Noise añade un comportamiento de pigmento estilo acuarela a las capas de pintura transparentes: un borde oscurecido a lo largo del borde alfa, variación de la línea de contacto rota, granulación interior y escasas motas más oscuras. Está diseñado para asentarse naturalmente después del trabajo con bordes húmedos o sangrado, convirtiendo una forma limpia y pintada en algo con mayor concentración de pigmento físico.

### Fractales

Fractal genera ruido Perlin fractal enlosable, lo que lo hace especialmente valioso como fuente reutilizable para máscaras, nubes, texturas de papel, rupturas similares a terrenos y superposiciones de procedimientos. Debido a que forma mosaicos, puede alimentar flujos de trabajo más grandes sin crear costuras obvias.

### Grano de ruido azul

Blue Noise Grain es el generador de grano monocromático estilo película e impresión de Lumi. Los ajustes preestablecidos de tamaño de grano del diálogo, el enmascaramiento de ruido azul, el sesgo de medios tonos, el sesgo de sombras y los controles de semilla muestran que está diseñado para colocar el grano de manera uniforme y controlable, no solo para rociar motas monocromáticas aleatorias sobre la imagen.

### Grano de risógrafo

Risograph Grain se basa en la misma lógica del grano pero la convierte en un efecto de impresión de dos planchas. Los colores de tinta separados, el equilibrio de planchas, el registro incorrecto deliberado y la variación de semillas lo convierten en una buena opción para trabajos de carteles, estética de impresión independiente e ilustraciones que deben parecer físicamente sobreimpresas en lugar de perfectas digitalmente.

### Medios tonos (FM)

Halftone (FM) crea un semitono estocástico modulado en frecuencia utilizando ruido azul o métodos de umbral relacionados. Con modos de color para monocromo, duotono y CMYK, además de controles de ganancia de punto y descorrelación de placa, su objetivo es lograr una textura similar a la de una impresión que se mantenga irregular y vivaz en lugar de caer en una cuadrícula rígida.

## Bordes

### Diferencia de gaussianos

La diferencia de gaussianos detecta bordes restando dos versiones borrosas de la imagen entre sí. Es un operador compacto y útil para mapas de bordes, extracción de líneas estilizadas y búsqueda de transiciones estructurales sin comprometerse con un contorno con umbral completo.

## Morfología

### Mediana

La mediana reemplaza cada píxel con el valor mediano de su vecindario, lo que tiende a eliminar el ruido aislado y al mismo tiempo preserva límites más fuertes mejor que un simple desenfoque. Es un práctico filtro de limpieza para aplanar pequeñas conversaciones visuales sin suavizar inmediatamente toda la imagen.

### Dilatar

Dilate hace crecer regiones más claras hacia afuera usando la misma lógica de vecindad consciente de la forma. En términos de creación de imágenes, puede espesar marcas brillantes, expandir formas claras o cerrar pequeños espacios oscuros.

### Erosionar

Erode hace el movimiento complementario, haciendo crecer las regiones más oscuras y retirando las más claras. Es útil para adelgazar detalles claros, agrandar masas oscuras o ajustar máscaras y formas gráficas.

## Patrón

### tablero de ajedrezEl tablero de ajedrez genera un patrón de mosaico alterno regular. Es simple, pero esa simplicidad lo hace útil para probar la transparencia, crear máscaras, bloquear fondos gráficos o crear material fuente geométrico limpio.

### Cuadrícula

Grid dibuja divisiones horizontales y verticales repetidas, lo que lo hace útil para guías de diseño, fondos de diseño, ilustración técnica y enmascaramiento de procedimientos. Debido a que se genera como un filtro, el espaciado y la apariencia se pueden ajustar sin necesidad de crear el patrón a mano.

### Voronói

Voronoi genera una textura celular enlosable a partir de puntos sembrados, con controles para tipo de entidad, métrica de distancia, aleatoriedad, detalle fractal y envoltura perfecta. En la práctica, puede pasar de estructuras limpias de células agrietadas a patrones de red más orgánicos, piel, mapas o redes abstractas.

### Ola

Wave produce patrones de bandas o anillos formados por el perfil de la forma de onda, la disposición geométrica, la distorsión, el detalle fractal y el desplazamiento de fase. Eso lo convierte en más que una simple herramienta de franjas: puede generar ondulaciones controladas, bandas topográficas, gráficos tipo muaré o campos de patrones concéntricos ruidosos.

### Semitono (AM)

Halftone (AM) aplica una trama de puntos clásica con modulación de amplitud, con controles de frecuencia, forma de punto, nitidez, modo de color y ángulo CMYK para una estructura de impresión estilo roseta. En comparación con los medios tonos FM, es la opción más ordenada y claramente mecánica cuando el aspecto deseado es papel de periódico, litografía offset o geometría de pantalla deliberadamente visible.