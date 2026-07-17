---
title: "Filtros"
type: docs
url: "hub/features/filters"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 312088430d35761f6df789821c1629c829e6eb1d2f8b4be58c5843c893c3c7ed
---

El menú Filtros de Lumi reúne ajustes correctivos, efectos de lente estilizados, generadores de textura procedimental, tratamientos inspirados en la impresión y herramientas de análisis en un solo lugar. El orden del menú es práctico, no académico: las herramientas de desenfoque y mejora están una junto a la otra, los efectos de distorsión e iluminación se agrupan por apariencia, y los generadores de textura o patrón se mantienen juntos cuando el objetivo es crear material de origen en lugar de modificar una imagen existente.

Los cuadros de diálogo de filtro siguen el mismo flujo de trabajo general. Los preajustes, la vista previa, la vista dividida y los controles de opacidad o fusión permiten afinar un efecto con rapidez; en las capas, el resultado puede permanecer como un filtro editable y no destructivo en lugar de fusionarse de inmediato. Lumi también conserva un historial reciente del uso de filtros, de modo que repetir el último efecto o reabrir el último diálogo forma parte del ritmo normal de pintura y no de una tarea aparte.

## Desenfoque

### Gaussian Blur

Gaussian Blur es el filtro de suavizado estándar de Lumi: un desenfoque limpio y uniforme con controles de tamaño horizontal y vertical independientes, manejo de bordes y opciones de kernel. Es la opción de uso general para enfoque suave, máscaras suavizadas, profundidad atmosférica y cualquier flujo de trabajo en el que el desenfoque deba permanecer neutro.

### Pixelize

Pixelize reduce el detalle en estructuras de bloques deliberadas en lugar de un desenfoque suave. Como el diálogo expone ancho y alto de bloque, desplazamientos, forma del píxel y comportamiento de relleno, sirve tanto como efecto de censura gruesa como de mosaico controlable o tratamiento gráfico de baja resolución.

### Selective Gaussian Blur

Selective Gaussian Blur suaviza dentro de las regiones intentando preservar los bordes más marcados. Es útil cuando una imagen necesita una textura más tranquila o menos ruido sin perder los contornos de forma que aún deben leerse con claridad.

### Lens Blur

Lens Blur es uno de los filtros de desenfoque más orientados a la ilustración de Lumi. Sus controles giran en torno a la forma del iris poligonal, la curvatura de las láminas, el estiramiento anamórfico, el realce de luces y una región de enfoque configurable, de modo que se comporta menos como un suavizador genérico y más como una herramienta estilizada de profundidad de campo con bokeh definido.

### Tilt-shift

Tilt-shift mantiene nítida una banda de enfoque controlable mientras desenfoca progresivamente la imagen por encima y por debajo. El ángulo de la banda, el suavizado de transición, el sesgo de perspectiva, la forma del iris y el realce de efecto miniatura del diálogo lo hacen muy adecuado para escenas con aspecto de maqueta, vistas arquitectónicas y cualquier composición en la que el enfoque deba leerse como una franja diseñada y no como una señal de profundidad circular.

### Circular Motion Blur

Circular Motion Blur difumina el detalle alrededor de un punto central, convirtiendo los bordes en estelas de rotación. Es la elección natural para sujetos en giro, energía de turbina o ilustraciones que necesitan una sensación de movimiento orbital.

### Linear Motion Blur

Linear Motion Blur estira el detalle en una dirección, simulando desplazamiento, movimiento de cámara o gesto rápido a lo largo del encuadre. Es especialmente útil cuando el movimiento debe sentirse direccional y gráfico en lugar de difuso.

### Zoom Motion Blur

Zoom Motion Blur irradia el detalle hacia fuera desde un centro, produciendo la sensación de una carrera hacia o alejándose del espectador. Funciona bien para momentos de impacto, líneas de velocidad y composiciones que necesitan la energía de un zoom de cámara sin repintar toda la imagen.

## Mejora

### High Pass

High Pass aísla el contraste local fino en lugar del cambio tonal amplio. Con solo escala y contraste que gestionar, es una herramienta directa para extraer detalle de borde, crear superposiciones nítidas o preparar pasadas de nitidez que deben enfatizar la estructura más que el color.

### Noise Reduction

Noise Reduction es el movimiento opuesto: suprime la variación fina no deseada para que las formas mayores se lean con más claridad. Es útil cuando material escaneado, texturas comprimidas o pasajes sobrepasados deben simplificarse antes de seguir pintando o filtrando.

### Sharpen

Sharpen utiliza un modelo de máscara de enfoque, con radio, cantidad y umbral que controlan con qué fuerza se refuerza el contraste local. En la práctica, sirve para recuperar claridad tras el desenfoque, el redimensionado de exportación o pasadas de acabado sutiles en las que el detalle debe destacar sin convertir cada píxel en ruido.

## Color

### Tonal Grading

Tonal Grading reasigna el color por rango tonal en lugar de remodelar el contraste o trazar una curva. La luminancia de cada píxel elige una mezcla suave de tres colores definidos por el usuario para sombras, medios tonos y luces; la imagen conserva así su estructura de claro a oscuro mientras la paleta cambia. La intensidad por región, un sesgo de equilibrio al estilo Lightroom (a la izquierda favorece el grado de sombra, a la derecha el de luces) y la suavidad de transición controlan hasta dónde llega cada color y con qué suavidad se superponen los grados. Está pensado para ilustración, cómics, arte conceptual y fotografía cuando el objetivo es un acabado o un look coherente.

## Distorsión

### Chromatic Aberration

Chromatic Aberration separa los canales de color hacia fuera desde un centro elegido, con controles de dirección radial o tangencial, sesgo entre pares de canales, caída y preservación de luminancia. Tanto el código como el diálogo lo tratan como herramienta bidireccional: puede añadir franjas de lente estilizadas para dar energía o invertir el signo para corregir una aberración leve en el material de origen.

### Lens Distortion

Lens Distortion remodela la imagen mediante curvatura de barril o cojín, términos de borde, compensación de zoom, desplazamientos del centro y aclarado de esquinas. Resulta útil tanto para corregir una imagen que se siente ópticamente curvada como para empujarla deliberadamente hacia un carácter de gran angular o lente retro.

## Iluminación

### Bloom

Bloom convierte las zonas brillantes en un resplandor controlado; umbral, suavidad, radio e intensidad definen hasta dónde se extiende la luz y con qué fuerza eleva la imagen. El control adicional de limitación de exposición lo mantiene útil como efecto de luces altas en lugar de un lavado automático.

### Sky

Sky es más que una superposición de tinte o degradado: representa un cielo analítico con los modelos Preetham, Hosek/Wilkie o Nishita. Como el diálogo expone proyección, ángulo del sol, turbidez, densidad atmosférica, altitud, controles del disco solar y exposición, puede construir desde un fondo claro sencillo hasta un atardecer o crepúsculo con mayor base física.

### Vignette

Vignette oscurece, colorea o incluso borra hacia los bordes de la imagen, con controles de forma, radio, suavidad, gamma, proporción, compresión, rotación y posicionamiento en el lienzo. Funciona como tratamiento clásico de borde fotográfico, pero es lo bastante flexible para actuar como máscara de encuadre o foco compositivo irregular.

## Ruido

### HSV Noise

HSV Noise aleatoriza tono, saturación y valor de forma independiente. Es útil cuando una imagen necesita vivacidad cromática o inestabilidad analógica sin descomponer por completo la estructura local.

### Hurl

Hurl es la versión extrema del ruido: sustituye los píxeles por colores completamente aleatorios. Conviene pensarlo como fuente de caos destructivo para trabajos de glitch, texturas desgastadas o máscaras que necesitan una ruptura agresiva.

### Pick

Pick sustituye cada píxel por un vecino elegido al azar, de modo que la imagen sigue relacionada con su origen en lugar de convertirse en estática pura. El resultado es una variación granular barajada que puede sentirse más orgánica que el ruido totalmente aleatorio.

### Spread

Spread dispersa los píxeles desplazándolos al azar dentro de un radio. Es útil cuando se busca una alteración sin movimiento: una superficie rota, un borde manchado o una textura desgastada que aún conserva las relaciones de color de la imagen de origen.

### Fractal

Fractal genera ruido Perlin fractal enlosable, lo que lo hace especialmente valioso como fuente reutilizable para máscaras, nubes, textura de papel, rupturas tipo terreno y superposiciones procedimentales. Al ser enlosable, puede alimentar flujos de trabajo mayores sin crear costuras evidentes.

### Blue Noise Grain

Blue Noise Grain es el generador de grano monocromo estilo película e impresión de Lumi. Los preajustes de tamaño de grano, el enmascaramiento de ruido azul, el sesgo de medios tonos, el sesgo de sombras y los controles de semilla del diálogo muestran que está diseñado para colocar el grano de forma uniforme y controlable, no solo para rociar motas monocromas al azar sobre la imagen.

### Risograph Grain

Risograph Grain parte de la misma lógica de grano pero la convierte en un efecto de impresión de dos planchas. Colores de tinta separados, equilibrio de planchas, desregistro deliberado y variación con semilla lo hacen adecuado para carteles, estética de impresión independiente e ilustraciones que deben sentirse físicamente sobreimpresas y no digitalmente perfectas.

### Halftone (FM)

Halftone (FM) crea un semitono estocástico modulado en frecuencia con ruido azul u otros métodos de umbral relacionados. Con modos de color monocromo, duotono y CMYK, además de controles de ganancia de punto y descorrelación de planchas, apunta a una textura similar a la impresión que se mantiene irregular y viva en lugar de caer en una cuadrícula rígida.

## Bordes

### Difference of Gaussians

Difference of Gaussians detecta bordes restando dos versiones desenfocadas de la imagen. Es un operador compacto y útil para mapas de borde, extracción de líneas estilizada y localización de transiciones estructurales sin comprometerse con un contorno umbralizado completo.

## Morfología

### Median

Median sustituye cada píxel por el valor mediano de su vecindario, lo que tiende a eliminar ruido aislado preservando mejor los contornos marcados que un simple desenfoque. Es un filtro de limpieza práctico para aplanar el parpadeo visual sin suavizar de inmediato toda la imagen.

### Dilate

Dilate expande las regiones claras hacia fuera con la misma lógica de vecindario sensible a la forma. En términos de creación de imagen, puede engrosar marcas claras, expandir formas luminosas o cerrar huecos oscuros pequeños.

### Erode

Erode hace el movimiento complementario: expande las regiones oscuras y retrae las claras. Es útil para adelgazar detalles luminosos, agrandar masas oscuras o ajustar máscaras y formas gráficas.

## Patrón

### Checkerboard

Checkerboard genera un patrón de mosaico alterno regular. Es sencillo, pero esa simplicidad lo hace útil para probar transparencia, crear máscaras, bloquear fondos gráficos o generar material geométrico limpio.

### Grid

Grid dibuja divisiones horizontales y verticales repetidas, lo que lo hace útil para guías de maquetación, fondos de diseño, ilustración técnica y enmascaramiento procedimental. Al generarse como filtro, el espaciado y la apariencia pueden afinarse sin construir el patrón a mano.

### Voronoi

Voronoi genera una textura celular enlosable a partir de puntos semilla, con controles de tipo de característica, métrica de distancia, aleatoriedad, detalle fractal y envoltura continua. En la práctica puede ir desde estructuras de celdas agrietadas limpias hasta patrones más orgánicos de piedra, piel, mapa o red abstracta.

### Wave

Wave produce patrones de bandas o anillos definidos por perfil de onda, disposición geométrica, distorsión, detalle fractal y desfase de fase. Es más que una simple herramienta de franjas: puede generar ondulaciones controladas, bandas topográficas, gráficos tipo moiré o campos de patrones concéntricos con ruido.

### Halftone (AM)

Halftone (AM) aplica una trama de puntos clásica con modulación de amplitud, con controles de frecuencia, forma de punto, nitidez, modo de color y ángulo CMYK para una estructura de impresión tipo roseta. Frente al semitono FM, es la opción más ordenada y claramente mecánica cuando el aspecto buscado es papel de periódico, litografía offset o geometría de trama deliberadamente visible.
