---
title: "Mezcla de colores espectrales"
type: docs
---
El sistema de paleta de Lumi utiliza un modelo de color espectral para simular cómo se mezclan los pigmentos reales. El objetivo es hacer que la experiencia de crear y seleccionar colores de una paleta digital se comporte como mezclar pinturas físicas. Una vez que se aplica un color al lienzo, es RGB estándar.

## Qué significa la mezcla espectral

La mezcla RGB tradicional es aditiva: la combinación de dos valores RGB los promedia hacia un punto medio. La mezcla de pigmentos es sustractiva: cada pigmento absorbe ciertas longitudes de onda y su efecto combinado es más oscuro y, a menudo, cambia de tono.

Lumi modela esto utilizando una representación de reflectancia espectral de 10 bandas para los colores de la paleta, en lugar de RGB.

Esto produce resultados similares a los de la pintura: la mezcla de azul y amarillo produce verde, no gris. La mezcla de dos colores saturados produce un color que se vuelve neutro como lo hacen los pigmentos físicos.

El cálculo espectral se ejecuta durante la construcción de la paleta, cuando se generan entradas de paleta secundaria y terciaria y cuando el Mezclador de paletas combina dos colores principales. El color resultante se convierte a RGB lineal para visualización y pintura.

## Perfiles de pigmentos

Las entradas de la paleta se pueden basar en datos de pigmentos reales utilizando **códigos de índice de color (CI)**. Cada familia de pigmentos CI tiene un sesgo espectral característico que influye en cómo se mezcla.

| Papel del pigmento | Comportamiento de mezcla | Ejemplo |
| :--- | :--- | :--- |
| **Primario** | Alto croma, secundarias limpias | PY3 (amarillo limón), PR122 (magenta) |
| **Cuerpo** | Tono de masa fuerte y opaco, cambia a oliva en mezclas de verdes | PY35 (amarillo cadmio), PR108 (rojo cadmio) |
| **Neutralizador** | Desatura y silencia rápidamente | PBk11 (Negro Marte), PBr7 (Siena) |
| **Ancla cromática** | Alto poder colorante, domina las mezclas | PB29 (azul ultramarino), PG7 (verde ftalo) |

Agregar primarios con códigos CI a una paleta le da al motor de mezcla un sesgo espectral preciso para esos colores, por lo que las mezclas secundarias y terciarias generadas reflejan el comportamiento de mezcla del mundo real.

## pigmentos luminosos

La paleta Master se envía con los siguientes pigmentos. Las muestras muestran la apariencia típica de cada pigmento (sin diluir, sin diluir).

### Naranjas y Amarillos

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "245,135,20" >}} | Naranja pirrol | PO73 | Rojo (escarlata) |
| {{< swatch "243,114,64" >}} | Naranja cadmio | PO20 | Amarillo (Cuerpo) |
| {{< swatch "240,180,80" >}} | Amarillo Cadmio | PY35 | Amarillo (Cuerpo) |
| {{< swatch "245,210,25" >}} | Amarillo cadmio pálido | PY35: Pálido | Amarillo (cadmio pálido) |
| {{< swatch "250,230,5" >}} | Amarillo limón | PY3 | Amarillo (Limón) |
| {{< swatch "225,155,10" >}} | Níquel Azo Amarillo | PY150 | Amarillo (Medio) |
| {{< swatch "180,175,45" >}} | Oro Verde | PY129 | Amarillo-Verde (Oro) |

### Colores de la Tierra

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Siena tostada | PBr7:Quemado | Tierra (Marrón Rojo) |
| {{< swatch "117,66,0" >}} | Sombra quemada | PBr7:Ocre | Tierra (Neutro) |
| {{< swatch "205,68,35" >}} | Siena cruda | PBr7: Crudo | Tierra (Amarillo Marrón) |
| {{< swatch "187,124,25" >}} | Ocre amarillo | PY42 | Tierra (amarilla) |

### Verdes

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Verde ftalo (YS) | PG36 | Verde (tono amarillo ftalo) |
| {{< swatch "64,130,109" >}} | Viridiano | PG18 | Verde (viridiano) |
| {{< swatch "128,138,112" >}} | Tierra Verde | PG23 | Verde (Tierra Fría) |
| {{< swatch "0,110,100" >}} | Verde Winsor (BS) | PG7 | Verde (tono azul ftalo) |

### Azules y cianes

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Luz turquesa cobalto | PG50 | Cian (Mineral) |
| {{< swatch "0,148,214" >}} | Azul cerúleo | PB35 | Cian (Mineral) |
| {{< swatch "0,100,110" >}} | Ftalo Turquesa | PB16 | Azul (ftalo) |
| {{< swatch "0,123,194" >}} | Azul cobalto | PB28 | Azul (Violeta-Magro) |
| {{< swatch "0,75,115" >}} | Azul Winsor | PB15 | Azul (ftalo) |
| {{< swatch "27,63,148" >}} | Ultramarino | PB29 | Azul (Violeta-Magro) |

### Violetas, Magentas y Rojos

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Violeta brillante | PV23 | Violeta (Dioxazina) |
| {{< swatch "230,90,180" >}} | Rosa permanente | PV19:Rosa | Magenta (Quinacridona) |
| {{< swatch "190,40,120" >}} | Quinacridona magenta | PV19:Magenta | Magenta (Quinacridona) |
| {{< swatch "160,30,65" >}} | Carmesí de alizarina permanente | PV19:Carmesí | Magenta (Quinacridona) |
| {{< swatch "120,35,65" >}} | Violeta de perileno | PV29 | Magenta (Quinacridona) |
| {{< swatch "135,10,45" >}} | Perileno Granate | PR179 | Rojo (carmesí) |
| {{< swatch "215,30,60" >}} | Rojo pirrol | PR254 | Rojo (escarlata) |
| {{< swatch "225,55,65" >}} | Luz roja de pirrol | PR255 | Rojo (luz de pirrol) |

### Negros y blancos

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Marte negro (cálido) | PBk11 | Negro (Marte) |
| {{< swatch "18,28,12" >}} | Verde perileno | PBk31 | Negro (verde perileno) |
| {{< swatch "10,18,19" >}} | Negro marfil (fresco) | PBk9 | Negro (Marfil) |
| {{< swatch "18,18,18" >}} | Lámpara Negra (Neutra) | PBk7 | Negro (Lámpara) |
| {{< swatch "255,249,235" >}} | Blanco titanio (cálido) | PW6: Cálido | Blanco (titanio cálido) |
| {{< swatch "255,255,255" >}} | Blanco titanio (neutro) | PW6 | Blanco (titanio neutro) |
| {{< swatch "245,250,255" >}} | Blanco zinc (frío) | PW4 | Blanco (Zinc Frío) |

### Controlar los grises

Los grises de control son neutralizadores estandarizados que se utilizan para desaturar mezclas de manera predecible.

| Muestra | Nombre | Código CI |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Gris cálido | N_CALIENTE |
| {{< swatch "128,128,128" >}} | Gris neutro | N_NEUTRAL |
| {{< swatch "120,128,135" >}} | Gris frío | N_FRESCO |

## El mapa de la paleta

El Mapa de paleta visualiza la paleta activa como una rueda de tonos: 36 sectores de tonos (pasos de 10°) × 15 celdas de luminosidad. Cuando se agregan primarios, el sistema genera mezclas secundarias y terciarias y las coloca en las posiciones apropiadas del mapa.

Al hacer clic en una celda, se selecciona un color como primer plano. Mayús y clic lo asignan como punto final principal en el Mezclador de paleta.

## El mezclador de paleta

El Mezclador de paletas deriva nuevos colores a partir de dos entradas principales mediante un proceso fijo de tres etapas:

1. **Mezcla**: WGM espectral entre el Padre A (CCW) y el Padre B (CW).
2. **Croma**: Difumina hacia el espectro neutro de la paleta, reduciendo la saturación.
3. **Tono**: Difumina para mezclar blanco o mezclar negro, ajustando la luminosidad.

El tono se aplica al final para que los ajustes de luminosidad no se diluyan con los cambios de croma. Los controles Value Lock y Band Clamp restringen los resultados a un nivel de luminosidad o banda de valores específicos.

Los colores mezclados se pueden guardar en la paleta como entradas **Personalizadas**, almacenando la receta completa (UID principales, factor de mezcla, tono, valores cromáticos) para su posterior recuperación.

## Los píxeles del lienzo son RGB

El sistema espectral opera enteramente dentro de la construcción de la paleta y la selección de colores. Cuando se aplica una pincelada, el color de primer plano (ya convertido a RGB lineal) es lo que se pinta. El lienzo almacena datos de píxeles RGB estándar.La mezcla espectral mejora la experiencia de crear una paleta y elegir colores de manera consistente con el comportamiento físico de los pigmentos, sin cambiar la forma en que se almacenan o componen los datos de la imagen.