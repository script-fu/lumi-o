---
title: "Mezcla de colores espectrales"
type: docs
---
El sistema de paleta de Lumi utiliza un modelo de color espectral para simular cómo se mezclan los pigmentos reales. El objetivo es hacer que la experiencia de crear y seleccionar colores de una paleta digital se comporte como mezclar pinturas físicas. Una vez que se aplica un color al lienzo, es RGB estándar.

## Qué significa la mezcla espectral

La mezcla RGB tradicional es aditiva: la combinación de dos valores RGB los promedia hacia un punto medio. La mezcla de pigmentos es sustractiva: cada pigmento absorbe ciertas longitudes de onda y su efecto combinado es más oscuro y, a menudo, cambia de tono.

Lumi modela esto utilizando una representación de reflectancia espectral de 10 bandas para los colores de la paleta, en lugar de RGB.

Esto produce resultados similares a los de la pintura: la mezcla de azul y amarillo produce verde, no gris. La mezcla de dos colores saturados produce un color que se vuelve neutro como lo hacen los pigmentos físicos.

El cálculo espectral se ejecuta durante la construcción de la paleta: cuando se generan entradas de paleta secundarias y terciarias y cuando el Mezclador de paletas combina dos colores principales. El color resultante se convierte a RGB lineal para visualización y pintura.

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

### Naranjas y Amarillos| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Naranja pirrol | PO73 | Rojo (escarlata) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Naranja cadmio | PO20 | Amarillo (Cuerpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarillo Cadmio | PY35 | Amarillo (Cuerpo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarillo cadmio pálido | PY35: Pálido | Amarillo (cadmio pálido) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Amarillo limón | PY3 | Amarillo (Limón) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Níquel Azo Amarillo | PY150 | Amarillo (Medio) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Oro Verde | PY129 | Amarillo-Verde (Oro) |

### Colores de la Tierra

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Siena tostada | PBr7:Quemado | Tierra (Marrón Rojo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Sombra quemada | PBr7:Ocre | Tierra (Neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Siena cruda | PBr7: Crudo | Tierra (Amarillo Marrón) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ocre amarillo | PY42 | Tierra (amarilla) |

### Verdes

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde ftalo (YS) | PG36 | Verde (tono amarillo ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridiano | PG18 | Verde (viridiano) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Tierra Verde | PG23 | Verde (Tierra Fría) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde Winsor (BS) | PG7 | Verde (tono azul ftalo) |### Azules y cianes

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Luz turquesa cobalto | PG50 | Cian (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Azul cerúleo | PB35 | Cian (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ftalo Turquesa | PB16 | Azul (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Azul cobalto | PB28 | Azul (Violeta-Magro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Azul Winsor | PB15 | Azul (ftalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramarino | PB29 | Azul (Violeta-Magro) |

### Violetas, Magentas y Rojos

| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violeta brillante | PV23 | Violeta (Dioxazina) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rosa permanente | PV19:Rosa | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinacridona magenta | PV19:Magenta | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Carmesí de alizarina permanente | PV19:Carmesí | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Violeta de perileno | PV29 | Magenta (Quinacridona) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perileno Granate | PR179 | Rojo (carmesí) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Rojo pirrol | PR254 | Rojo (escarlata) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Luz roja de pirrol | PR255 | Rojo (luz de pirrol) |

### Negros y blancos| Muestra | Nombre | Código CI | Familia |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Marte negro (cálido) | PBk11 | Negro (Marte) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Verde perileno | PBk31 | Negro (verde perileno) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Negro marfil (fresco) | PBk9 | Negro (Marfil) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lámpara Negra (Neutra) | PBk7 | Negro (Lámpara) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanco titanio (cálido) | PW6: Cálido | Blanco (titanio cálido) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanco titanio (neutro) | PW6 | Blanco (titanio neutro) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Blanco zinc (frío) | PW4 | Blanco (Zinc Frío) |

### Controlar los grises

Los grises de control son neutralizadores estandarizados que se utilizan para desaturar mezclas de manera predecible.

| Muestra | Nombre | Código CI |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris cálido | N_CALIENTE |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris neutro | N_NEUTRAL |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Gris frío | N_FRESCO |

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

## Los píxeles del lienzo son RGBEl sistema espectral opera enteramente dentro de la construcción de la paleta y la selección de colores. Cuando se aplica una pincelada, el color de primer plano, ya convertido a RGB lineal, es lo que se pinta. El lienzo almacena datos de píxeles RGB estándar.

La mezcla espectral mejora la experiencia de crear una paleta y elegir colores de manera consistente con el comportamiento físico de los pigmentos, sin cambiar la forma en que se almacenan o componen los datos de la imagen.