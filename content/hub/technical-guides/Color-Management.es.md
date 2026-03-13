---
title: "Gestión del color"
type: docs
weight: 15
---
Lumi-o está configurado para funcionar desde el primer momento. Siempre que esté trabajando en una imagen con **16 bits o mayor precisión**, el software ya está configurado para utilizar la prueba en pantalla (CMYK) predeterminada y los perfiles sRGB integrados; Todo debería funcionar sin ninguna configuración.

Para aquellos que necesitan un control más profundo, esta guía explica el modelo central de gestión del color de Lumi, la diferencia entre un perfil de imagen y un perfil de prueba en pantalla, dónde se encuentran los controles y exactamente cómo se combinan los perfiles predeterminados con la aplicación.

## Resumen rápido

Lumi utiliza tres roles de perfil diferentes:

1. **Perfil de trabajo de imagen**
   - Define lo que significan los números RGB o en escala de grises de la imagen.
   - Se utiliza para operaciones de asignación/conversión.
   - Ejemplos típicos: sRGB integrado, Adobe RGB.

2. **Mostrar perfil**
   - Describe su monitor.
   - Se utiliza para mostrar la imagen correctamente en su pantalla.
   - Generalmente proporcionado por el sistema o elegido en Preferencias.

3. **Perfil a prueba de software**
   - Simula otro dispositivo de salida o condición de impresión.
   - **No** redefine los valores de píxeles de la imagen.
   - Ejemplos típicos: perfiles de prensa CMYK como `CoatedFOGRA39`.

## Perfil de imagen versus perfil de prueba en pantalla

### Imagen de perfil

Utilízalo cuando quieras decirle a Lumi en qué espacio de color se encuentra realmente la imagen.

Dos operaciones comunes:

- **Asignar perfil**
  - Cambia la etiqueta de perfil adjunta a la imagen.
  - **No** convierte valores de píxeles.
  - Úselo solo cuando los números de píxeles ya estén en el espacio de ese perfil.

- **Convertir a perfil**
  - Convierte valores de píxeles del perfil de imagen actual a uno nuevo.
  - Úselo cuando desee que la imagen realmente se mueva a un espacio de trabajo diferente.

**Ubicaciones del menú:**
- Imagen > Gestión del color > Asignar perfil de color...
- Imagen > Gestión de color > Convertir a perfil de color...

### Perfil de prueba suave

Utilícelo cuando desee obtener una vista previa de cómo se reproduciría la imagen en un dispositivo de destino o condición de impresión.

Pruebas en pantalla:
- deja el espacio de trabajo de la imagen solo
- cambia la canalización de vista previa
- puede marcar colores fuera de gama
- está destinado a la vista previa, no a la reasignación de datos de imagen

**Ubicaciones del menú:**
- Imagen > Gestión del color > Configuración de prueba en pantalla > Elegir perfil de prueba en pantalla...
- Imagen > Gestión del color > Configuración de prueba en pantalla > Intención de renderizado
- Imagen > Gestión del color > Configuración de prueba en pantalla > Compensación de punto negro
- Ver > Gestión de color > Activar vista previa de prueba en pantalla
- Ver > Gestión de color > Marcar colores fuera de la gama

## Cómo ver la vista previa de prueba en pantalla

Hay dos puntos de entrada principales para alternar pruebas en pantalla.

### 1. Ver menú

Uso:
- Ver > Gestión de color > Activar vista previa de prueba en pantalla

Esto activa o desactiva la simulación de vista previa para la pantalla actual.

### 2. Alternar barra de estado

Lumi también expone la prueba en pantalla directamente en la barra de estado inferior.

- **Clic izquierdo** (alternar): habilitar o deshabilitar colores de prueba
- **Clic derecho**: abre la ventana emergente de prueba en pantalla donde puedes modificar:
  - perfil actual
  - selector de perfil
  - intención de representación
  - compensación de punto negro
  - marcado fuera de gama

{{< callout type="warning" >}}
**Nota importante sobre la precisión**
La vista previa de prueba en pantalla solo está habilitada para imágenes de **16 y 32 bits**.
Para imágenes de **8 bits**, la opción está desactivada y Lumi le pedirá que primero convierta la precisión a una profundidad mayor antes de obtener una vista previa de los colores con precisión.
{{< /callout >}}

## Preferencias y valores predeterminados

Los incumplimientos globales se encuentran en:
- Editar > Preferencias > Gestión de colorSecciones relevantes:
- **Perfil de monitor manual**
- **Perfil RGB preferido**
- **Perfil de escala de grises preferido**
- **Prueba en pantalla**

### Valores predeterminados actuales de Lumi

#### Espacios de trabajo

ICC de espacio de trabajo agrupados que se ofrecen actualmente desde la carpeta de datos compartidos:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Para el trabajo sRGB estándar, Lumi también proporciona un **perfil de trabajo sRGB incorporado internamente**.

#### Valores predeterminados de prueba en pantalla

Perfiles de prueba en pantalla incluidos actualmente instalados:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Cuando esté disponible, `CoatedFOGRA39.icc` se utiliza como perfil de referencia CMYK/prueba en pantalla incluido predeterminado.

## Flujos de trabajo prácticos

### Para pintura y trabajos normales de mampara.

- Mantenga la imagen en sRGB integrado u otro espacio de trabajo RGB válido.
- Permita que Lumi use el perfil de monitor del sistema si está disponible.

### Para vista previa de impresión

- Mantenga la imagen en su espacio de trabajo RGB estándar.
- Elija un perfil de prueba en pantalla que coincida con la condición de impresión de destino (por ejemplo, FOGRA39).
- Habilitar vista previa de prueba en pantalla.
- Opcionalmente, habilite las advertencias de gama para ver los intentos de renderizado recortados.