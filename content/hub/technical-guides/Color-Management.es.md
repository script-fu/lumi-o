---
title: "Gestión del color"
type: docs
weight: 15
url: "hub/technical-guides/Color-Management"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: e124f17c1f65c73f4e135c25dd7962eb44f1d0676147a7e4bcbf6dc8ecf51e69
---

Lumi-o está configurado para funcionar desde el primer momento. Siempre que trabajes en una imagen con **16 bits o más de precisión**, el software ya está preparado para usar la prueba en pantalla (CMYK) incluida por defecto y los perfiles sRGB integrados; todo debería funcionar sin configuración adicional.

Para quienes necesiten un control más profundo, esta guía explica el modelo central de gestión del color de Lumi, la diferencia entre un perfil de imagen y un perfil de prueba en pantalla, dónde se encuentran los controles y cómo se incluyen los perfiles predeterminados con la aplicación.

## Resumen rápido

Lumi utiliza tres roles de perfil distintos:

1. **Perfil de espacio de trabajo de la imagen**
   - Define qué significan los valores RGB o en escala de grises de la imagen.
   - Se utiliza en las operaciones de asignación y conversión.
   - Ejemplos habituales: sRGB integrado, Adobe RGB.

2. **Perfil de pantalla**
   - Describe tu monitor.
   - Se utiliza para mostrar la imagen correctamente en pantalla.
   - Suele proporcionarlo el sistema o elegirse en Preferencias.

3. **Perfil de prueba en pantalla**
   - Simula otro dispositivo de salida o condición de impresión.
   - **No** redefine los valores de píxel de la imagen.
   - Ejemplos habituales: perfiles CMYK de imprenta como `CoatedFOGRA39`.

## Perfil de imagen frente a perfil de prueba en pantalla

### Perfil de imagen

Utilízalo cuando quieras indicarle a Lumi en qué espacio de color se encuentra realmente la imagen.

Dos operaciones habituales:

- **Asignar perfil**
  - Cambia la etiqueta de perfil asociada a la imagen.
  - **No** convierte los valores de píxel.
  - Úsalo solo cuando los valores de píxel ya correspondan a ese espacio de color.

- **Convertir a perfil**
  - Convierte los valores de píxel del perfil de imagen actual a uno nuevo.
  - Úsalo cuando quieras que la imagen pase realmente a otro espacio de trabajo.

**Ubicaciones en el menú:**
- Imagen > Gestión del color > Asignar perfil de color...
- Imagen > Gestión del color > Convertir a perfil de color...

### Perfil de prueba en pantalla

Utilízalo cuando quieras previsualizar cómo se reproduciría la imagen en un dispositivo de destino o en una condición de impresión concreta.

La prueba en pantalla:
- deja intacto el espacio de trabajo de la imagen
- modifica la canalización de previsualización
- puede marcar los colores fuera de gama
- está pensada para la previsualización, no para reasignar los datos de la imagen

**Ubicaciones en el menú:**
- Imagen > Gestión del color > Ajustes de prueba en pantalla > Elegir perfil de prueba en pantalla...
- Imagen > Gestión del color > Ajustes de prueba en pantalla > Intención de renderizado
- Imagen > Gestión del color > Ajustes de prueba en pantalla > Compensación del punto negro
- Ver > Gestión del color > Activar previsualización de prueba en pantalla
- Ver > Gestión del color > Marcar colores fuera de gama

## Cómo ver la previsualización de prueba en pantalla

Hay dos formas principales de activar o desactivar la prueba en pantalla.

### 1. Menú Ver

Utiliza:
- Ver > Gestión del color > Activar previsualización de prueba en pantalla

Esto activa o desactiva la simulación de previsualización en la pantalla actual.

### 2. Conmutador de la barra de estado

Lumi también ofrece acceso directo a la prueba en pantalla en la barra de estado inferior.

- **Clic izquierdo** (conmutador): activar o desactivar los colores de prueba
- **Clic derecho**: abre el panel emergente de prueba en pantalla, donde puedes ajustar:
  - perfil actual
  - selector de perfil
  - intención de renderizado
  - compensación del punto negro
  - marcado de colores fuera de gama

{{< callout type="warning" >}}
**Nota importante sobre la precisión**
La previsualización de prueba en pantalla solo está disponible para imágenes de **16 y 32 bits**.
En imágenes de **8 bits**, el conmutador está desactivado y Lumi te pedirá que conviertas primero la precisión a una profundidad mayor antes de previsualizar los colores con exactitud.
{{< /callout >}}

## Preferencias y valores predeterminados

Los valores predeterminados globales se encuentran en:
- Editar > Preferencias > Gestión del color

Secciones relevantes:
- **Perfil de monitor manual**
- **Perfil RGB preferido**
- **Perfil de escala de grises preferido**
- **Prueba en pantalla**

### Valores predeterminados actuales de Lumi

#### Espacios de trabajo

Perfiles ICC de espacio de trabajo incluidos actualmente en la carpeta de datos compartida:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

Para el trabajo sRGB estándar, Lumi también proporciona internamente un **perfil de trabajo sRGB integrado**.

#### Valores predeterminados de prueba en pantalla

Perfiles de prueba en pantalla incluidos actualmente instalados:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

Cuando está disponible, `CoatedFOGRA39.icc` se utiliza como perfil de referencia CMYK/prueba en pantalla incluido por defecto.

## Flujos de trabajo prácticos

### Para pintura y trabajo habitual en pantalla

- Mantén la imagen en sRGB integrado u otro espacio de trabajo RGB válido.
- Deja que Lumi utilice el perfil de monitor del sistema si está disponible.

### Para previsualización de impresión

- Mantén la imagen en su espacio de trabajo RGB habitual.
- Elige un perfil de prueba en pantalla que coincida con la condición de impresión de destino (p. ej., FOGRA39).
- Activa la previsualización de prueba en pantalla.
- Opcionalmente, activa las advertencias de gama para ver los colores recortados según la intención de renderizado.
