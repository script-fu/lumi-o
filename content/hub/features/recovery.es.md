---
title: "Recuperación de archivos"
type: docs
---
Lumi mantiene dos sistemas de recuperación independientes (guardado automático en segundo plano y puntos de control incrementales manuales), ambos accesibles desde un único cuadro de diálogo.

## Acceso

**Archivo** → **Recuperar imagen**

El cuadro de diálogo se abre precargado con estados de recuperación para el archivo actualmente abierto. Utilice el selector de archivos en la parte superior para cambiar a un archivo `.lum` diferente.

---

## Autoguardado

Lumi guarda una instantánea de fondo de su trabajo a intervalos regulares mientras edita. Los guardados automáticos se escriben en un **directorio de caché separado**, dejando intacto el archivo `.lum` en funcionamiento:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

La codificación de ruta utiliza `~` como separador para crear un directorio de caché único por archivo. Esto significa que los guardados automáticos están disponibles incluso si el archivo del proyecto se pierde o está dañado.

- **Frecuencia**: Configurable en **Editar** → **Preferencias** → **Rendimiento** → Intervalo de guardado automático.
- **Ubicación de almacenamiento**: También se configura en Preferencias → Rendimiento.
- **Propósito**: Recuperación de fallas. La pestaña Autoguardado en el cuadro de diálogo Recuperar imagen muestra los estados de autoguardado disponibles con marcas de tiempo.

Cuando abres un archivo que tiene datos de guardado automático más nuevos, Lumi te notifica en el momento de su apertura.

---

## Guardados incrementales

El ahorro incremental es un sistema de punto de control manual almacenado **dentro del archivo del proyecto** en `recovery/`. La estructura es:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

Se escribe una nueva línea base `primary-NN.lum/` después de **Archivo → Guardar**. Las pulsaciones posteriores de Ctrl+S crean subdirectorios `delta-NNNN.lum/` que contienen solo los búferes que cambiaron desde la última línea de base. Los deltas de guardado automático y los deltas de guardado manual utilizan contadores separados para que no interfieran con el historial de cada uno.

Los guardados incrementales están **deshabilitados de forma predeterminada** y deben habilitarse por proyecto:

1. **Archivo** → **Guardar como** (Mayús+Ctrl+S).
2. En el cuadro de diálogo Guardar como, marque **Guardado incremental** y, opcionalmente, establezca un límite de **Guardado máximo**.
3. La configuración se almacena con el proyecto y se aplica a todas las pulsaciones posteriores de Ctrl+S.

Cuando abres un archivo `.lum` que tiene guardados incrementales más nuevos que el guardado principal, Lumi muestra un mensaje **Guardado incremental detectado** que ofrece cargar el punto de control más reciente.

---

## Cuadro de diálogo Recuperar imagen

El cuadro de diálogo tiene tres pestañas y dos botones de acción.

### Pestaña de guardado automático

Enumera todos los estados de guardado automático disponibles para el archivo seleccionado con marcas de tiempo y miniaturas (donde estén disponibles). Seleccione un estado y haga clic en **Recuperar** para abrirlo.

Utilice esta pestaña para:
- Recuperarse después de un accidente.
- Volver a un estado anterior de la misma sesión.

### Pestaña incremental

Enumera todos los estados de los puntos de control almacenados dentro del archivo del proyecto. Cada entrada muestra la marca de tiempo del punto de control. Seleccione un punto de control y haga clic en **Recuperar** para abrirlo.

Utilice esta pestaña para:
- Volver a un punto anterior de una sesión sin haber guardado archivos separados.
- Navegar por el historial de versiones de un proyecto.

### Última pestaña

La pestaña predeterminada cuando se abre el cuadro de diálogo. Identifica automáticamente el estado de recuperación más reciente disponible en los guardados automáticos y en los puntos de control incrementales, y muestra su marca de tiempo. Haga clic en **Recuperar** para cargarlo inmediatamente sin explorar estados individuales.

---

## Botones

| Botón | Acción |
|--------|--------|
| **Recuperar** | Abre el estado de recuperación seleccionado como una nueva imagen. |
| **Cerrar** | Cierra el cuadro de diálogo sin recuperarlo. |
| **Limpiar los viejos estados…** | Abre un mensaje de limpieza (ver más abajo). |

---

## Limpiar los viejos estadosLa acumulación de estados de recuperación a lo largo del tiempo puede consumir una cantidad significativa de espacio en disco. El botón **Limpiar estados antiguos...** (abajo a la izquierda del cuadro de diálogo) abre un mensaje de limpieza para la pestaña activa (Guardado automático o Incremental).

El mensaje muestra:
- Cuántas partidas guardadas completas existen para el archivo.
- El espacio total en disco que ocupan.
- Un botón giratorio **Mantener más reciente** para seleccionar cuántos archivos guardados conservar.

Configurar **Mantener más reciente** en `0` elimina todos los estados de recuperación. El siguiente Ctrl+S después de una limpieza completa escribirá un nuevo guardado primario.

---

## Recuperación de inicio

Al iniciar, si Lumi detecta que el archivo abierto más recientemente tiene datos de guardado automático más recientes que el último guardado completo, presenta un mensaje de recuperación antes de cargarlo. Puede aceptar (cargar el guardado automático) o descartar (abrir el guardado principal como de costumbre).