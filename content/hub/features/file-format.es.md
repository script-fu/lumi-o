---
title: "Formato de archivo (.lum)"
type: docs
---
Lumi utiliza un formato de archivo abierto basado en directorio (`.lum`) diseñado para brindar rendimiento, confiabilidad y accesibilidad a largo plazo.

## Descripción general

Un archivo `.lum` es en realidad un directorio que contiene:
- **Metadatos** (capas, modos de fusión, propiedades).
- **Búfers de capa** (datos de píxeles individuales para cada capa).
- **Máscaras** (datos en escala de grises para máscaras de capa).
- **Historial de recuperación** (instantáneas incrementales).

Esta estructura permite guardar rápidamente, cargar archivos grandes de forma diferida y recuperar el trabajo incluso después de una falla.

## Propiedades clave

### Abierto y legible

El formato `.lum` utiliza metadatos XML y buffers binarios comprimidos. Puede inspeccionar la estructura de capas, las propiedades y los modos de fusión en texto sin formato. Sin códec propietario; Los datos de píxeles se almacenan en el formato de búfer GEGL estándar.

### Ahorro incremental

El guardado incremental debe habilitarse por proyecto en el cuadro de diálogo **Guardar como** (una casilla de verificación **Guardado incremental** y un botón giratorio **Guardados máximos**). Una vez habilitado, Ctrl+S escribe solo las capas modificadas en lugar de reescribir todo el proyecto, lo que reduce drásticamente el tiempo de ahorro. La configuración se almacena con el proyecto y persiste en todas las sesiones.

### Carga diferida

Los grandes proyectos se abren rápidamente. Los píxeles de capa se cargan desde el disco solo cuando:
- La capa se hace visible.
- Pintas sobre la capa.
- La capa se exporta o se compone.

Los proyectos muy grandes (más de 500 capas, varios gigabytes de datos) siguen respondiendo. La carga diferida está habilitada de forma predeterminada y se puede alternar en **Editar → Preferencias → Rendimiento → Recursos de memoria**.

### Autoguardado

Lumi guarda automáticamente los cambios en una **ubicación de caché separada** (`~/.cache/lumi/autosave/`) a intervalos regulares. Los guardados automáticos son independientes del archivo de trabajo y no lo modifican. El intervalo y la ubicación de la caché se pueden configurar en **Editar → Preferencias → Rendimiento**.

## Acceso

### Guardar y guardar como

- **Archivo** → **Guardar** (Ctrl+S): guardar en el directorio `.lum` actual.
- **Archivo** → **Guardar como** (Shift+Ctrl+S): guardar en un nuevo archivo `.lum`. El cuadro de diálogo Guardar como incluye opciones para el tipo de compresión y una opción para **Guardar incremental** (con un límite de **Guardado máximo**) para habilitar o deshabilitar el guardado incremental para este proyecto.

Los cambios no guardados se indican con un asterisco (*) en el título de la ventana.

### Exportar

- **Archivo** → **Exportar como** (Shift+Ctrl+E): Exportar a PNG, JPEG, TIFF u otros formatos.
- **Archivo** → **Sobrescribir** (Ctrl+E): reexportar al último archivo exportado.

La exportación aplana las capas visibles y convierte del espacio de color espectral al sRGB.

### Importar

- **Archivo** → **Abrir** (Ctrl+O): carga un proyecto `.lum`.
- **Archivo** → **Abrir como capas** (Shift+Ctrl+O): Importe archivos `.lum`, XCF o PSD como nuevas capas.
- **Archivo** → **Archivos recientes**: acceso rápido a proyectos abiertos recientemente.

Los archivos PSD y XCF se convierten al formato nativo de Lumi al importarlos.

## Compatibilidad de importación y exportación

### Formatos de importación admitidos
- **.lum**: formato nativo Lumi.
- **.xcf**: formato nativo de GIMP (se conservan las capas y propiedades básicas).
- **.psd**: formato Photoshop (se conservan las capas y los modos de fusión).
- **PNG, JPEG, TIFF, etc.**: Importación de imágenes aplanadas.

### Formatos de exportación admitidos
- **PNG**: Sin pérdidas, con transparencia alfa.
- **JPEG**: con pérdida, aplanado.
- **TIFF**: Sin pérdidas o comprimido con LZW.
- **XCF**: formato de compatibilidad con GIMP. Sólo exportación; capas y propiedades básicas preservadas.

## Recuperación del proyectoLumi mantiene guardados automáticos en segundo plano y puntos de control incrementales manuales, ambos accesibles desde **Archivo** → **Recuperar imagen**. Consulte la página [File Recovery](../recovery) para obtener todos los detalles.

## Organización

Un archivo `.lum` es un directorio con una estructura fija:

```
my-painting.lum/
  ├── metadata.xml                       (image structure, layer tree, properties)
  ├── thumbnail-YYYYMMDD-HHMMSS.png      (last-saved thumbnail)
  ├── drawables/
  │   ├── layer-<name>.geglbuf           (pixel data per layer)
  │   └── mask-<name>.geglbuf            (mask data, shares layer name)
  ├── icc/                               (embedded colour profiles)
  ├── parasites/                         (per-image metadata)
  ├── paths/                             (vector paths as SVG)
  ├── configs/                           (non-destructive filter configurations)
  └── recovery/
      └── primary-01.lum/                (incremental save checkpoint)
          ├── metadata.xml
          ├── drawables/                 (only modified buffers)
          ├── delta-0001.lum/            (Ctrl+S checkpoint)
          └── delta-0002.lum/
```

Los buffers de capa llevan el nombre de la capa (`layer-Background.geglbuf`), no están numerados secuencialmente. Los espacios en los nombres de las capas se almacenan como guiones bajos; las capas de grupo obtienen un sufijo `-GROUP`. Las máscaras comparten el nombre de la capa (`mask-Background.geglbuf`).

Cada `recovery/primary-NN.lum/` es una partida guardada completa. Las pulsaciones posteriores de Ctrl+S agregan subdirectorios `delta-NNNN.lum/` que contienen solo los búferes modificados desde la última línea de base, lo que mantiene los puntos de control guardados rápidamente independientemente del tamaño del proyecto.

Los guardados automáticos siguen la misma estructura pero se almacenan por separado en `~/.cache/lumi/autosave/`, dejando el archivo de trabajo intacto.
- **Proyectos muy grandes**: un proyecto con más de 1000 capas y terabytes de datos será el que más se beneficiará de la carga diferida; sin embargo, la exportación final a formato de imagen plana puede llevar algún tiempo.
- **Unidades de red**: se admite el almacenamiento en directorios montados en red, pero es más lento que el almacenamiento local debido a la latencia de E/S.