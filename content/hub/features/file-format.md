---
title: "File Format (.lum)"
type: docs
url: "hub/features/file-format"
---

Lumi uses an open, directory-based file format (`.lum`) designed for performance, reliability, and long-term accessibility.

## Overview

A `.lum` file is actually a directory containing:
- **Metadata** (layers, blending modes, properties).
- **Layer buffers** (individual pixel data for each layer).
- **Masks** (grayscale data for layer masks).
- **Recovery history** (incremental snapshots).

This structure enables fast saving, lazy loading of large files, and recovery of work even after a crash.

## Key Properties

### Open & Readable

The `.lum` format uses XML metadata and compressed binary buffers. You can inspect layer structure, properties, and blending modes in plain text. No proprietary codec; pixel data is stored in standard GEGL buffer format.

### Incremental Saving

Incremental saving must be enabled per-project in the **Save As dialog** (an **Incremental Save** checkbox and a **Max Saves** spin button). Once enabled, Ctrl+S writes only the modified layers rather than rewriting the entire project, drastically reducing save time. The setting is stored with the project and persists across sessions.

### Lazy Loading

Large projects open fast. Layer pixels are loaded from disk only when:
- The layer is made visible.
- You paint on the layer.
- The layer is exported or composited.

Very large projects (500+ layers, multiple gigabytes of data) remain responsive. Lazy loading is enabled by default and can be toggled in **Edit → Preferences → Performance → Memory Resources**.

### Autosave

Lumi automatically saves changes to a **separate cache location** (`~/.cache/lumi/autosave/`) at regular intervals. Autosaves are independent of the working file and do not modify it. The interval and cache location are configurable in **Edit → Preferences → Performance**.

## Access

### Save & Save As

- **File** → **Save** (Ctrl+S): Save to the current `.lum` directory.
- **File** → **Save As** (Shift+Ctrl+S): Save to a new `.lum` file. The Save As dialog includes options for compression type and an **Incremental Save** toggle (with a **Max Saves** limit) to enable or disable incremental saving for this project.

Unsaved changes are indicated by an asterisk (*) in the window title.

### Export

- **File** → **Export As** (Shift+Ctrl+E): Export to PNG, JPEG, TIFF, or other formats.
- **File** → **Overwrite** (Ctrl+E): Re-export to the last exported file.

Exporting flattens visible layers and converts from spectral to sRGB color space.

### Import

- **File** → **Open** (Ctrl+O): Load a `.lum` project.
- **File** → **Open as Layers** (Shift+Ctrl+O): Import `.lum`, XCF, or PSD files as new layers.
- **File** → **Recent Files**: Quick access to recently opened projects.

PSD and XCF files are converted to Lumi's native format on import.

## Import & Export Compatibility

### Supported Import Formats
- **.lum**: Lumi native format.
- **.xcf**: GIMP native format (layers and basic properties preserved).
- **.psd**: Photoshop format (layers and blending modes preserved).
- **PNG, JPEG, TIFF, etc.**: Flattened image import.

### Supported Export Formats
- **PNG**: Lossless, with alpha transparency.
- **JPEG**: Lossy, flattened.
- **TIFF**: Lossless or LZW-compressed.
- **XCF**: GIMP compatibility format. Export-only; layers and basic properties preserved.

## Project Recovery

Lumi maintains automatic background saves and manual incremental checkpoints, both accessible from **File** → **Recover Image**. See the [File Recovery](../recovery) page for full details.

## Organization

A `.lum` file is a directory with a fixed structure:

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

Layer buffers are named after the layer (`layer-Background.geglbuf`), not numbered sequentially. Spaces in layer names are stored as underscores; group layers get a `-GROUP` suffix. Masks share the layer name (`mask-Background.geglbuf`).

Each `recovery/primary-NN.lum/` is a full baseline save. Subsequent Ctrl+S presses append `delta-NNNN.lum/` subdirectories containing only the modified buffers since the last baseline, keeping checkpoint saves fast regardless of project size.

Autosaves follow the same structure but are stored separately in `~/.cache/lumi/autosave/`, leaving the working file untouched.
- **Very Large Projects**: A project with 1000+ layers and terabytes of data will benefit most from lazy loading; however, final export to flat image format may take time.
- **Network Drives**: Saving to network-mounted directories is supported but slower than local storage due to I/O latency.
