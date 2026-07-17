---
title: "Lumi-o"
type: docs
url: "hub/about/lumi-o"
weight: 1
---

Lumi is a fast, efficient, Linux-only application for raster image making, developed in the open. Its design decisions, architecture documents, and development history are public so users can understand how it evolves.

Lumi is biased toward digital painting and illustration while remaining capable of structured image editing and photographic correction. Corrective adjustments, lens style effects, tone based color grading, and non destructive filter layers support a wide range of editing workflows.

Lumi is built around the idea that digital painting software should behave like a dependable studio tool: predictable, transparent, and focused on the act of making images.

## Purpose

Lumi supports a structured, non-destructive method of image making, whether the source is painted, drawn, or photographic. It is a focused, opinionated alternative to both general image editors and dedicated painting software: free and open source, without subscriptions, lock-in, cloud dependence, or AI image generation.

Reliability and long-term access are core features. The open, directory-based file format remains readable without proprietary software, with XCF and PSD import and export supported.

## Artistic Foundation

Lumi is developed by an independent artist with experience in pixel art, traditional drawing and painting, game development, technical art, illustration, and 3D animation. That background shapes its approach to color, linework, layers, performance, data recovery, scripting, and user experience.

## The Philosophy

Lumi combines a pigment-based color system with a responsive, non-destructive, layer-based workflow. Its color system is a deliberate departure from conventional HSV sliders and arbitrary RGB pickers.

- **Pigment-centric color**: Real world pigment profiles (Colour Index codes) are mixed spectrally so palettes behave more like real paint.
- **Palette-driven workflow**: Saved, switchable palettes organize pigments, mixes, value bands, and gradients, keeping color decisions coherent across an individual painting or a project.
- **Tactile, focused tools**: Brushes integrate stylus pressure, tilt, and velocity for direct, nuanced control; controls support deliberate decisions without unnecessary complexity.
- **Non-destructive reliability**: Layers and editable filters scale to complex projects while remaining predictable. Autosave, fast saving, incremental saves, and recovery protect long painting sessions and large projects.
- **Live workspaces**: Named profiles preserve docks, tools, presets, palettes, and device bindings, then switch them atomically at runtime.
- **Scheme scripting**: Lumi extends the Script-Fu tradition with a Scheme-based plug-in language and additional utility functions for building plug-ins and automating workflows.

[Filters](/hub/features/filters/), including shaped bokeh lens blur, tilt shift, tonal grading, sharpening, and noise reduction, can remain editable alongside brush work.

## Boundaries

- **Focused, not exhaustive**: Lumi is not aimed at web design, desktop publishing, or every niche a broad editor such as GIMP tries to cover.
- **Linux-only**: Lumi is optimized specifically for Linux and does not support Windows or macOS.

## Acknowledgments

Lumi is built on the foundation of the GNU Image Manipulation Program (GIMP). Lumi acknowledges and is deeply grateful for the many years of work by the developers, artists, and contributors.

![Lumi logo placeholder](/images/lumi.png)
