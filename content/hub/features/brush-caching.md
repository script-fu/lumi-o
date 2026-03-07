---
title: "Brush Caching"
type: docs
url: "hub/features/brush-caching"
---

Brush caching is designed to make your favourite brushes feel fast as early as possible. Instead of recomputing the same transformed brush stamp over and over, Lumi can keep a saved cache of the brush shapes you actually use and reload that cache automatically later.

## Overview

The feature is built around the idea that many expressive brushes still revisit the same practical combinations of size, angle, hardness, and aspect ratio during painting. When those combinations are reused, Lumi can serve the transformed brush stamp directly from cache instead of rebuilding it.

The result is:

- faster stroke startup after a cache has been saved
- smoother repeated use of favourite presets
- less wasted recomputation during long painting sessions
- automatic restore of saved caches when the preset is used again

## Intent

Brush caching is meant for brushes you return to often: core painting presets, favourite inking tools, textured dry brushes, and other brushes whose transformed stamps are expensive enough to notice.

The goal is not to pre-bake every theoretical brush state. The goal is to let real painting usage populate the most valuable states first, then save that populated cache so the brush is already warm the next time you use it.

## How it Works

Brush caching works together with brush quantization.

When quantization is enabled for a dynamics preset, transform-affecting outputs are snapped to discrete steps. That gives Lumi a finite set of reusable brush states. As you paint:

1. Lumi checks whether the transformed stamp already exists in cache.
2. If it does, the stamp is reused immediately.
3. If it does not, Lumi builds it once and stores it.
4. Over time, the cache fills with the brush states you actually use.

If you save that cache, Lumi can autoload it later so the brush starts closer to a warmed-up state instead of rebuilding everything from scratch.

## Typical Workflow

1. Choose a brush preset you use frequently.
2. Enable quantization for its dynamics.
3. Paint normally for a while so the cache fills organically.
4. Open the **Tool Preset Editor** and inspect the **Preset Cache** section.
5. Watch the live metrics:
   - **Hit Rate**
   - **Coverage**
   - **Memory**
6. Click **Save** when the cache looks worthwhile.
7. On later sessions, Lumi autoloads that saved cache when the preset becomes active.

This makes the preset feel fast sooner, especially for brushes with expensive transforms or large stamps.

## Where to Find It

### Dynamics Editor

Use the **Dynamics Editor** to control quantization:

- enable quantization
- choose the global step count
- optionally override step counts per output axis

Quantization is what makes the cache practical by reducing continuous variation into reusable bins.

### Tool Preset Editor

Use the **Tool Preset Editor** to manage the cache for the current preset:

- **Save** — persist the current in-memory cache to disk
- **Load** — restore a previously saved cache
- **Free Memory** — release the in-memory cache without deleting the saved copy
- **Remove** — delete the saved cache from disk

The **Preset Cache** expander also shows live hit rate, coverage, and memory use.

## What Gets Cached

Brush caching targets transformed brush stamps: the expensive rasterised results after size, angle, hardness, aspect ratio, and related transform inputs have been resolved.

It is most useful when:

- the brush has costly transform work
- the same preset is used across many sessions
- the brush revisits similar dynamic states repeatedly
- quick startup responsiveness matters

It is less useful for brushes whose transform state changes wildly and rarely repeats.

## Automatic Loading

Saved caches are intended to help from the start of a session, not only after you have already painted for a while.

When a saved cache exists for the active preset, Lumi can load it automatically so your favourite brush starts with many useful states already available. That reduces the cold-start period and gets the brush closer to peak responsiveness immediately.

## Memory Safety

Brush caching is designed to improve speed without taking over the machine.

Lumi tracks cache memory usage, exposes it in the UI, and applies runtime limits under memory pressure. If the system is short on available RAM, cache growth is constrained automatically.

## Best Use Cases

Brush caching is especially good for:

- favourite daily-driver brushes
- textured brushes used throughout a painting
- large expressive brushes with heavy transform cost
- brush presets shared across repeated illustration workflows
- presets you want to feel "ready" as soon as you select them

## In Short

Brush caching lets Lumi learn the brush states you actually use, save them, and bring them back automatically later. It is a practical speed feature for favourite presets: paint with the brush, let the cache fill, save it, and future sessions start faster.
