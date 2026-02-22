---
title: "Layers & Non-Destructive Editing"
type: docs
url: "hub/features/layers"
---

Lumi's layer system enables complex, non-destructive workflows with full control over blending, masking, and composition.

## Overview

Layers are the foundation of structured illustration. Each layer is independent, with its own blending mode, opacity, and optional layer mask. Groups can nest layers hierarchically with their own blending and clipping properties.

## Access

**Panels** → **Layers**, or the default **Layers** panel on the right.

## Layer Types

### Paint Layers
Standard raster layers for painted content. Store pixel data as GEGL buffers with optional alpha transparency.

### Group Layers
Hierarchical containers for organizing related layers. Groups can have their own blending mode, opacity, and clipping masks. Group projections are composited on-demand.

### Layer Masks
Grayscale masks attached to any layer, controlling per-pixel opacity. Painting on a mask with white makes pixels opaque; black makes them transparent; gray provides partial opacity.

## Blending Modes

Every layer has a blending mode determining how it combines with layers below:

- **Normal**: Direct opacity blending.
- **Multiply**: Darken by multiplying color values.
- **Screen**: Lighten by inverting, multiplying, and inverting again.
- **Overlay**: Combination of Multiply and Screen.
- **Add**: Additive blending (sums color values).
- **Subtract**: Subtractive blending.
- **Color, Hue, Saturation, Lightness**: HSL component blending.

## Clipping & Masking

- **Composite Mode — Clip to Backdrop**: Setting a layer's composite mode to **Clip to Backdrop** restricts compositing to areas where the accumulated **Union** layers below have established opacity. The layer paints only where those layers have content — it cannot expand the alpha footprint. This is set per-layer in the Layer Attributes dialog (**Composite mode** dropdown). When a layer's effective composite mode is anything other than Union, the eye icon in the Layers panel is replaced with a composite icon to indicate the non-standard compositing behavior.

  **Example — shared alpha shape:** In a group, the bottom layer contains a filled circle on a transparent background, set to the default **Union** composite mode. Every layer above it in the same group is set to **Clip to Backdrop**. Those layers can only paint where the circle provides opacity — one shape, many layers. This is a common pattern for colouring, shading, and detailing within a defined silhouette without worrying about spill.
- **Layer Masks**: Apply a grayscale mask to control layer visibility pixel-by-pixel. Painting white on the mask reveals; black conceals; grey provides partial opacity.
- **Pure-Child Masks**: Masks are stored as children within the drawable stack, preventing data loss during transformations.

## Layer Picking (Alt Key)

Tapping **Alt** (left Alt) while hovering over the canvas selects the layer with visible pixels beneath the cursor — without switching tools or clicking.

### How It Works

- **Press Alt**: The cursor changes to a crosshair, indicating picking mode is active.
- **Release Alt**: Lumi picks the topmost non-transparent layer at the cursor position (opacity > 25%) and selects it. The layer is highlighted in the Layers panel and the status bar shows **"Layer picked: 'layer name'"**.
- A handle is drawn at the centre point of the picked layer on the canvas. The handle diminishes and fades as the cursor moves away.

### Cycling Through Layers

Each subsequent Alt tap at the same location picks the **next layer down** in the stack at that point. Lumi remembers the last picked layer and skips past it to the one beneath. Once the bottom of the stack is reached, the next tap cycles back to the topmost layer at that position. This makes it straightforward to reach nested layers in complex scenes by tapping Alt repeatedly.

### Cancellation Rules

The pick is cancelled (does not fire on Alt release) if either of the following occurs while Alt is held:

- A mouse button is pressed (left or right click).
- Any other key is pressed.

This ensures Alt drag gestures (such as brush size adjustment) and Alt-modified shortcuts work without accidentally changing the active layer.

### Limitations

- Layer picking does not activate during **Transform** tool operations — Alt has a different meaning there.
- Picking does not occur if a floating selection is present.
- Only left Alt triggers picking; right Alt is treated as a standard modifier.

## Operations

In the Layers panel:

- **Create Layer**: Right-click → **New Layer**, or use **Layer** menu.
- **Duplicate**: Right-click → **Duplicate**, or **Layer** → **Duplicate**.
- **Delete**: Right-click → **Delete**, or select and press **Delete**.
- **Reorder**: Drag layers up or down to change stacking order.
- **Rename**: Double-click the layer name.
- **Merge Down**: Right-click → **Merge Down** to combine with the layer below.
- **Flatten Image**: **Image** → **Flatten Image** to merge all visible layers.

## Layer Properties

- **Opacity**: 0–100%, controls overall layer transparency.
- **Blending Mode**: Dropdown menu to select how the layer combines with layers below.
- **Visible/Hidden**: Eye icon toggles layer visibility.

## Layer Locks

Lock icons are shown in the Layers panel header row. Each lock can be toggled independently. Right-clicking a lock icon sets it exclusively (locks only that type, unlocking all others on the same layer).

- **Lock Alpha**: Prevents painting on transparent areas. Brush strokes only affect pixels that already have opacity; fully transparent pixels are not modified. Useful for painting within existing shapes without spilling outside them.

- **Lock Mask**: Prevents editing the layer mask. The mask remains visible and active but cannot be painted on or modified while this lock is on.

- **Lock Color**: Locks painting to a specific color — the current foreground color at the moment the lock is applied. Subsequent strokes on this layer use that stored color regardless of the active foreground color. Unlocking discards the stored color.

- **Lock Content** (Lock Pixels): Prevents all pixel edits to the layer. The layer cannot be painted on, filled, transformed, or otherwise modified. Useful for protecting finished layers.

- **Lock Position**: Prevents the layer from being moved or transformed. The layer can still be edited; only positional changes (Move tool, Transform tool) are blocked.

- **Lock Visibility**: Prevents the eye icon from toggling the layer's visibility. Protects layers that should always remain visible (or hidden) during editing.

All locks are saved with the project and persist across sessions.

## Layer Effects (fx)

Non-destructive GEGL filters applied through the **Filters** menu are stored as committed effects on the layer rather than immediately modifying pixels. When a layer has at least one committed effect, an **fx** icon appears in the Layers panel next to that layer.

### Accessing the Effects Popup

Click the **fx** icon on a layer row in the Layers panel to open the **Layer Effects** popover for that layer.

The popover displays the filter stack for the layer — each committed effect listed by name with a visibility toggle beside it.

### Controls

- **Visibility eye toggle** (top of popup): Toggles all effects on or off simultaneously.
- **Per-filter visibility toggle**: Each filter row has its own eye icon to enable or disable that effect independently.
- **Edit**: Opens the settings dialog for the selected filter, allowing its parameters to be adjusted non-destructively.
- **Raise / Lower**: Moves the selected filter up or down in the stack, changing the order effects are applied.
- **Merge**: Commits all currently visible effects to the layer's pixels, making the changes permanent. The fx icon is removed if all effects are merged. Merge is not available on group layers.
- **Remove**: Deletes the selected filter entirely. The popover closes automatically if no effects remain.

Double-clicking a filter in the list also opens its edit dialog.

**Edit** and **Remove** are blocked if Lock Pixels is active on the layer. Filters cannot be reordered while one is actively being edited.

### Adding Effects

Apply a filter from **Filters** → (any category). If the active layer is targeted and the operation runs non-destructively, the result is stored as a layer effect rather than baked into the pixel data. The fx icon appears on the layer when at least one effect is present.

## Layer Attributes Dialog

Double-click a layer in the Layers panel to open the Layer Attributes dialog.

### Identity

- **Color tag**: Colour label for visual organization in the Layers panel.

### Composite Space and Mode

- **Composite space**: The color space used when compositing this layer with layers below. Options: Auto, Linear (RGB), Perceptual (RGB).
- **Composite mode**: Controls how the layer alpha interacts with the backdrop. Options include Union (affects all areas — the default for Normal mode), Clip to Backdrop (only affects areas with existing content — the default for most other blend modes), and Intersection.

### Size and Offsets

For an existing layer, **Sizes** shows the layer dimensions and mask dimensions (if a mask is attached) as read-only labels.

**Layer Offsets** — X and Y spinners controlling the layer's position on the canvas. Changes apply immediately rather than on dialog close.

If the layer has a mask, **Mask Offsets** — X and Y spinners for the mask's independent position — are shown below.

When creating a new layer, Width and Height fields and a **Fill with** dropdown (Foreground, Background, White, Transparent) replace the read-only size display.

### Layer Attributes (Persistent Parasites)

The lower section of the dialog contains a scrollable Name / Value table for persistent parasites — arbitrary key-value metadata attached to the layer. These values are stored with the project and are accessible from the Scheme scripting interface.

- Click any cell in the Name or Value column to edit it inline.
- **Add**: Appends a new empty row.
- **Delete**: Removes the selected row and its parasite from the layer.

If the layer has no persistent parasites, three empty starter rows are shown.

### Content State

A read-only info line at the bottom shows the current content state of the layer (and mask, if present): **Clear**, **Uniform**, or **Mixed**. A `*` prefix indicates the layer has unsaved changes since the last save.

## Performance

- **Fast Mode**: When painting on a single layer nested inside a group, Lumi temporarily switches ancestor groups to pass-through rendering for the duration of the stroke, skipping full group projection recomposition. This eliminates nested projection update lag during inking and painting. Full compositing resumes when the stroke ends, the active layer changes, or before a save.

  Fast mode is disabled when any of the following conditions apply to an ancestor group:
  - The group has visible non-destructive filters (filters need the projection buffer).
  - The group's blend mode is anything other than **Normal** or **Pass-through**.
  - The group has a direct child using **Clip to Backdrop** or **Intersection** composite mode (these require backdrop data from the projection buffer).

  Fast mode also does not activate for top-level layers, floating selections, or when multiple layers are targeted simultaneously.

  Structuring files to avoid these conditions in painting groups, using Normal blend modes on layers, ensures fast mode remains active throughout an inking or painting session.
- **Lazy Loading**: Large projects load quickly; layer data is loaded only when needed (e.g., when made visible or painted on).

## File Format

All layers, masks, and properties are stored in Lumi's open `.lum` format. The file is a directory containing individual layer buffers and metadata, ensuring compatibility and long-term accessibility.
