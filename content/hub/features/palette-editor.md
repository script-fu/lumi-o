---
title: "Palette Editor"
type: docs
url: "hub/features/palette-editor"
---

The Palette Editor is where you build and manage a Lumi palette. It holds your pigment set, stores the mixes you save from the Palette Mixer, records the colors you actually used while painting, and lets you configure the value structure and gradients for the palette.

## Selecting a Palette

A palette is more than a collection of pigments: it is a stylistic commitment. Many artists work with a small, fixed set of pigments they know intimately: the way they mix, the neutrals they produce, the temperature shifts between them. That familiarity becomes part of their visual voice. A painter might keep a warm, low-chroma palette for figure work and a separate high-key palette for landscapes, or they might do all their work within a single four-pigment set as a deliberate constraint that unifies a body of work.

Lumi supports this way of working. Each palette has its own pigments, mixes, value structure, and gradients. Switching palettes changes the entire color system: the Map, the Mixer, and the available mixes all update to reflect the new set.

A dropdown at the top of the Palette Editor selects the active palette. Lumi ships with three palettes in the **Standard** group:

| Palette | Character |
| :--- | :--- |
| **Default** | A versatile warm-leaning palette covering the full hue wheel. Good starting point for most subjects. |
| **Master** | A large full-spectrum palette for painters who want maximum hue coverage and explicit control over graying axes. |
| **Zorn** | A four-pigment limited palette based on the approach of Anders Zorn. Covers a surprisingly wide range of warm flesh tones and low-chroma neutrals from a minimal pigment set. |

Palettes can also be created, imported, or duplicated from the Palettes Tab.

## Palette Pigments

The **Palette Pigments** section at the top of the palette view lists your primary entries: the base pigments the rest of the palette is built from. These are the inputs to the spectral mixing system. Secondaries and tertiaries are generated from them automatically and are used to populate the Palette Map

## Saved Mixes

The **Saved Mixes** section holds colors you have explicitly kept from the Palette Mixer using **Add to Palette**. These are your derived colors: the results of spectral mixing, tone, and chroma adjustments saved for reuse.

Saved Mixes are sub-divided into five value bands:

| Band | Default Lightness Range |
| :--- | :--- |
| High Key | 80 – 100% |
| Upper Mid | 60 – 80% |
| Middle | 40 – 60% |
| Lower Mid | 20 – 40% |
| Deep | 0 – 20% |

Lumi places each saved mix into the appropriate band automatically based on its perceptual lightness (CIE L\*). This organises your mixes by value rather than searching through a flat list, and typically matches the way an artist thinks about colour.

Saved mixes can be renamed via the **Rename Custom** button or the context menu.

## Used Mixes

The **Used Mixes** section is a paint-triggered history. Every time a color from the palette is applied to the canvas, it is recorded here. Used Mixes are ordered from most to least recent.

This section is useful for retrieving a color you painted with but did not explicitly save. To keep a Used Mix permanently, select it and click **Promote** and it moves into Saved Mixes in the appropriate value band.

Used Mixes are stored per-palette and persist between sessions.

## Value Bands

Value Bands define where the boundaries between the five lightness zones sit. By default they divide lightness evenly across the 0–100% range, but you can adjust them to match the tonal structure of your subject. It's useful for painters to define and manage value bands _and_ the gaps between them.

### The Value Band Slider

The **Value Bands expander** in the Palette Editor contains a slider with five draggable dividers. Drag any divider to shift the boundary between adjacent bands. The label above the slider shows the name and exact percentage range of the active band.

**Buttons:**

| Button | Effect |
| :--- | :--- |
| **Cancel** | Reverts the slider to the last applied state |
| **Copy** | Copies the current band configuration to the clipboard |
| **Paste** | Pastes a copied band configuration from another palette |
| **Defaults** | Restores the factory equal-division defaults |
| **Apply** | Commits the changes and regenerates the palette |

**Apply** is required to make the changes permanent. It triggers a full palette regeneration and will remove any Saved Mixes whose lightness no longer falls within any band. Lumi shows a confirmation dialog listing how many mixes would be removed before proceeding.

### Value Bands and the Palette Map

The Palette Map displays the palette as a hue wheel with 36 hue sectors (10° each) and 15 lightness cells arranged as concentric rings. Each band corresponds to three rings: the five bands × 3 rings = 15 total cells.

Adjusting the value bands shifts which lightness values land in each ring tier. A band compressed toward the dark end makes its three rings span a narrower tonal range; a wide band gives its three rings more tonal spread. This is how the same Palette Map structure adapts to palettes tuned for different tonal priorities.

## Palette Gradients

Each palette can store one or more **Gradients**: smooth progressions derived from palette entries that can be applied to the canvas as gradient fills or used as reference strips.

Gradients are managed in the **Gradients expander**. The combo at the top lists the gradients in the current palette. **Add** creates a new gradient. **Remove** deletes the selected one. **Rename** renames it.

### Gradient Editor

The **Gradient Editor expander** configures the selected gradient. Each gradient has three endpoints (**A**, **B**, and **C**) displayed as color swatches. Click a swatch to make it the active endpoint for editing.

Each endpoint can be set by clicking **Pick** and then clicking a palette entry in the Palette Map or the palette view. The endpoint is linked to that palette entry by UID; if the entry changes, the gradient updates.

**Per-endpoint controls:**

| Control | Effect |
| :--- | :--- |
| **Strength** | How strongly the endpoint color contributes relative to its neighbors |
| **Opacity** | Alpha of the endpoint color in the gradient |
| **Curve** | Gamma adjustment for the color falloff from this endpoint |

**Distribution sliders** (S1, S2, S3) set where the three midpoints between the endpoints fall along the gradient strip. Resetting them returns the midpoints to equal spacing.

The gradient preview strip at the top of the Gradient Editor block shows the result of the current endpoint and distribution settings.

## Palette Dockable

The **Palette** dockable (**Panels > Palette**) is a simpler read-focused panel for browsing and selecting colors from any palette. It shows the same three-section view (Palette Pigments, Saved Mixes, Used Mixes) without the Value Bands and Gradients expanders.

A palette selector dropdown at the top lets you switch between all available palettes. Click any entry to set it as the foreground color. Double-click to open the color name editor. For writable palettes, Edit Color, New Color from FG, and Delete Color actions are available in the button bar.

The Palette dockable is intended for quick color access during painting when the full Palette Editor would take up too much space.

## Palettes Tab

The **Palettes Tab** (available as a dockable tab) shows the active palette in compact mode. It excludes the pigments to focus on saved mixes
