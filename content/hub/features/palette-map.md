---
title: "Palette Map"
type: docs
url: "hub/features/palette-map"
---

The Palette Map answers a practical question for painters: given a set of pigments, what colors can actually be mixed from them? Starting from the palette's input pigments, it procedurally explores every combination (two-pigment blends, three-way mixes, tonal variations) and maps the results onto a color wheel. The output is a picture of the reachable color space for that specific set of pigments.

The Map is also a coordinate-based navigation tool. It organises every generated mix by hue and lightness into a circular grid, so the entire palette is readable at a glance and every color has a stable home address.

## Grid Structure

The Map is divided into a 36 × 15 grid:

- **36 hue sectors**: 10° steps around the wheel, centred on major hue names.
- **15 lightness cells**: 3 cells per value band × 5 bands (High Key, Upper Mid, Middle, Lower Mid, Deep), running from white at the outside to black at the centre.

Each cell is a small wedge on the wheel. An entry placed in a cell is said to have that cell as its **origin**: its logical home address on the map.

## Colors in Cells

When multiple colors compete for the same cell, only one **winner** is displayed prominently:

1. **Primary** entries always win their cell, regardless of other occupants.
2. If no Primary is present, the generated mix (Secondary or Tertiary) with the **highest chroma** wins.

Entries that don't win are runner-ups and remain accessible via click cycling (see below).

Custom entries (Saved Mixes) render as square dots; generated mixes and primaries render as round dots.

## Click Cycling

Clicking an occupied cell selects the winner as the foreground color. Clicking the same cell again cycles to the next occupant (runner-up generated mixes, then any Custom entries saved at that grid address). Each click advances one step through the stack.

**Left-click** routes to the foreground. When the color target is set to background (from the toolbox), clicks route to the background instead.

## Shift-Select: Loading Mixer Endpoints

Hold **Shift** to enter endpoint-loading mode:

- **Left-click** assigns the clicked entry as **Parent A (CCW)** in the Palette Mixer.
- **Right-click** assigns it as **Parent B (CW)**.

Only Class A entries (Primaries and Custom mixes with intact provenance) are selectable in this mode. Tertiaries are hidden and non-Class-A dots are dimmed. A brief overlay confirms the mode is active.

## Mixer Parent Highlights

When the Palette Mixer has active Parent A and Parent B endpoints, both are marked on the Map with **diamond rings** (a diamond shape with a black border). These highlights remain visible even when other display elements are toggled, so the active blend parents are always identifiable.

## Origin vs Visual Position

Each entry has two positions on the Map:

- **Origin (Source Cell)**: The logical grid address the entry belongs to, fixed for its lifetime.
- **Visual dot position**: Where the color actually renders based on its perceptual hue and lightness.

With **Best-Match Relocation**, when a mix is saved the system computes the optimal recipe for the final color and sets the origin to match the color's visual position. This keeps saved colors close to their visual location on the wheel and makes the map spatially coherent.

## Dragging Saved Mixes

Custom entries (Saved Mixes) can be repositioned by dragging:

1. Click and hold on a Custom entry (square dot) and drag past the 5-pixel threshold.
2. The cursor changes to indicate drag mode. Parent highlights update live as you move across the map to show the new blend parents at each candidate position.
3. The dragged dot snaps to the nearest valid sample position.
4. Release to commit. The entry adopts the destination cell's recipe: its parents, blend, tone, and chroma are updated to match, and its origin is updated to match the new visual position.

Drag moves are undoable via **Edit → Undo**.

## Double-Click: Toggling the Map Workspace

In the **Palette Editor**, double-clicking any palette entry toggles the Palette Map workspace view on and off. This is a quick way to switch between browsing saved colors and mixing on the Map without using a menu. Single-click behavior (restoring the entry's recipe into the Mixer) is unaffected.

## Canvas Overlay

The Palette Map can be summoned directly onto the image canvas as a full-screen overlay by clicking the **Foreground/Background swatch** in the toolbox. This gives a large mixing surface without dedicating a permanent panel to the Map.

## Central Color Swatch

A circular swatch sits at the centre of the donut hole and reflects the color of whichever cell the cursor is over:

- **Hover color**: when the cursor rests on a map entry, the swatch updates immediately to show that entry's color.
- **Selected color as fallback**: when no cell is hovered, the swatch shows the Palette Mixer's computed result for the currently selected entry. If the mixer hasn't resolved yet, it uses the entry's base display color so the spot never goes blank.
- A thin dark border outlines the swatch at all times.
- After the cursor dwells over the central swatch briefly, a white-and-black outer ring appears to signal the area is interactive.
- **Clicking the central swatch** closes the canvas overlay, returning to the normal image view (the same as clicking outside the outer ring).

## Alt Key: Canvas Comparison Mode

When the Palette Map canvas overlay is open, holding **Alt** temporarily reveals the image beneath:

- The entire palette map UI fades to invisible (its opacity drops to zero), uncovering the canvas.
- A 64-pixel circular swatch follows the cursor, filled with the Palette Mixer's current sampled color, so you stay aware of the active mix while inspecting the image.
- Releasing Alt restores the palette map at full opacity.

A hint label, *"Hold down the Alt key to see the Image"*, is shown inside the workspace view as a reminder.
