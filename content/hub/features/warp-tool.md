---
title: "Warp Tool"
type: docs
url: "hub/features/warp-tool"
---

The Warp Tool pushes, pulls, and flows pixels freely across the canvas. In Lumi it goes further than most implementations: it can warp an entire layer group — no matter how many nested layers and masks it contains — as a single unified object, without flattening or losing any structure.

## Overview

Select a layer and drag across it to displace pixels in any direction. The warp is non-destructive while you are working: you can undo and redo individual strokes, change the brush size or behavior between strokes, and continue refining until you commit. Committing applies the accumulated displacement map destructively to the layer's pixel data.

When a **group layer** is selected, the tool operates on the group as a whole. You see and interact with a live preview of the entire composited group. On commit, the same warp is applied precisely and independently to every child layer and mask inside the group, preserving the complete layer structure.

## Group Warp

Warping a group is the primary capability that sets Lumi's warp tool apart.

### The Problem It Solves

In most paint programs, warping a multi-layer illustration requires either flattening the group first (destroying the layer structure) or warping each layer separately and trying to match them by eye (tedious and imprecise). Neither approach preserves the original structure for further non-destructive editing.

Lumi warps the entire group as one item and then distributes the exact same transformation to every layer inside it.

### How It Works

When you select a group and begin a warp stroke, Lumi builds a **floating preview layer** from the group's composited projection. If the group has a mask, the mask is baked into the preview so the preview accurately represents the final appearance. You paint your warp strokes directly on this preview — what you see is precisely what you get.

On commit, Lumi:

1. Applies the displacement to every basic layer inside the group (including deeply nested layers in sub-groups), expanding each layer's canvas just enough to capture the full warp area.
2. Applies the same displacement to every mask inside the group in the same pass.
3. Resumes the group's automatic bounds calculation so the group resizes to fit its newly warped children.
4. Crops each warped layer back to its actual painted content to keep file sizes compact.
5. Removes the preview layer and regenerates the group projection from the updated children.

All of this happens within a single undo step. After committing, the group looks exactly as it did in the preview, with every layer and mask intact.

### Masks

The **Warp Masks** option (enabled by default) causes masks on every layer and group inside the warp target to receive the identical displacement transformation. Layer masks move with their layers: a mask that was clipping a character's outline continues to clip that same outline after warping.

When **Warp Masks** is off, only layer content is displaced; masks retain their original positions.

## Tool Options

### Behavior

| Mode | Effect |
| :--- | :--- |
| **Move** | Pushes pixels in the direction of the stroke. The primary mode for most warping work. |
| **Grow** | Expands pixels outward from the brush centre. |
| **Shrink** | Pulls pixels inward toward the brush centre. |
| **Swirl Clockwise** | Rotates pixels clockwise around the brush centre. |
| **Swirl Counter-Clockwise** | Rotates pixels counter-clockwise around the brush centre. |
| **Erase** | Removes warp displacement, restoring pixels toward their original positions. |
| **Smooth** | Diffuses displacement, softening abrupt transitions between warped and unwarped areas. |

### Brush Controls

- **Size**: Diameter of the warp brush in pixels. Larger brushes displace broader areas with a softer falloff; smaller brushes give precise, localised control.
- **Hardness**: Falloff from centre to edge. High hardness produces a uniform displacement across the full brush area; low hardness concentrates the effect at the centre.
- **Strength**: How far pixels are displaced per stroke. Lower strength allows subtle, gradual shaping; higher strength produces dramatic, fast movement.

### Stroke Timing

- **Stroke During Motion** (Move mode only): Applies warp continuously as the mouse moves, rather than only on a timer pulse. Use for flowing, brush-like strokes where you want displacement to trail the cursor directly.
- **Stroke Periodically**: Applies warp at a fixed time interval while the mouse button is held. Use for Grow, Shrink, and Swirl modes where continuous circular application is the intent.
- **Rate**: The frequency of periodic stroke application.

### Quality

- **Interpolation**: The sampling method used when committing. Linear is fast and smooth for most work; Cubic and Nohalo give higher fidelity for fine detail.
- **High Quality Preview**: Uses the commit-quality sampler during the interactive preview. Slower, but the preview matches the committed result exactly.

### Group Options

- **Expand Warp Area** (group warp only): The number of pixels added as a transparent margin around the group preview on all sides. This gives displaced content room to move into. The default 256 px is sufficient for most work; reduce it for large images where memory matters, or increase it for very large displacement strokes.
- **Warp Masks**: Whether to apply the same warp to layer and group masks. On by default.

## Undo and Redo

Each stroke is a discrete undo step within the warp session. **Ctrl+Z** removes the last stroke and restores the displacement map to its prior state. **Ctrl+Y** (or **Ctrl+Shift+Z**) reapplies it. You can walk back through the entire stroke history before committing.

Pressing **Escape** or switching tools discards all uncommitted strokes and restores the layer(s) to their original state. No changes are written until you explicitly commit.

## Committing

Click the **Commit** button (or press **Enter**) to apply the accumulated warp destructively. For group warps, this triggers the full multi-layer application described above. The undo history for the committed warp is then a single entry in the image undo stack, reversible with the standard **Edit → Undo**.
