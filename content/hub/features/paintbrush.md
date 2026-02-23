---
title: "Paintbrush Tool"
type: docs
url: "hub/features/paintbrush"
---

The Paintbrush is the primary painting tool, designed for responsive, intelligent brushwork with full control over pressure, velocity, tilt, and spacing dynamics.

## Overview

The Paintbrush tool supports raster, procedurally generated, and animated brush types. Strokes can be stabilized, smoothed, and post-processed. Brush dynamics respond to stylus input, giving precise control over opacity, size, color, angle, and other properties during a stroke.

## Brush Types

### Raster Brushes (.raster)
Bitmap brush images that supports alpha transparency.

### Generated Brushes (.param)
Procedurally rendered shapes (Circle, Square, Diamond, Triangle) with adjustable parameters: hardness, aspect ratio, angle, roundness, and corner radius. Generated brushes are lightweight and scalable.

### Animated Brushes (.anim)
Sequential frame sequences that advance during strokes. Frames can be cycled incrementally (frame advances per dab), randomly selected per dab, or indexed by dynamics (pressure, velocity, tilt, angle).

## Painting Cursor
The cursor adapts to the current tool state to provide clear, contextual feedback:

- **Brush outline**: The cursor tracks the exact brush shape and size, giving a live preview of where paint will land.
- **Erase mode**: When erasing is active, the outline switches to a dashed circle to visually distinguish erase strokes from paint strokes.
- **Simple Brush Boundary**: For complex or very large brushes where rendering the accurate outline is costly, enable **Simple Brush Boundary** (in Additional Options) to use a plain circle instead.

## Tool Options

### Top-Level Controls
Present at all times, outside any expander:
- **Mode**: Paint blending mode (Normal, Multiply, Screen, etc.)
- **Opacity**: Overall stroke opacity (0–100).

### Brush Options
In the **Brush Options** expander (expanded by default):
- **Size**: Brush diameter in pixels.
- **Ratio**: Squash or stretch the brush shape (-1.0–1.0). 0 = unmodified; negative values rotate the squash 90°.
- **Angle**: Rotates the brush stamp (-180–180°). Independent of stroke direction dynamics.
- **Spacing**: Distance between painted dabs as a percentage of brush size. Lower = smoother strokes; higher = scattered pattern.
- **Hardness**: Soft fade (0.0) to sharp edge (1.0).
- **Force**: Brush application force (0.0–1.0). Hidden for the Pencil tool.
- **Jitter**: Randomly offsets each dab position by up to this many pixels (0–1024).
- **Eraser**: Size multiplier applied when this brush is used as an eraser (0.1–10.0). Not shown on the Eraser tool itself.

### Stroke Effects
In the **Stroke Effects** expander:
- **Post Process**: Applies stabilization, velocity compression, and replay correction after the stroke is complete, improving consistency without latency.
  - **Turn Threshold**: Angle threshold (0–180°) for direction correction at sharp corners. 0 = skip direction fix.
  - **Preview Velocity**: Suppresses the post-process preview when stroke velocity exceeds this value (0 = always preview).
- **Build-Up**: When on, each dab accumulates opacity rather than being composited as a single stroke.

#### Calligraphic
When active, dab stamping is replaced by a continuous geometric corridor:
- **Width** and **Height**: Dimensions of the calligraphic corridor.
- **Angle**: Nib orientation (degrees).
- **Dynamic Opacity**: Modulates opacity within the stroke based on velocity and direction changes. Works best on fine, controlled strokes; results are less predictable on rapid scribbles. Experimental.
- **Velocity Growth** (0–100%): Maximum allowed size increase per sample as a percentage of the previous sample's size. Limits how quickly a velocity-driven size dynamic can grow, preventing sudden jumps when the stroke accelerates.
- **Velocity Shrink** (0–100%): Maximum allowed size decrease per sample. Limits how quickly the size can drop when the stroke decelerates.

#### Smoothing
Enables real-time input smoothing applied to the stroke path as you paint. Expands to reveal:
  - **Depth** (2–256): Number of previous input samples considered when computing the smoothed position. Higher values produce a longer, more committed lag.
  - **Position** (0–100): Intensity of smoothing applied to the brush position. Higher values round out sharp direction changes.
  - **Pressure** (0–100): Smoothing applied to the stylus pressure signal, reducing pressure spikes and jitter.
  - **Direction** (0–100): Smoothing applied to the stroke direction, stabilising angle-sensitive dynamics.

#### Dynamics
Assign stylus input or other live values to painting parameters:

- **Pressure** (stylus): Controls size, opacity, rate, hardness, color, and more based on stylus pressure.
- **Velocity**: Maps stroke speed to brush properties.
- **Tilt**: X and Y tilt angles of the stylus affect angle and other parameters.
- **Wheel**: Mouse wheel or stylus wheel input.
- **Direction**: Angle of stroke direction.
- **Fade**: Fade opacity or size over a fixed number of dabs.

Each dynamic input can be mapped to multiple properties independently. Open **Tool Options** → **Dynamics** to configure.

#### Fade and Colour
In the **Fade and Colour** expander (nested inside Stroke Effects; only visible when **Dynamics System** is enabled):

- **Relative Initial Angle**: The **Initial Angle** value is interpreted relative to the stroke direction rather than as an absolute canvas angle.
- **Fade Initial Angle**: Fades from the **Initial Angle** at stroke start toward the live dynamic angle over the course of the stroke. Enabling this forces **Relative Initial Angle** on.
- **Initial Angle** (-180–180°): The brush angle at the very start of a stroke, before dynamics take over.
- **Angle Blend Factor** (0.0–1.0): Controls how quickly the brush angle transitions from the initial angle to the dynamic angle. 0 = holds the initial angle; 1 = immediately uses the fully dynamic angle.
- **Direction Stabilization** (0–100 px): Delays direction-sensitive dynamics by requiring the pointer to travel this many pixels before updating the stroke direction. Only active when **Post Process** is off (Post Process provides its own stabilization). 0 = disabled (immediate direction, may jump at stroke start).
- **Fade Length**: Distance in canvas units over which the fade plays out.
- **Repeat**: How the fade is repeated once the fade length is exhausted (None, Loop, Sawtooth, Triangle).


### Brush Heads

Brush Heads places multiple independent brush heads on a circular **orbit ring** centred on the stroke path. Every head paints a full dab at its own position each time the stroke advances, producing multiple parallel or fanned strokes simultaneously.

The orbit radius is determined by the global brush size minus the head size: larger heads sit closer to the centre; smaller heads orbit further out. Heads space evenly around the ring. With two heads you get one on each side of the stroke, creating a symmetrical spread that behaves like a calligraphy nib. The **Follow Direction** slider rotates the whole ring to stay perpendicular to the stroke, so the nib tracks direction naturally as you paint. Adding more heads fans them progressively around the ring, up to a full spray circle at 16.

Controls appear in the **Brush Heads** expander in the tool options panel.

- **Count**: Number of simultaneous brush heads (1–16).
- **Size**: Rendered size of each head relative to the global brush size (0.1–1.0).
- **Bristle Stiffness**: How rigidly the orbit radius follows the dynamics-scaled brush size. 0 = orbit expands and contracts with pressure; 1 = orbit stays fixed to the base size.
- **Angle** (0–360°): Static orientation of the formation ring, used when **Follow Direction** is below 1.0.
- **Follow Direction** (0.0–1.0): How strongly the formation ring tracks the stroke travel direction. At 1.0 the ring is always perpendicular to the direction of travel; at 0.0 it locks to the static **Angle** value.
- **Pressure Variation**: Per-head size variation applied as an independent pressure bias through the dynamics curves.
- **Opacity Variation**: Per-head opacity variation, independent of size variation.
- **Character Seed** (0–255): Fixed seed for per-head character (size, fill-spacing position). The same seed reproduces the same formation every stroke. Desensitized when **Randomize Head Character** is on.

#### Scatter
Displaces heads along and around the stroke path each dab, creating smear and spray effects.

- **Fill Spacing** (0.0–1.0): Spreads heads across the gap between consecutive dab positions. Each head's stable character value determines its lean direction; at 1.0 heads fill the full spacing interval. Character is stable per seed.
- **Scatter Angle** (0–90°, default 10°): Fans each head outward from the stroke direction by a freshly randomised angle up to this value. Clamped to 90° so no head ever faces backwards.
- **Forward Scatter** (0–4000 px): Maximum random scatter ahead of the stroke direction. Re-rolled independently every dab.
- **Backward Scatter** (0–4000 px): Maximum random scatter behind the stroke. Heads still face forward; only the displacement direction reverses. Both Forward and Backward can be nonzero simultaneously.
- **Scatter Size Balance** (0.0–1.0): Minimum scatter weight for large heads. At 0 big heads land close to the stroke; at 1 all heads scatter equally regardless of size.
- **Scatter Size Threshold** (1–100 px): Heads smaller than this pixel radius scatter at full distance; larger heads are progressively pulled closer to the stroke.

#### Randomization
- **Randomize Head Character**: Re-draws per-head character values (size, scatter position) every stamp so the formation is fully chaotic along the stroke. Overrides **Character Seed**.
- **Randomize Animation Frames**: For animated brushes: each head advances its animation frame independently.

### Additional Options

In the **Additional Options** expander (collapsed by default):

- **Lock to View**: Keeps the brush appearance fixed relative to the canvas view: when you rotate the canvas, the brush rotates with it.
- **Simple Brush Boundary**: Uses a plain circle for the brush cursor outline instead of rendering the full brush shape. Useful for complex or large brushes where the accurate boundary is expensive to draw.
- **Uniform Jitter**: When on, dab offsets from the **Jitter** slider are drawn from a uniform distribution (every offset equally likely within the range). When off, the distribution is Gaussian (offsets cluster toward centre).
- **Restore Last Used Colors**: Restores the foreground and background colors from the previous session at startup, instead of defaulting to black and white.
- **Random Horizontal**: 50% chance to mirror each stamp left-to-right per dab.
- **Random Vertical**: 50% chance to flip each stamp upside-down per dab.
- **Random Rotation**: Randomly rotates each stamp by 0°, 90°, 180°, or 270° per dab.
- **Reset Animation**: For animated brushes: when on, the animation restarts from frame 0 at each new stroke; when off, it continues from where the previous stroke ended.

