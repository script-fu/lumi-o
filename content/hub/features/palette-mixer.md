---
title: "Palette Mixer"
type: docs
url: "hub/features/palette-mixer"
---

The Palette Mixer derives new colors from pairs of palette entries using a fixed three-stage pipeline. Because mixing happens in the spectral domain rather than RGB, the results behave like physical pigments: blue and yellow produce green, saturated colors shift toward neutral as they mix.

## The Pipeline

Every color produced by the Mixer passes through three stages in a fixed order:

1. **Blend**: Spectral WGM between Parent A (CCW) and Parent B (CW).
2. **Chroma**: Blend toward the palette's neutral spectrum, reducing saturation.
3. **Tone**: Blend toward mixing white (tint) or mixing black (shade).

Tone is always applied last. This makes lightness dominant: a tone adjustment lands at exactly the intended lightness level without being diluted by the chroma adjustment preceding it.

## Selecting Parents

Parent A and Parent B are the two entries the blend slider mixes between. They are loaded from the Palette Map:

- Hold **Shift** on the Palette Map and **left-click** to set Parent A (CCW).
- Hold **Shift** and **right-click** to set Parent B (CW).

Only **Class A** entries (Primaries and Custom mixes with intact provenance) are accepted as parents. Tertiaries and entries with lost ancestry are excluded.

The Mixer's Parent A and Parent B positions are shown on the Map as **diamond ring** highlights so you can always see which entries are loaded.

## The Sliders

| Slider | Effect |
| :--- | :--- |
| **Blend** | Moves between Parent A (CCW end) and Parent B (CW end). At 0.0 the result matches Parent A; at 1.0 it matches Parent B. |
| **Chroma** | Desaturates the blend toward the palette's neutral. Higher values produce more muted, earthy results. |
| **Tone** | Shifts lightness toward mixing white (tint direction) or mixing black (shade direction). |

## Value Controls

**Value Lock** freezes the perceptual lightness (CIE L\*) at its current level while the other sliders move. Use this to explore chroma or hue variation without changing the value of a mix.

**Band Clamp** confines the result to stay within the boundaries of its current value band (e.g., within Lower Mid). The tone slider is still draggable but the output lightness is clamped.

The Tone slider also reflects any Value Gaps configured in the Palette Editor. Lightness ranges that fall inside a gap are shown as semi-transparent grey bands on the slider trough. The slider handle hops automatically over these gaps: dragging through a grey region jumps to the nearest valid band boundary on the other side.

## Mixing Endpoints (White, Black, Neutral)

The tone and chroma stages require reference endpoints: a mixing white, a mixing black, and a neutral. Lumi discovers these automatically by searching the active palette for the best candidates:

- **Mixing White**: the highest-chroma Primary closest to pure white.
- **Mixing Black**: the lowest-lightness Primary.
- **Neutral**: the Primary closest to achromatic (lowest chroma).

These can be manually overridden by right-clicking an entry in the Palette Editor.

## Saving a Mix

Click **Add to Palette** to save the current mixer result as a **Saved Mix** (Custom entry). Before saving, the system applies **Best-Match Relocation**: it searches the palette for the optimal recipe that produces the same final color with the best spatial fit on the Palette Map. If a closer recipe is found, the mixer sliders will jump to reflect it, confirming the system found a better origin and the saved entry's position will align with its visual dot on the Map.

Saved mixes store their full recipe (Parent A/B UIDs, blend factor, tone, chroma) so they can be reproduced exactly.

## Recipe Recovery

Single-clicking a Custom entry in the Palette Editor restores that entry's recipe into the Mixer:

- Parent A and Parent B are reloaded.
- The blend, tone, and chroma sliders return to their original positions.
- Any Value Lock or Band Clamp that was active during creation is re-enabled.

This makes it straightforward to return to a color and adjust it further, or to use it as a starting point for a new mix.

