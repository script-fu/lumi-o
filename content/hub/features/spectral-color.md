---
title: "Spectral Color Mixing"
type: docs
url: "hub/features/spectral-color"
---

Lumi's palette system uses a spectral color model to simulate how real pigments mix. The goal is to make the experience of building and selecting colors from a digital palette behave like mixing physical paints. Once a color is applied to the canvas it is standard RGB.

## What Spectral Mixing Means

Traditional RGB mixing is additive: blending two RGB values averages them toward a midpoint. Pigment mixing is subtractive: each pigment absorbs certain wavelengths, and their combined effect is darker and often shifts in hue.

Lumi models this using a 10-band spectral reflectance representation for palette colors, rather than RGB.

This produces paint-like results: mixing blue and yellow produces green, not grey. Mixing two saturated colors produces a color that shifts toward neutral the way physical pigments do.

The spectral computation runs during palette construction — when generating secondary and tertiary palette entries and when the Palette Mixer blends two parent colors. The resulting color is converted to linear RGB for display and for painting.

## Pigment Profiles

Palette entries can be based on real pigment data using **Colour Index (CI) codes**. Each CI pigment family has a characteristic spectral bias that influences how it mixes.

| Pigment Role | Mixing Behaviour | Example |
| :--- | :--- | :--- |
| **Primary** | High chroma, clean secondaries | PY3 (Lemon Yellow), PR122 (Magenta) |
| **Body** | Opaque, strong mass tone, shifts to olive in green mixes | PY35 (Cadmium Yellow), PR108 (Cadmium Red) |
| **Neutralizer** | Rapidly desaturates and mutes | PBk11 (Mars Black), PBr7 (Sienna) |
| **Chroma Anchor** | High tinting strength, dominates mixtures | PB29 (Ultramarine Blue), PG7 (Phthalo Green) |

Adding primaries with CI codes to a palette gives the mixing engine accurate spectral bias for those colors, so generated secondary and tertiary mixes reflect real-world mixing behavior.

## Lumi Pigments

The Master palette ships with the following pigments. Swatches show each pigment's typical masstone (full-strength, undiluted) appearance.

### Oranges and Yellows

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,135,20);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrole Orange | PO73 | Red (Scarlet) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(243,114,64);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cadmium Orange | PO20 | Yellow (Body) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(240,180,80);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cadmium Yellow | PY35 | Yellow (Body) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,210,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cadmium Yellow Pale | PY35:Pale | Yellow (Cadmium Pale) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(250,230,5);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lemon Yellow | PY3 | Yellow (Lemon) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,155,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Nickel Azo Yellow | PY150 | Yellow (Mid) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(180,175,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Green Gold | PY129 | Yellow-Green (Gold) |

### Earth Colors

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(200,100,70);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Burnt Sienna | PBr7:Burnt | Earth (Red Brown) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(117,66,0);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Burnt Umber | PBr7:Umber | Earth (Neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(205,68,35);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Raw Sienna | PBr7:Raw | Earth (Yellow Brown) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(187,124,25);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Yellow Ochre | PY42 | Earth (Yellow) |

### Greens

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,166,81);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo Green (YS) | PG36 | Green (Phthalo Yellow-Shade) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(64,130,109);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Viridian | PG18 | Green (Viridian) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,138,112);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Terre Verte | PG23 | Green (Earth Cool) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,110,100);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Green (BS) | PG7 | Green (Phthalo Blue-Shade) |

### Blues and Cyans

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,177,176);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cobalt Turquoise Light | PG50 | Cyan (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,148,214);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cerulean Blue | PB35 | Cyan (Mineral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,100,110);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Phthalo Turquoise | PB16 | Blue (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,123,194);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cobalt Blue | PB28 | Blue (Violet-Lean) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(0,75,115);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Winsor Blue | PB15 | Blue (Phthalo) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(27,63,148);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ultramarine | PB29 | Blue (Violet-Lean) |

### Violets, Magentas and Reds

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(124,65,153);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Brilliant Violet | PV23 | Violet (Dioxazine) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(230,90,180);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanent Rose | PV19:Rose | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(190,40,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Quinacridone Magenta | PV19:Magenta | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(160,30,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,35,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Violet | PV29 | Magenta (Quinacridone) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,10,45);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Maroon | PR179 | Red (Crimson) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(215,30,60);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrole Red | PR254 | Red (Scarlet) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(225,55,65);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Pyrrole Red Light | PR255 | Red (Pyrrole Light) |

### Blacks and Whites

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(22,15,10);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Mars Black (Warm) | PBk11 | Black (Mars) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,28,12);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Perylene Green | PBk31 | Black (Perylene Green) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(10,18,19);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Ivory Black (Cool) | PBk9 | Black (Ivory) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(18,18,18);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Lamp Black (Neutral) | PBk7 | Black (Lamp) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,249,235);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanium White (Warm) | PW6:Warm | White (Titanium Warm) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(255,255,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Titanium White (Neutral) | PW6 | White (Titanium Neutral) |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(245,250,255);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Zinc White (Cool) | PW4 | White (Zinc Cool) |

### Control Grays

Control grays are standardized neutralizers used to predictably desaturate mixes.

| Swatch | Name | CI Code |
| :---: | :--- | :--- |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(135,128,120);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Warm Gray | N_WARM |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(128,128,128);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Neutral Gray | N_NEUTRAL |
| <span style="display:inline-block;width:1.3em;height:1.3em;border-radius:3px;background:rgb(120,128,135);vertical-align:middle;border:1px solid rgba(0,0,0,0.25)"></span> | Cool Gray | N_COOL |

## The Palette Map

The Palette Map visualizes the active palette as a hue wheel: 36 hue sectors (10° steps) × 15 lightness cells. When primaries are added, the system generates secondary and tertiary mixes and places them in the appropriate map positions.

Clicking a cell selects a color as the foreground. Shift-click assigns it as a parent endpoint in the Palette Mixer.

## The Palette Mixer

The Palette Mixer derives new colors from two parent entries using a fixed three-stage pipeline:

1. **Blend**: Spectral WGM between Parent A (CCW) and Parent B (CW).
2. **Chroma**: Blend toward the palette's neutral spectrum, reducing saturation.
3. **Tone**: Blend toward mixing white or mixing black, adjusting lightness.

Tone is applied last so lightness adjustments are not diluted by chroma changes. Value Lock and Band Clamp controls constrain results to a specific lightness level or value band.

Mixed colors can be saved to the palette as **Custom** entries, storing the full recipe (parent UIDs, blend factor, tone, chroma values) for later recovery.

## Canvas Pixels Are RGB

The spectral system operates entirely within palette construction and color selection. When a brush stroke is applied, the foreground color — already converted to linear RGB — is what gets painted. The canvas stores standard RGB pixel data.

Spectral mixing improves the experience of building a palette and choosing colors in a way consistent with physical pigment behavior, without changing how image data is stored or composited.
