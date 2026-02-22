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

The spectral computation runs during palette construction, when generating secondary and tertiary palette entries and when the Palette Mixer blends two parent colors. The resulting color is converted to linear RGB for display and for painting.

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
| {{< swatch "245,135,20" >}} | Pyrrole Orange | PO73 | Red (Scarlet) |
| {{< swatch "243,114,64" >}} | Cadmium Orange | PO20 | Yellow (Body) |
| {{< swatch "240,180,80" >}} | Cadmium Yellow | PY35 | Yellow (Body) |
| {{< swatch "245,210,25" >}} | Cadmium Yellow Pale | PY35:Pale | Yellow (Cadmium Pale) |
| {{< swatch "250,230,5" >}} | Lemon Yellow | PY3 | Yellow (Lemon) |
| {{< swatch "225,155,10" >}} | Nickel Azo Yellow | PY150 | Yellow (Mid) |
| {{< swatch "180,175,45" >}} | Green Gold | PY129 | Yellow-Green (Gold) |

### Earth Colors

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| {{< swatch "200,100,70" >}} | Burnt Sienna | PBr7:Burnt | Earth (Red Brown) |
| {{< swatch "117,66,0" >}} | Burnt Umber | PBr7:Umber | Earth (Neutral) |
| {{< swatch "205,68,35" >}} | Raw Sienna | PBr7:Raw | Earth (Yellow Brown) |
| {{< swatch "187,124,25" >}} | Yellow Ochre | PY42 | Earth (Yellow) |

### Greens

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,166,81" >}} | Phthalo Green (YS) | PG36 | Green (Phthalo Yellow-Shade) |
| {{< swatch "64,130,109" >}} | Viridian | PG18 | Green (Viridian) |
| {{< swatch "128,138,112" >}} | Terre Verte | PG23 | Green (Earth Cool) |
| {{< swatch "0,110,100" >}} | Winsor Green (BS) | PG7 | Green (Phthalo Blue-Shade) |

### Blues and Cyans

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| {{< swatch "0,177,176" >}} | Cobalt Turquoise Light | PG50 | Cyan (Mineral) |
| {{< swatch "0,148,214" >}} | Cerulean Blue | PB35 | Cyan (Mineral) |
| {{< swatch "0,100,110" >}} | Phthalo Turquoise | PB16 | Blue (Phthalo) |
| {{< swatch "0,123,194" >}} | Cobalt Blue | PB28 | Blue (Violet-Lean) |
| {{< swatch "0,75,115" >}} | Winsor Blue | PB15 | Blue (Phthalo) |
| {{< swatch "27,63,148" >}} | Ultramarine | PB29 | Blue (Violet-Lean) |

### Violets, Magentas and Reds

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| {{< swatch "124,65,153" >}} | Brilliant Violet | PV23 | Violet (Dioxazine) |
| {{< swatch "230,90,180" >}} | Permanent Rose | PV19:Rose | Magenta (Quinacridone) |
| {{< swatch "190,40,120" >}} | Quinacridone Magenta | PV19:Magenta | Magenta (Quinacridone) |
| {{< swatch "160,30,65" >}} | Permanent Alizarin Crimson | PV19:Crimson | Magenta (Quinacridone) |
| {{< swatch "120,35,65" >}} | Perylene Violet | PV29 | Magenta (Quinacridone) |
| {{< swatch "135,10,45" >}} | Perylene Maroon | PR179 | Red (Crimson) |
| {{< swatch "215,30,60" >}} | Pyrrole Red | PR254 | Red (Scarlet) |
| {{< swatch "225,55,65" >}} | Pyrrole Red Light | PR255 | Red (Pyrrole Light) |

### Blacks and Whites

| Swatch | Name | CI Code | Family |
| :---: | :--- | :--- | :--- |
| {{< swatch "22,15,10" >}} | Mars Black (Warm) | PBk11 | Black (Mars) |
| {{< swatch "18,28,12" >}} | Perylene Green | PBk31 | Black (Perylene Green) |
| {{< swatch "10,18,19" >}} | Ivory Black (Cool) | PBk9 | Black (Ivory) |
| {{< swatch "18,18,18" >}} | Lamp Black (Neutral) | PBk7 | Black (Lamp) |
| {{< swatch "255,249,235" >}} | Titanium White (Warm) | PW6:Warm | White (Titanium Warm) |
| {{< swatch "255,255,255" >}} | Titanium White (Neutral) | PW6 | White (Titanium Neutral) |
| {{< swatch "245,250,255" >}} | Zinc White (Cool) | PW4 | White (Zinc Cool) |

### Control Grays

Control grays are standardized neutralizers used to predictably desaturate mixes.

| Swatch | Name | CI Code |
| :---: | :--- | :--- |
| {{< swatch "135,128,120" >}} | Warm Gray | N_WARM |
| {{< swatch "128,128,128" >}} | Neutral Gray | N_NEUTRAL |
| {{< swatch "120,128,135" >}} | Cool Gray | N_COOL |

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

The spectral system operates entirely within palette construction and color selection. When a brush stroke is applied, the foreground color (already converted to linear RGB) is what gets painted. The canvas stores standard RGB pixel data.

Spectral mixing improves the experience of building a palette and choosing colors in a way consistent with physical pigment behavior, without changing how image data is stored or composited.
