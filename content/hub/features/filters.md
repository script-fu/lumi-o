---
title: "Filters"
type: docs
url: "hub/features/filters"
---

Lumi's Filters menu brings together corrective adjustments, stylized lens effects, procedural texture generators, print-inspired treatments, and analysis tools in one place. The menu order is practical rather than academic: blur and enhancement tools sit next to each other, distort and lighting effects are grouped by look, and texture or pattern generators are kept together when the goal is to build source material rather than modify an existing image.

Filter dialogs follow the same general workflow. Presets, preview, split view, and opacity or blend controls let an effect be tuned quickly, and on layers the result can stay as an editable non-destructive filter instead of being merged immediately. Lumi also keeps a recent history of filter use, so repeating the last effect or reopening the last dialog is part of the normal painting rhythm rather than a separate task.

## Blur

### Gaussian blur

Gaussian Blur is Lumi's standard softening filter: a clean, even blur with separate horizontal and vertical size controls, edge handling, and kernel options. It is the general-purpose choice for soft focus, softened masks, atmospheric depth, and any workflow where the blur itself should stay neutral.

### Pixelize

Pixelize reduces detail into deliberate block structures instead of a soft blur. Because the dialog exposes block width, block height, offsets, pixel shape, and fill behaviour, it works as both a coarse censoring effect and a controllable mosaic or low-resolution graphic treatment.

### Selective Gaussian blur

Selective Gaussian Blur softens within regions while trying to preserve stronger edges. It is useful when an image needs calmer texture or reduced chatter without losing the larger shape boundaries that still need to read clearly.

### Lens blur

Lens Blur is one of Lumi's more illustration-focused blur filters. Its controls are built around polygon iris shape, blade curvature, anamorphic stretch, highlight boosting, and a configurable focus region, so it behaves less like a generic softener and more like a stylized depth-of-field tool with shaped bokeh.

### Tilt-shift

Tilt-shift keeps a controllable focus band sharp while progressively blurring the image above and below it. The dialog's band angle, feather, perspective bias, iris shape, and miniature boost make it well suited to miniature-look scenes, architectural views, and any composition where focus should read as a designed stripe rather than a circular depth cue.

### Circular motion blur

Circular Motion Blur smears detail around a centre point, turning edges into rotational trails. It is the natural choice for spinning subjects, turbine-like energy, or illustrations that need a sense of orbital movement.

### Linear motion blur

Linear Motion Blur stretches detail in one direction, simulating travel, camera movement, or fast gesture across the frame. It is especially useful when the motion needs to feel directional and graphic rather than diffused.

### Zoom motion blur

Zoom Motion Blur radiates detail outward from a centre, producing the feeling of a rush toward or away from the viewer. It works well for impact moments, speed lines, and compositions that need a camera-zoom energy without repainting the whole image.

## Enhance

### High pass

High Pass isolates fine local contrast rather than broad tonal change. With only scale and contrast to manage, it is a straightforward tool for extracting edge detail, building crisp overlays, or preparing sharpening passes that should emphasize structure more than colour.

### Noise reduction

Noise Reduction is the opposite move: it suppresses unwanted fine variation so larger forms read more clearly. It is useful when scanned material, compressed textures, or overworked passages need to be simplified before further painting or filtering.

### Sharpen

Sharpen uses an unsharp-mask model, with radius, amount, and threshold controlling how strongly local contrast is pushed. In practice that makes it suitable for restoring clarity after blur, export resizing, or subtle finishing passes where detail needs to come forward without turning every pixel into noise.

## Distort

### Chromatic aberration

Chromatic Aberration separates colour channels outward from a chosen centre, with controls for radial or tangential direction, bias between channel pairs, falloff, and luminance preservation. The code and dialog both treat it as a two-way tool: it can add stylized lens fringing for energy, or reverse the sign to correct mild aberration in source material.

### Lens distortion

Lens Distortion reshapes the image through barrel or pincushion-style curvature, edge terms, zoom compensation, centre offsets, and corner brightening. That makes it useful both for correcting an image that feels optically bent and for deliberately pushing one toward a wide-angle or retro lens character.

## Lighting

### Bloom

Bloom turns bright areas into controlled glow, with threshold, softness, radius, and strength defining how far the light spreads and how strongly it lifts the image. The extra exposure-limiting control keeps it usable as a highlight effect rather than an automatic washout.

### Sky

Sky is more than a tint or gradient overlay: it renders an analytic sky using Preetham, Hosek/Wilkie, or Nishita models. Because the dialog exposes projection, sun angle, turbidity, atmospheric density, altitude, sun-disc controls, and exposure, it can build anything from a simple clear backdrop to a more physically grounded sunset or twilight sky.

### Vignette

Vignette darkens, colours, or even erases toward the image edges, with shape, radius, softness, gamma, proportion, squeeze, rotation, and on-canvas positioning controls. It works as a classic photographic edge treatment, but it is flexible enough to act as a framing mask or an irregular compositional spotlight.

## Noise

### HSV noise

HSV Noise randomizes hue, saturation, and value independently. That makes it useful when an image needs colour liveliness or analogue instability without fully breaking apart the local structure.

### Hurl

Hurl is the extreme version of noise: it replaces pixels with completely random colours. It is best thought of as a destructive chaos source for glitch work, distressed textures, or masks that need aggressive breakup.

### Pick

Pick replaces each pixel with a randomly chosen neighbour, so the image stays related to its source instead of becoming pure static. The result is a shuffled, granular variation that can feel more organic than fully random noise.

### Spread

Spread scatters pixels by randomly displacing them within a radius. It is useful when you want motionless disruption: a broken surface, a smeared edge, or a distressed texture that still carries the source image's colour relationships.

### Fractal

Fractal generates tileable fractal Perlin noise, which makes it especially valuable as a reusable source for masks, clouds, paper texture, terrain-like breakup, and procedural overlays. Because it tiles, it can feed larger workflows without creating obvious seams.

### Blue noise grain

Blue Noise Grain is Lumi's film-and-print style monochrome grain generator. The dialog's grain-size presets, blue-noise masking, midtone bias, shadow bias, and seed controls show that it is designed to place grain evenly and controllably, not just to spray random monochrome speckles over the image.

### Risograph grain

Risograph Grain builds on the same grain logic but turns it into a two-plate print effect. Separate ink colours, plate balance, deliberate misregistration, and seeded variation make it a good fit for poster work, indie print aesthetics, and illustrations that should feel physically overprinted rather than digitally perfect.

### Halftone (FM)

Halftone (FM) creates a stochastic, frequency-modulated halftone using blue-noise or related thresholding methods. With colour modes for monochrome, duotone, and CMYK, plus dot-gain and plate decorrelation controls, it is aimed at print-like texture that stays irregular and lively instead of falling into a rigid grid.

## Edges

### Difference of Gaussians

Difference of Gaussians detects edges by subtracting two blurred versions of the image from one another. It is a compact, useful operator for edge maps, stylized line extraction, and finding structural transitions without committing to a full thresholded outline.

## Morphology

### Median

Median replaces each pixel with the median value from its neighbourhood, which tends to remove isolated noise while preserving stronger boundaries better than a simple blur. It is a practical cleanup filter for flattening small visual chatter without immediately softening the whole image.

### Dilate

Dilate grows lighter regions outward using the same shape-aware neighbourhood logic. In image-making terms, it can thicken bright marks, expand light shapes, or close small dark gaps.

### Erode

Erode does the complementary move, growing darker regions and pulling back lighter ones. It is useful for thinning light details, enlarging dark masses, or tightening masks and graphic shapes.

## Pattern

### Checkerboard

Checkerboard generates a regular alternating tile pattern. It is simple, but that simplicity makes it useful for testing transparency, building masks, blocking graphic backgrounds, or creating clean geometric source material.

### Grid

Grid draws repeated horizontal and vertical divisions, making it useful for layout guides, design backdrops, technical illustration, and procedural masking. Because it is generated as a filter, the spacing and appearance can be tuned without hand-building the pattern.

### Voronoi

Voronoi generates a tileable cellular texture from seeded points, with controls for feature type, distance metric, randomness, fractal detail, and seamless wrapping. In practice it can move from clean cracked-cell structures to more organic stone, skin, map, or abstract network patterns.

### Wave

Wave produces banded or ringed patterns shaped by waveform profile, geometric arrangement, distortion, fractal detail, and phase offset. That makes it more than a simple stripe tool: it can generate controlled ripples, topographic bands, moire-like graphics, or noisy concentric pattern fields.

### Halftone (AM)

Halftone (AM) applies a classic amplitude-modulated dot screen, with frequency, dot shape, sharpness, colour mode, and CMYK angle controls for rosette-style print structure. Compared with FM halftoning, it is the more ordered, recognisably mechanical option when the desired look is newsprint, offset lithography, or deliberately visible screen geometry.