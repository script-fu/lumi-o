---
title: "Spectral Color Mixing"
type: docs
url: "hub/features/spectral-color"
---

Lumi's palette system uses spectral colour mixing to make digital colour selection behave more like mixing physical paint. Instead of treating colours as simple screen values, Lumi models how pigments absorb and reflect light while palette colours are being generated and mixed.

The result is a colour workflow where mixtures feel more familiar to painters: yellows and blues can move toward greens, saturated colours can settle into believable neutrals, and limited palettes develop a recognisable harmony.

## Pigment-like behaviour

Traditional digital colour blending often moves through mathematical midpoints that do not resemble paint. Spectral mixing gives Lumi a richer foundation for palette construction, allowing colours to interact according to pigment-like tendencies.

This makes palette exploration more intuitive for artists who think in terms of paint, not just screen colour values. A mixture can darken, mute, warm, cool, or shift hue in ways that feel connected to the source colours.

## Palettes with character

Because spectral behaviour is part of the palette system, each palette develops its own personality. A small limited palette may produce strong unity and muted transitions, while a broader palette can open a wider range of clean hues and controlled neutrals.

Pigment identity matters. The same visible colour can mix differently depending on the kind of pigment it represents, which helps Lumi capture some of the practical differences painters expect from real materials.

## Mixing before painting

Spectral colour in Lumi is used while building palettes, generating related colours, and mixing new palette entries. Once a colour is applied to the canvas, it becomes normal image colour data for painting and compositing.

This keeps the painting pipeline practical while still improving the artist's experience of choosing and constructing colour. The spectral model shapes the palette; the canvas remains efficient and compatible with standard image workflows.

## A bridge between digital and physical colour

The purpose of spectral mixing is not to imitate every physical property of paint, but to bring the most useful part of pigment behaviour into digital painting: believable mixture relationships.

For artists, that means a palette can be explored with more trust. Colours produced from the same pigment set tend to belong together, and saved mixes retain a sense of origin within the palette rather than feeling like unrelated sampled values.

## Colour as a system

Spectral mixing connects the Palette Editor, Palette Map, and Palette Mixer into one colour system. Pigments define the starting point, the map shows what the palette can reach, and the mixer lets artists search for useful colours within that space.

This gives Lumi a colour workflow centred on painting decisions: choose the pigments, explore their mixtures, keep the colours that matter, and paint with a palette that behaves as a coherent whole.
