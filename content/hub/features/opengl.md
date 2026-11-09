---
title: "OpenGL"
type: docs
url: "hub/features/opengl"
---

Lumi can draw the layer stack with OpenGL, and it can paint brush strokes the same way. Canvas navigation stays smooth. Large airbrushes and large texture stamps are much faster. Whether a particular tool paints this way is stored with its tool preset.

## The layer stack

The image window shows the composed layer stack. With OpenGL, that picture is held as tiles on the graphics processor, already prepared for the display. Pan, zoom, rotation, and flip change the view of those tiles. On a large canvas the gestures stay smooth, because navigation is a change of view over a picture already there.

Edits update the tiles they touch. Guides, the brush cursor, and the selection outline are drawn over that picture.

While the pen is down, a stroke that uses OpenGL can be shown in place among the layers around it, including layers above the one being painted. The mark is written into the layer when the pen lifts.

## Strokes, per tool preset

OpenGL painting is a choice on the tool preset, alongside the brush and the dynamics. One preset can paint on the graphics processor. Another can keep the ordinary stroke path. Returning to a tool brings that choice back with it.

The gain is largest where stamps are large. On the ordinary path, a broad airbrush falls behind the stylus as the mark grows: dabs queue up, and the stroke develops gaps, including while the pen is held and the brush keeps emitting. With OpenGL that airbrush keeps placing marks with the hand.

A large texture stamp shows the same change. The ordinary path spends longer on each dab as the stamp grows, until a stroke stalls. The OpenGL stroke stays with the gesture. Stamps that carry a surface grain can be drawn with OpenGL as well.

Bristle marks can be drawn this way too. A round-tipped bristle stamp can be painted with OpenGL while the bristles change along the stroke. A calligraphic stroke can be drawn as one continuous ribbon, and surface texture can mask that ribbon. [Surface texture](/hub/features/paintbrush/#surface-texture) and [bristles along the stroke](/hub/features/paintbrush/#bristles-along-the-stroke) belong to the brush itself.

Some strokes stay on the ordinary path. Spectral mixing, a filter on the painted layer, and some blend modes are among them. Those marks are still painted.

## When the ordinary canvas remains

OpenGL is used when the graphics hardware can run it and the image fits the memory set aside for the canvas. Otherwise Lumi draws the window as before. The published AppImage carries its own graphics libraries. On a driver those libraries cannot use, including NVIDIA's proprietary driver, the canvas stays on that ordinary path.

## Cinnamon on X11, and GNOME on Wayland

Lumi is developed on Debian with Cinnamon and X11. That is still the development environment. GNOME on Wayland has been tested for the general appearance of the application and for this OpenGL rendering. Canvas navigation is smooth there as well.
