---
title: "Quick Start"
type: docs
url: "hub/quick-start"
---

Lumi is not released yet, it is available as a developement version.

If you are already on Linux and want to run Lumi quickly, use the latest **development AppImage** from GitLab artifacts:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

1. Download the latest development AppImage artifact zip.
2. Extract the zip.
3. Double-click the `Lumi*.AppImage` file to run it.

The AppImage should already be runnable. If it is not, enable **Allow executing file as program** in the file's permissions, or use the terminal method below.

```bash
chmod +x Lumi*.AppImage
./Lumi*.AppImage
```

## Wacom setup on Linux

For digital painting in Lumi, a simple **linear pressure setup** is usually best:

- Keep the tablet driver pressure curve linear.
- Keep pressure/input curves in Lumi linear.
- Shape the feel with the brush itself, since brush dynamics can be non-linear.

You can check and reset the Linux driver curve with:

```bash
xsetwacom --get "Wacom Intuos Pro L Pen stylus" PressureCurve
xsetwacom --set "Wacom Intuos Pro L Pen stylus" PressureCurve 0 0 100 100
```

Practical tips:

- Lumi currently blocks the problematic Wacom pad/touch-ring input to avoid X11 glitches. Map tablet buttons to **relative** brush-size up/down instead.
- If brush-size dragging with `Alt` does not work, your desktop may be using `Alt` to move windows. Change that window-manager shortcut to `Super` or disable it.

If you want to work from source code, go to [Technical Guides](/hub/technical-guides/) and [Installation](/hub/technical-guides/Installation/).
