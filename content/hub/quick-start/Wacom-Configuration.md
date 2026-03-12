---
title: "Wacom Configuration"
type: docs
url: "hub/quick-start/Wacom-Configuration"
---

For digital painting in Lumi, a simple **linear pressure setup** is recommended.

- Keep the tablet driver pressure curve linear.
- Keep pressure/input curves in Lumi mostly linear.
- Shape the feel with the brush itself, since brush dynamics can already be non-linear.

We recommend maintaining the default linear pressure curve at the OS driver level. Compounding multiple non-linear curves often leads to unpredictable input behavior; by keeping the driver neutral, you ensure that any adjustments made within Lumi·o remain intuitive and reproducible. A slight adjustment to Lumi's global curve can still be reasonable when needed.

## Global Stylus Curve in Lumi

In Lumi, open:

Edit → Preferences → Input Devices → Configure Tablet, Stylus and Further Devices...

Here you can set the global pressure curve for your stylus.

## Wacom Touch Ring

Lumi now supports Wacom Touch Ring input directly, including modifier-based ring inputs.

In the same device configuration dialog, you can assign ring actions per input, including:

- Brush Size
- Brush Relative Size
- Brush Angle
- View Angle
- View Zoom

Note: An image must be active for the Touch Ring to affect attributes. The ring defaults to a relative brush size change. To prevent accidental adjustments a half-circle swipe is required to trigger a command (e.g., a half-swipe clockwise doubles brush size).
