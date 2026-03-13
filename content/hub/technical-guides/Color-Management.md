---
title: "Color Management"
type: docs
weight: 15
---

Lumi-o is configured to work out-of-the-box. As long as you are working on an image with **16-bit or greater precision**, the software is already set up to use the default bundled soft-proofing (CMYK) and built-in sRGB profiles; it should all just work without any configuration.

For those needing deeper control, this guide explains Lumi's core color management model, the difference between an image profile and a soft-proof profile, where the controls live, and exactly how the default profiles bundle with the application.

## Quick Summary

Lumi uses three different profile roles:

1. **Image working profile**
   - Defines what the image's RGB or grayscale numbers mean.
   - Used for assign/convert operations.
   - Typical examples: built-in sRGB, Adobe RGB.

2. **Display profile**
   - Describes your monitor.
   - Used to show the image correctly on your screen.
   - Usually provided by the system or chosen in Preferences.

3. **Soft-proof profile**
   - Simulates another output device or print condition.
   - Does **not** redefine the image's pixel values.
   - Typical examples: CMYK press profiles such as `CoatedFOGRA39`.

## Image Profile vs Soft-Proof Profile

### Image Profile

Use this when you want to tell Lumi what color space the image is actually in.

Two common operations:

- **Assign profile**
  - Changes the profile label attached to the image.
  - Does **not** convert pixel values.
  - Use only when the pixel numbers are already in that profile's space.

- **Convert to profile**
  - Converts pixel values from the current image profile to a new one.
  - Use when you want the image to truly move into a different working space.

**Menu locations:**
- Image > Color Management > Assign Color Profile...
- Image > Color Management > Convert to Color Profile...

### Soft-Proof Profile

Use this when you want to preview how the image would reproduce on a target device or print condition.

Soft-proofing:
- leaves the image working space alone
- changes the preview pipeline
- can mark out-of-gamut colors
- is intended for preview, not reassignment of image data

**Menu locations:**
- Image > Color Management > Soft-Proof Settings > Choose Soft-Proof Profile...
- Image > Color Management > Soft-Proof Settings > Rendering Intent
- Image > Color Management > Soft-Proof Settings > Black Point Compensation
- View > Color Management > Enable Soft-Proof Preview
- View > Color Management > Mark Out of Gamut Colors

## How To See The Soft-Proof Preview

There are two main entry points for toggling soft proofs.

### 1. View Menu

Use:
- View > Color Management > Enable Soft-Proof Preview

This turns the preview simulation on or off for the current display.

### 2. Status Bar Toggle

Lumi also exposes soft-proofing directly in the bottom status bar.

- **Left-click** (toggle): enable or disable proof colors
- **Right-click**: open the soft-proofing popover where you can tweak:
  - current profile
  - profile chooser
  - rendering intent
  - black point compensation
  - out-of-gamut marking

{{< callout type="warning" >}}
**Important Note on Precision**
Soft-proof preview is only enabled for **16-bit and 32-bit** images.
For **8-bit** images, the toggle is disabled and Lumi will prompt you to convert precision to a higher depth first before previewing colors accurately.
{{< /callout >}}

## Preferences and Defaults

Global defaults live in:
- Edit > Preferences > Color Management

Relevant sections:
- **Manual Monitor Profile**
- **Preferred RGB profile**
- **Preferred grayscale profile**
- **Soft-Proofing**

### Current Lumi defaults

#### Working Spaces
Bundled working-space ICCs currently offered from the shared data folder:
- `AdobeRGB1998.icc`
- `AppleRGB.icc`

For standard sRGB work, Lumi also provides a **built-in sRGB working profile internally**.

#### Soft-Proof Defaults
Bundled soft-proof profiles currently installed:
- `CoatedFOGRA39.icc`
- `USWebCoatedSWOP.icc`
- `JapanColor2001Coated.icc`

When available, `CoatedFOGRA39.icc` is used as the default bundled soft-proof/CMYK reference profile.

## Practical Workflows

### For painting and normal screen work
- Keep the image in built-in sRGB or another valid RGB working space.
- Let Lumi use the system monitor profile if available.

### For print preview
- Keep the image in its standard RGB working space.
- Choose a soft-proof profile that matches the target print condition (e.g. FOGRA39).
- Enable soft-proof preview.
- Optionally enable gamut warnings to see clipped rendering intents.
