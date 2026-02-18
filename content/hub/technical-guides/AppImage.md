---
title: "AppImage"
type: docs
url: "hub/technical-guides/AppImage"
---

An AppImage is a single-file Linux application package. You download one file, mark it executable, and run it without installing software system-wide.

Official AppImage site: https://appimage.org/

The AppImage provides a portable version of Lumi that runs without installation or system modification. It is ideal for artists who want to use the software immediately without managing dependencies, compiling source code, or configuring a development environment.

As a self-contained executable, the AppImage can be stored anywhere on the system. This makes it easy to test new releases, keep multiple versions, or move the software between machines.

For Lumi’s development process, the AppImage functions as a portable test build that closely matches continuous integration output. This allows reliable testing in a consistent environment while keeping local source builds focused on development work.

Note: CI builds the AppImage using Lumi’s in-repo integrated dependency sources (BABL/GEGL/GTK3), so the dependency stack is consistent with the local `lumi-build-script.sh` workflow.

## Release vs Development AppImage

- **Release AppImage**: not available yet (Lumi has not been released).
- **Development AppImage (CI artifact)**: automatically generated from ongoing development commits for testing.

This guide mainly covers the **development AppImage** workflow.

Current artifact page:

https://gitlab.gnome.org/pixelmixer/lumi-dev/-/artifacts

## CI AppImage Download Basics

CI produces artifact zip files (for example `lumi-appimage*.zip`).

Basic manual flow:

1. Download the latest CI artifact zip.
2. Extract it.
3. Run the included `Lumi*.AppImage` file.

The scripts below are optional helpers that automate these steps.

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Unpack latest downloaded CI zip from ~/Downloads
bash lumi-appimage-unpack-zip.sh

# Launch AppImage with terminal output
bash lumi-appimage-launch.sh
```

## Optional Helper Scripts

- `lumi-appimage-unpack-zip.sh`
  - finds the latest `lumi-appimage*.zip` in `~/Downloads`
  - installs AppImage to `~/AppImage/Lumi/Lumi_CI.AppImage`
  - installs desktop resources to `~/.local/share/applications/lumi.desktop`

- `lumi-appimage-launch.sh`
  - launches the AppImage in a terminal
  - enables runtime output (`APPIMAGE_DEBUG=1`)

## Common Notes

- If you run AppImage manually (without helper scripts), make it executable first:

```bash
chmod +x ~/AppImage/Lumi/Lumi_CI.AppImage
```

`lumi-appimage-unpack-zip.sh` already applies executable permissions automatically.

- If Lumi is already running from another build, close it before launching the AppImage.
