---
title: "Workspaces"
type: docs
url: "hub/features/workspaces"
---

A workspace is a saved snapshot of your entire UI environment: which panels are open and where, the canvas decorations and padding for both Normal and Fullscreen view, the active theme and icon set, toolbox layout, the active palette, and your tool settings. Lumi lets you save as many named workspaces as you like and switch between them instantly — all open images update in place, no restart required.

## What a Workspace Saves

Each named workspace stores the following independently:

| Component | What it covers |
| :--- | :--- |
| **Layout** | Window position and size, dock arrangement (left and right panel columns, which panels are open and in which order), single- vs multi-window mode, maximised state, tab bar visibility and position |
| **Tool Options** | The current settings for every tool (brush size, hardness, warp behavior, etc.) |
| **Input Devices** | Input device configuration: pressure curves, button assignments, axis mappings for stylus and other devices |
| **Canvas Decorations** | Per-workspace defaults for rulers, scrollbars, guides, grid, selection highlight, layer boundary, and canvas boundary — set via **Preferences → Image Windows → Default Appearance** and **Full Screen Appearance**, independently for Normal and Fullscreen view |
| **Canvas Padding** | Per-workspace padding mode and color for Normal and Fullscreen view — set via **Preferences → Image Windows → Default Appearance** |
| **Theme & Icons** | Active theme, dark/light color variant, icon set, icon size override, and font scale |
| **Toolbox** | FG/BG widget position (top/bottom/left/right), FG/BG scale, Wilber mascot visibility, tool group headers |

The active **palette** and **tool preset** are also recorded per-workspace and restored when you switch.

> **Canvas decorations and padding** are controlled by
> **Preferences → Image Windows → Advanced Window Options → Default Appearance** (Normal view)
> and **Full Screen Appearance** (Fullscreen view). Adjust those settings to your liking,
> then save the workspace. The **View menu** items (rulers, guides, etc.) are local to the
> current image window and are not saved per-workspace.

### Live updates on switch

When you switch workspaces, all open image windows update immediately — rulers, guides, scrollbars, padding color, and every other View setting change in place without needing to close and reopen images.

## Access

**Edit → Preferences → Workspace**

The top section of the Workspace preferences page lists all your saved workspaces and provides controls to manage them.

## Creating a Workspace

Set up your panels, tools, and palette exactly as you want them, then:

1. Open **Edit → Preferences → Workspace**.
2. Click **Save Layout As…**.
3. Enter a name and click **Save**.

The new workspace appears in the **Active layout** dropdown and in the **Windows** menu.

## Switching Workspaces

There are two ways to switch:

- **Windows menu**: The layout names appear under **Windows → Layout** for quick access from the canvas.
- **Preferences → Workspace**: Select a layout from the **Active layout** dropdown and click **Reload Layout**.

Switching is immediate — Lumi rebuilds the panel layout, restores tool options, reloads device settings, updates canvas decorations, padding, theme, and toolbox layout, all without restarting.

## Managing Workspaces

From **Edit → Preferences → Workspace**:

| Action | Effect |
| :--- | :--- |
| **Save Layout** | Overwrites the current workspace with your present settings. |
| **Save Layout As…** | Creates a new named workspace from your present settings. |
| **Rename Layout…** | Renames the selected workspace. |
| **Reload Layout** | Applies the selected workspace immediately. |
| **Delete Layout…** | Permanently removes the selected workspace and its files. |

## Persistence Settings

The lower part of the Workspace preferences page controls what Lumi saves automatically:

- **Save window positions on exit**: When on, dock and window positions are written to disk every time you quit.
- **Open windows on the same monitor**: Reopens each window on the monitor it was on during the last session.
- **Save tool options on exit**: Saves the current tool settings when quitting.
- **Save input device settings on exit**: Saves stylus and device configuration when quitting.

These settings apply per-workspace — each layout maintains its own saved state independently.

## Example Workflows

A few ways artists might use multiple workspaces:

- **Painting** — large brush docks, warm padding color (set in Preferences → Image Windows → Default Appearance), your preferred theme variant
- **Inking** — guides and canvas boundary on, scrollbars on (set in Preferences → Default Appearance), neutral padding color
- **Roughs** — docks hidden, no rulers or grid, dark padding, compact icon size to maximize canvas space
- **Fullscreen focus** — different padding color and decoration settings in Full Screen Appearance vs Default Appearance, so toggling fullscreen gives a genuinely different working environment
- **Scripting** — scripting panel open, font-size bump for readability, different icon set
