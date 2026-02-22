---
title: "File Recovery"
type: docs
url: "hub/features/recovery"
---

Lumi maintains two independent recovery systems — automatic background saves and manual incremental checkpoints — both accessible from a single dialog.

## Access

**File** → **Recover Image**

The dialog opens pre-populated with recovery states for the currently open file. Use the file chooser at the top to switch to a different `.lum` file.

---

## Autosave

Lumi saves a background snapshot of your work at regular intervals while editing. Autosaves are written to a **separate cache directory**, leaving the working `.lum` file untouched:

```
~/.cache/lumi/autosave/~home~user~projects~my-painting.lum/
```

The path encoding uses `~` as a separator to create a unique cache directory per file. This means autosaves are available even if the project file itself is lost or corrupted.

- **Frequency**: Configurable in **Edit** → **Preferences** → **Performance** → Autosave interval.
- **Storage location**: Also set in Preferences → Performance.
- **Purpose**: Crash recovery. The Autosave tab in the Recover Image dialog shows available autosave states with timestamps.

When you open a file that has newer autosave data, Lumi notifies you at open time.

---

## Incremental Saves

Incremental saving is a manual checkpoint system stored **inside the project file** under `recovery/`. The structure is:

```
my-painting.lum/recovery/
  └── primary-01.lum/       (full baseline, created on first Ctrl+S)
      ├── delta-0001.lum/   (Ctrl+S checkpoint, only modified buffers)
      ├── delta-0002.lum/
      └── ...
```

A new `primary-NN.lum/` baseline is written after **File → Save**. Subsequent Ctrl+S presses create `delta-NNNN.lum/` subdirectories containing only the buffers that changed since the last baseline. Autosave deltas and manual save deltas use separate counters so they do not interfere with each other's history.

Incremental saves are **disabled by default** and must be enabled per-project:

1. **File** → **Save As** (Shift+Ctrl+S).
2. In the Save As dialog, check **Incremental Save** and optionally set a **Max Saves** limit.
3. The setting is stored with the project and applies to all subsequent Ctrl+S presses.

When you open a `.lum` file that has newer incremental saves than the primary save, Lumi shows an **Incremental Save Detected** prompt offering to load the most recent checkpoint.

---

## Recover Image Dialog

The dialog has three tabs and two action buttons.

### Autosave Tab

Lists all available autosave states for the selected file with timestamps and thumbnails (where available). Select a state and click **Recover** to open it.

Use this tab to:
- Recover after a crash.
- Revert to an earlier state from the same session.

### Incremental Tab

Lists all checkpoint states stored inside the project file. Each entry shows the checkpoint timestamp. Select a checkpoint and click **Recover** to open it.

Use this tab to:
- Return to an earlier point in a session without having saved separate files.
- Browse through the version history of a project.

### Latest Tab

The default tab when the dialog opens. Automatically identifies the newest available recovery state across both autosaves and incremental checkpoints, and shows its timestamp. Click **Recover** to load it immediately without browsing individual states.

---

## Buttons

| Button | Action |
|--------|--------|
| **Recover** | Opens the selected recovery state as a new image. |
| **Close** | Dismisses the dialog without recovering. |
| **Clean Up Old States…** | Opens a cleanup prompt (see below). |

---

## Clean Up Old States

Accumulating recovery states over time can consume significant disk space. The **Clean Up Old States…** button (bottom-left of the dialog) opens a cleanup prompt for the active tab (Autosave or Incremental).

The prompt shows:
- How many full saves exist for the file.
- The total disk space they occupy.
- A **Keep most recent** spin button to select how many saves to retain.

Setting **Keep most recent** to `0` deletes all recovery states. The next Ctrl+S after a full cleanup will write a fresh primary save.

---

## Startup Recovery

On startup, if Lumi detects that the most recently opened file has newer autosave data than the last full save, it presents a recovery prompt before loading. You can accept (load the autosave) or dismiss (open the primary save as normal).
