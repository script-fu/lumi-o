---
title: "File Format (.lum)"
type: docs
url: "hub/features/file-format"
---

Lumi's native `.lum` format is a project directory, not a single sealed file. It is designed for layered illustration: deep layer trees, large canvases, masks, non-destructive effects, and checkpoints that do not have to duplicate the entire painting.

The format's job is to keep that working structure intact — so a project can be reopened faithfully, inspected when something goes wrong, and recovered from a recent checkpoint without treating the artwork as one opaque blob.

## Separate pieces, on purpose

A `.lum` project is a folder. The layer tree and image properties live in readable XML. Each layer and mask keeps its own pixel buffer, named after the artwork rather than after an internal ID. Vector paths are stored as ordinary SVG. Heavy filter settings sit in their own files next to the image. ICC profiles are stored once at the project root, so recovery snapshots can refer to them instead of copying them.

That split is what makes the rest of the format possible. Unchanged layers can be left alone on disk. A damaged buffer fails on its own instead of taking the whole file with it. Missing layer pixels become empty layers that still have names, positions, and blend settings; a missing group composite is rebuilt from the children. The project remains a map of how the painting was built.

Pigment palettes stay with Lumi's colour tools. A project can remember which palette was associated with the image, but the palette library itself is outside the `.lum`.

## Editable state, not a flatten

The file stores the working painting. Layers remain layers, groups remain groups, and masks remain masks, including offsets, locks, blend behaviour, and filter stacks. Non-destructive filters are saved as operations and parameters rather than as baked pixels. A layer that is a single flat colour does not need a pixel file at all.

Collapsed groups also keep a composited view of themselves. That cached composite is what appears on the canvas when a group is shut, so children do not have to be reconstructed just to look at the picture. Display-only inspection modes stay out of that cache: showing a mask or alpha for editing is restored as metadata, not burned into the saved group.

## Large files can stay partly on disk

Opening a `.lum` does not have to load every pixel. Content inside collapsed groups can remain on disk while the group's saved composite is shown immediately. Expanding a group is when those layers, masks, and nested groups come into memory. Groups that stay closed stay cheap.

The file also records which groups were actually in use. Groups on the active selection path can reopen expanded; other folders are stored as collapsed even if they happened to be open in the last session. That keeps a deep file from hydrating every unused branch the moment it is opened.

Grouping is therefore a performance choice as well as an organisational one. Large background plates, archived experiments, and unused variants can sit in closed groups without occupying the same memory as the layers being painted. Saving follows the same rule: still-hidden buffers are copied or skipped as files, not inflated back into memory just to be written out again.

## Checkpoints that write only what changed

File → Save updates the working project. Incremental saves and autosaves write into a recovery tree, and they only write dirty data — changed layer buffers, not a second copy of the entire image. Each checkpoint still carries a full description of the layer tree, so any point in that trail can be opened by filling in unchanged pixels from older checkpoints and, if needed, from the working file itself.

Autosave uses the same pattern in a separate cache, so automatic protection does not have to rewrite the file on disk. If a project is opened when newer checkpoints exist than the last full save, Lumi can offer them instead of silently discarding the more recent work. Recovered images open under a distinct name so a quick save cannot overwrite the original.

## A working format

`.lum` is for continuing a painting in Lumi. Flattened or compatibility formats are for publishing, delivery, and other applications. Because a project is a directory of many files, it should be archived if it needs to travel.

The working file stays rich and editable. Exports are how a finished or shared image leaves that structure.
