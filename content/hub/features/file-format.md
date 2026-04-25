---
title: "File Format (.lum)"
type: docs
url: "hub/features/file-format"
---

Lumi's native file format is built for layered painting projects that need to remain reliable, inspectable, and recoverable over time. It is designed around the realities of illustration work: many layers, large canvases, embedded colour information, masks, effects, and recovery data.

Rather than treating a project as a single opaque blob, the format keeps the structure of the artwork visible to the application. This lets Lumi save, load, and recover large images more intelligently while preserving the organisation artists depend on.

## Open project structure

A Lumi project keeps the artwork's parts separate: image structure, layer content, masks, colour data, metadata, and recovery information each have a clear role. This makes the format easier to reason about and better suited to long-term access than a closed, monolithic container.

The goal is not only to store pixels, but to store the working state of an illustration. Layers remain layers, masks remain masks, and the file continues to reflect the way the artwork was built.

## Designed for large paintings

Large layered images can become heavy quickly. Lumi's format supports workflows where not every piece of image data needs to be pulled into memory at once. Projects can remain responsive by loading the parts of the image that are actually needed for viewing, editing, compositing, or export.

This approach helps complex files feel manageable, especially when an artwork contains many hidden, archived, experimental, or grouped layers.

## Saving without breaking flow

The file format supports both normal project saving and lightweight recovery-style snapshots. This gives artists a way to protect work frequently without turning every checkpoint into a full duplicate of the entire image.

Because recovery information belongs to the project structure, Lumi can keep useful history close to the artwork while still allowing automatic safety saves to live separately from the working file.

## Interchange and export

The native format is intended for ongoing Lumi work, while export formats are used for sharing flattened or compatibility-focused results. Import support helps bring existing artwork into Lumi's layered environment, and export support lets finished pieces leave the project format when they are ready for publishing, delivery, or further processing.

The distinction keeps the working file rich and editable while allowing final images to be produced in common external formats.

## Long-term reliability

In short, the `.lum` format is a practical container for serious painting work: open enough to inspect, structured enough to recover, and flexible enough to handle complex layered images economically.