---
title: Batch Process
type: docs
url: "hub/scripting/plug-ins/batch-process"
---

A practical, end-to-end example for processing many files in one go.

## Where it lives

- [View the source](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Where it appears in Lumi

- **File → Batch Process**

## What it demonstrates

- `SF-DIRNAME` parameters for source/destination directories
- Validating GUI paths with fallbacks (`validate-path-and-dir`)
- Recursive directory scanning and iteration
- Progress reporting for long-running operations
