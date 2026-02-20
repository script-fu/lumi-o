---
title: Utility Browser
type: docs
url: "hub/scripting/reference/utility-browser"
---

The Utility Browser lets you explore the built-in Scheme utility stdlib that ships with Lumi — without having to leave the app or dig through source files.

## Opening the Utility Browser

Go to **Help → Programming → Utility Browser**.

The window opens immediately; no plug-in needs to be loaded in advance.

## What It Shows

The browser lists every procedure, variable, and syntax form exported by the seven utility libraries that Lumi loads automatically at startup:

| Library | What it covers |
|---|---|
| `common.scm` | General-purpose helpers (string, number, list utilities) |
| `files.scm` | File and path helpers |
| `gegl.scm` | GEGL buffer and colour helpers |
| `images.scm` | Image-level helpers (`image-get-open-list`, etc.) |
| `layers.scm` | Layer and drawable helpers |
| `parasites.scm` | Parasite read/write helpers |
| `paths.scm` | Path and vector helpers |

All of these are available in any Scheme plug-in or in the Scheme Console.

## Searching and Filtering

- **Search box** — filters by name as you type (case-insensitive substring match).
- **Kind filter** — narrow results to `procedure`, `variable`, or `syntax`.

Clicking an entry shows its full docstring and the library it comes from.

## The Stdlib as Wrappers

The utility libraries are a practical application of the wrapping pattern: each helper gives a clear name to a low-level operation, hides boilerplate, and provides a single place to update if the underlying command changes. If you want to understand the design approach behind them, see the **[Wrapping]({{< ref "/hub/scripting/tutorials/Wrapping/wrapping" >}})** tutorial.

## Relationship to the Procedure Browser

The Utility Browser is separate from **Filters → Script-Fu → Console → Browse** (the Procedure Browser). The Procedure Browser lists PDB-registered procedures. The Utility Browser lists helper definitions that intentionally live *outside* the PDB — they are Scheme-only and have no C binding.
