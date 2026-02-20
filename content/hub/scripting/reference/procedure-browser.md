---
title: Procedure Browser
type: docs
url: "hub/scripting/reference/procedure-browser"
---

The Procedure Browser is the primary reference tool for discovering the hundreds of functions available in Lumi's Procedural Database (PDB). Because every tool, filter, and script in Lumi must be registered in the PDB to be callable, this browser is effectively a complete PDB explorer.

## Opening the Procedure Browser

Go to **Help → Programming → Procedure Browser**.

You can also access it from the Scheme Console via **Browse**.

## What It Shows

The Procedure Browser can list all procedures currently registered in the PDB, regardless of their origin. It defaults to searching for "internal", to show the internally registered core procedures.

- **Internal Procedures**: Core functions for image manipulation, layer management, and tool control.
- **External Plug-ins**: Procedures provided by compiled C/C++ plug-ins or persistent extensions.

## Searching and Filtering

- **Search box**: Filters procedures by name, description, or author. Clearing the search field shows all available procedures.
- **Search Type**: The search dropdown allows you to filter by specific fields. If you set it to **by type** and search for "internal", the list will narrow down to only show the internally registered core procedures.
- **Detailed View**: Clicking a procedure shows its parameters, return values, author, date, and a description of what it does.

This is essential for finding the exact name and argument signature of a function you want to call from your script.
