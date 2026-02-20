---
title: Plug-In Browser
type: docs
url: "hub/scripting/reference/plugin-browser"
---

The Plug-In Browser allows you to explore the menus system and see where specific plug-ins are installed.

## Opening the Plug-In Browser

Go to **Help → Programming → Plug-In Browser**.

## What It Shows

While the Procedure Browser focuses on the raw *functions* in the PDB, the Plug-In Browser is a subset view focused on discovery of the user interface. It specifically filters the PDB to show "things that look like menu-installed plug-ins."

Internally, this uses a query that only returns procedures that have both an associated file on disk and a registered menu path.

- **Menu Tree**: Shows a tree representation of the Lumi menu structure.
- **Plug-In Locations**: Helps you find where a newly installed plug-in has nested itself in the menus.
- **Metadata**: Shows information about the plug-in's author, version, and date.

## Usage

Use the Plug-In Browser when you know a feature exists but can't find it in the menus, or when you are designing your own plug-in and want to see where similar tools are located.
