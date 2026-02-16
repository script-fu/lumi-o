---
title: "Building a Debug Version"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---

This guide describes the **local debug workflow** for Lumi using scripts in `build/lumi/scripts`.

The workflow is designed to:

- use local build artifacts (no symbol downloads required),
- verify that debug symbols are actually present,
- launch GDB with offline symbol mode by default.

## Prerequisites

- Debian-based Linux (project baseline: Debian 13)
- Lumi source tree already cloned

## One-Time GDB Setup (Optional but Recommended)

Install GDB tools:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Optional local logging setup:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Note: Lumi's local debug scripts disable `debuginfod` by default to keep symbol resolution local and reproducible.

## Quick Start

From the scripts directory:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug Build + Launch (default)

Use this for normal debugging sessions.

```bash
bash lumi-debug-local.sh lumi-dev build
```

This command:

1. builds Lumi in debug mode,
2. verifies debug symbols,
3. launches Lumi under GDB.

### Debug Build Only (for later TTY/remote session)

Use this when you want to build now and launch/debug later.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Using TTYs in Linux

TTYs (text consoles) are often the most reliable way to debug hard freezes.

- Switch to a TTY with `Ctrl + Alt + F1` through `Ctrl + Alt + F6`
- Log in from the text prompt
- Return to the graphical session with `Ctrl + Alt + F7` (or `F2` on some systems)

Why this matters: if the desktop session is stalled, a TTY often still responds, so you can attach GDB, capture a backtrace, and recover useful crash data.

## Optional: Remote/TTY Debugging

For hard freezes or display lockups, use `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Then from a TTY (recommended for freeze scenarios) or another terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

For a local GDB launch (non-TTY path):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Performance Note

Debug builds are slower by design. When you are done debugging, switch back to a faster build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
