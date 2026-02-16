---
title: "Erstellen einer Debug-Version"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
Diese Anleitung beschreibt den **lokalen Debug-Workflow** für Lumi mithilfe von Skripten in `build/lumi/scripts`.

Der Workflow ist darauf ausgelegt:

- lokale Build-Artefakte verwenden (keine Symbol-Downloads erforderlich),
- Überprüfen Sie, ob Debug-Symbole tatsächlich vorhanden sind.
- Starten Sie GDB standardmäßig mit dem Offline-Symbolmodus.

## Voraussetzungen

- Debian-basiertes Linux (Projektbasis: Debian 13)
- Lumi-Quellbaum bereits geklont

## Einmalige GDB-Einrichtung (optional, aber empfohlen)

GDB-Tools installieren:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Optionale Einrichtung der lokalen Protokollierung:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Hinweis: Die lokalen Debug-Skripte von Lumi deaktivieren `debuginfod` standardmäßig, um die Symbolauflösung lokal und reproduzierbar zu halten.

## Schnellstart

Aus dem Skriptverzeichnis:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug Build + Launch (Standard)

Verwenden Sie dies für normale Debugging-Sitzungen.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Dieser Befehl:

1. baut Lumi im Debug-Modus auf,
2. überprüft Debug-Symbole,
3. startet Lumi unter GDB.

### Nur Debug-Build (für spätere TTY-/Remote-Sitzung)

Verwenden Sie dies, wenn Sie jetzt erstellen und später starten/debuggen möchten.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Verwendung von TTYs unter Linux

TTYs (Textkonsolen) sind häufig die zuverlässigste Methode zum Debuggen von Hard Freezes.

- Wechseln Sie zu einem TTY mit `Ctrl + Alt + F1` bis `Ctrl + Alt + F6`
- Melden Sie sich über die Texteingabeaufforderung an
- Zurück zur grafischen Sitzung mit `Ctrl + Alt + F7` (oder `F2` auf einigen Systemen)

Warum das wichtig ist: Wenn die Desktop-Sitzung ins Stocken gerät, antwortet ein TTY oft immer noch, sodass Sie GDB anhängen, einen Backtrace erfassen und nützliche Absturzdaten wiederherstellen können.

## Optional: Remote/TTY-Debugging

Für ein starkes Einfrieren oder Anzeigenabstürze verwenden Sie `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Dann von einem TTY (empfohlen für Einfrier-Szenarien) oder einem anderen Terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Für einen lokalen GDB-Start (Nicht-TTY-Pfad):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Leistungshinweis

Debug-Builds sind von Natur aus langsamer. Wenn Sie mit dem Debuggen fertig sind, wechseln Sie zurück zu einem schnelleren Build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```