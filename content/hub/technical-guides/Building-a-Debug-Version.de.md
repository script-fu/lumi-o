---
title: "Erstellen einer Debug-Version"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Diese Anleitung beschreibt den **lokalen Debug-Workflow** für Lumi mithilfe von Skripten in `build/lumi/scripts`.

Der Workflow ist darauf ausgelegt:

- lokale Build-Artefakte zu verwenden (keine Symbol-Downloads erforderlich),
- zu überprüfen, ob Debug-Symbole tatsächlich vorhanden sind,
- GDB standardmäßig mit dem Offline-Symbolmodus zu starten.

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

### Debug-Build + Start (Standard)

Verwenden Sie dies für normale Debugging-Sitzungen.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Dieser Befehl:

1. baut Lumi im Debug-Modus,
2. überprüft Debug-Symbole,
3. startet Lumi unter GDB.

### Nur Debug-Build (für spätere TTY-/Remote-Sitzung)

Verwenden Sie dies, wenn Sie jetzt erstellen und später starten/debuggen möchten.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Verwendung von TTYs unter Linux

TTYs (Textkonsolen) sind häufig die zuverlässigste Methode zum Debuggen harter Einfrierer.

- Wechseln Sie zu einem TTY mit `Ctrl + Alt + F1` bis `Ctrl + Alt + F6`
- Melden Sie sich über die Texteingabeaufforderung an
- Kehren Sie zur grafischen Sitzung mit `Ctrl + Alt + F7` zurück (oder `F2` auf einigen Systemen)

Warum das wichtig ist: Wenn die Desktop-Sitzung hängt, antwortet ein TTY oft noch, sodass Sie GDB anhängen, einen Backtrace erfassen und nützliche Absturzdaten sichern können.

## Optional: Remote-/TTY-Debugging

Bei harten Einfrierern oder Display-Blockaden verwenden Sie `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Dann von einem TTY (empfohlen bei Einfrier-Szenarien) oder einem anderen Terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Für einen lokalen GDB-Start (ohne TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Leistungshinweis

Debug-Builds sind von Natur aus langsamer. Wenn Sie mit dem Debuggen fertig sind, wechseln Sie zurück zu einem schnelleren Build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Vollständiger Release-Reset aller Hauptkomponenten
bash lumi-debug-reset-release.sh lumi-dev

# Optionale schnellere lokale Variante
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
