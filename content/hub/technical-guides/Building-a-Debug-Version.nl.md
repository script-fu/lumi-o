---
title: "Debugversie bouwen"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Deze gids beschrijft de **lokale debugworkflow** voor Lumi met scripts in `build/lumi/scripts`.

De workflow is ontworpen om:

- lokale build-artefacten te gebruiken (geen symbol downloads nodig),
- te controleren of debugsymbolen daadwerkelijk aanwezig zijn,
- GDB standaard te starten met offline symbol mode.

## Vereisten

- Debian-gebaseerde Linux (projectbaseline: Debian 13)
- Lumi-boom met broncode al gekloond

## Eenmalige GDB-setup (optioneel maar aanbevolen)

Installeer GDB-tools:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Optionele lokale logconfiguratie:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Opmerking: Lumis lokale debugscripts schakelen `debuginfod` standaard uit om symbol resolution lokaal en reproduceerbaar te houden.

## Snelle start

Vanuit de scriptsmap:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug build + starten (standaard)

Gebruik dit voor normale debugsessies.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Dit commando:

1. bouwt Lumi in debugmodus,
2. controleert debugsymbolen,
3. start Lumi onder GDB.

### Alleen debug build (voor later TTY/remote sessie)

Gebruik dit wanneer je nu wilt bouwen en later wilt starten/debuggen.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## TTY's gebruiken in Linux

TTY's (tekstconsoles) zijn vaak de meest betrouwbare manier om hard freezes te debuggen.

- Schakel naar een TTY met `Ctrl + Alt + F1` t/m `Ctrl + Alt + F6`
- Log in via de tekstprompt
- Keer terug naar de grafische sessie met `Ctrl + Alt + F7` (of `F2` op sommige systemen)

Waarom dit belangrijk is: als de desktopsessie vastloopt, reageert een TTY vaak nog wel, zodat je GDB kunt koppelen, een backtrace kunt vastleggen en nuttige crashdata kunt verzamelen.

## Optioneel: remote/TTY-debugging

Voor hard freezes of display lockups gebruik je `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Vervolgens vanaf een TTY (aanbevolen bij freezes) of een andere terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Voor een lokale GDB-start (zonder TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Prestatie-opmerking

Debug builds zijn bewust trager. Als je klaar bent met debuggen, schakel terug naar een snellere build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
