---
title: "Een debugversie bouwen"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
Deze handleiding beschrijft de **lokale foutopsporingsworkflow** voor Lumi met behulp van scripts in `build/lumi/scripts`.

De werkstroom is ontworpen om:

- gebruik lokale build-artefacten (geen symbooldownloads vereist),
- controleer of debug-symbolen daadwerkelijk aanwezig zijn,
- start GDB standaard met de offline symboolmodus.

## Vereisten

- Op Debian gebaseerde Linux (projectbasislijn: Debian 13)
- Lumi-bronboom al gekloond

## Eenmalige GDB-installatie (optioneel maar aanbevolen)

GDB-tools installeren:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Optionele lokale log-instellingen:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Opmerking: de lokale foutopsporingsscripts van Lumi schakelen `debuginfod` standaard uit om de symboolresolutie lokaal en reproduceerbaar te houden.

## Snelle start

Vanuit de scriptmap:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Foutopsporing bij bouwen en starten (standaard)

Gebruik dit voor normale foutopsporingssessies.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Deze opdracht:

1. bouwt Lumi in debug-modus,
2. verifieert debug-symbolen,
3. lanceert Lumi onder GDB.

### Debug Build Only (voor latere TTY/externe sessie)

Gebruik dit als u nu wilt bouwen en later wilt starten/debuggen.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## TTY's gebruiken in Linux

TTY's (tekstconsoles) zijn vaak de meest betrouwbare manier om harde bevriezingen op te lossen.

- Schakel over naar een TTY met `Ctrl + Alt + F1` via `Ctrl + Alt + F6`
- Log in vanaf de tekstprompt
- Keer terug naar de grafische sessie met `Ctrl + Alt + F7` (of `F2` op sommige systemen)

Waarom dit belangrijk is: als de desktopsessie vastloopt, reageert een TTY vaak nog steeds, zodat u GDB kunt koppelen, een backtrace kunt vastleggen en nuttige crashgegevens kunt herstellen.

## Optioneel: foutopsporing op afstand/TTY

Voor harde bevriezingen of vastgelopen beeldschermen gebruikt u `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Vervolgens vanaf een TTY (aanbevolen voor bevriezingsscenario's) of een andere terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Voor een lokale GDB-lancering (niet-TTY-pad):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Prestatienota

Debug-builds zijn langzamer van opzet. Wanneer u klaar bent met het opsporen van fouten, schakelt u terug naar een snellere build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```