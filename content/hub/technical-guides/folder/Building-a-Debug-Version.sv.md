---
title: "Bygga en felsökningsversion"
type: docs
url: "hub/technical-guides/folder/Building-a-Debug-Version"
---
Den här guiden beskriver **lokalt felsökningsarbetsflöde** för Lumi med skript i `build/lumi/scripts`.

Arbetsflödet är utformat för att:

- använd lokala byggartefakter (inga symbolnedladdningar krävs),
- verifiera att felsökningssymboler faktiskt finns,
- Starta GDB med offline-symbolläge som standard.

## Förutsättningar

- Debian-baserat Linux (projektets baslinje: Debian 13)
- Lumi-källträdet redan klonat

## Engångsinställningar för GDB (valfritt men rekommenderas)

Installera GDB-verktyg:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Valfri lokal loggningsinställning:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Obs: Lumis lokala felsökningsskript inaktiverar `debuginfod` som standard för att hålla symbolupplösningen lokal och reproducerbar.

## Snabbstart

Från skriptkatalogen:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug Build + Launch (standard)

Använd detta för normala felsökningssessioner.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Detta kommando:

1. bygger Lumi i felsökningsläge,
2. verifierar felsökningssymboler,
3. lanserar Lumi under GDB.

### Debug Build Only (för senare TTY/fjärrsession)

Använd detta när du vill bygga nu och starta/felsöka senare.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Använda TTY i Linux

TTY:er (textkonsoler) är ofta det mest pålitliga sättet att felsöka hårda frysningar.

- Byt till en TTY med `Ctrl + Alt + F1` till `Ctrl + Alt + F6`
- Logga in från textprompten
- Återgå till den grafiska sessionen med `Ctrl + Alt + F7` (eller `F2` på vissa system)

Varför detta är viktigt: om skrivbordssessionen har avstannat svarar en TTY ofta fortfarande, så att du kan bifoga GDB, fånga en bakåtspårning och återställa användbar kraschdata.

## Valfritt: Fjärr/TTY-felsökning

För hårda frysningar eller skärmlåsning, använd `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Sedan från en TTY (rekommenderas för frysningsscenarier) eller en annan terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

För en lokal GDB-lansering (icke-TTY-sökväg):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Anmärkning om prestanda

Felsökningsbyggen är långsammare till sin design. När du är klar med felsökningen byter du tillbaka till en snabbare build:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```