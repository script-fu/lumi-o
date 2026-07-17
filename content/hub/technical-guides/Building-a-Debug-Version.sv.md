---
title: "Bygga felsökningsversion"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Den här guiden beskriver **det lokala felsökningsarbetsflödet** för Lumi med skript i `build/lumi/scripts`.

Arbetsflödet är utformat för att:

- använda lokala byggartefakter (inga symbolnedladdningar krävs),
- verifiera att felsökningssymboler faktiskt finns,
- starta GDB med offline-symboläge som standard.

## Förutsättningar

- Debian-baserat Linux (projektets baslinje: Debian 13)
- Lumi-källträd redan klonat

## Engångsinställning av GDB (valfritt men rekommenderas)

Installera GDB-verktyg:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Valfri lokal loggningskonfiguration:

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

### Felsökningsbygge + start (standard)

Använd detta för normala felsökningssessioner.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Detta kommando:

1. bygger Lumi i felsökningsläge,
2. verifierar felsökningssymboler,
3. startar Lumi under GDB.

### Endast felsökningsbygge (för senare TTY/fjärrsession)

Använd detta när du vill bygga nu och starta/felsöka senare.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Använda TTY i Linux

TTY:er (textkonsoler) är ofta det mest pålitliga sättet att felsöka hårda frysningar.

- Byt till en TTY med `Ctrl + Alt + F1` till `Ctrl + Alt + F6`
- Logga in från textprompten
- Återgå till den grafiska sessionen med `Ctrl + Alt + F7` (eller `F2` på vissa system)

Varför detta är viktigt: om skrivbordssessionen har låst sig svarar en TTY ofta fortfarande, så att du kan koppla GDB, fånga en backtrace och samla in användbar kraschdata.

## Valfritt: fjärr-/TTY-felsökning

För hårda frysningar eller skärmlåsningar, använd `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Sedan från en TTY (rekommenderas vid frysningar) eller en annan terminal:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

För lokal GDB-start (utan TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Prestandaanmärkning

Felsökningsbyggen är avsiktligt långsammare. När du är klar med felsökningen, byt tillbaka till ett snabbare bygge:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
