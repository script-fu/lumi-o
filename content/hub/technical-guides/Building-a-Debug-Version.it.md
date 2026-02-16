---
title: "Creazione di una versione di debug"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
Questa guida descrive il **flusso di lavoro di debug locale** per Lumi utilizzando gli script in `build/lumi/scripts`.

Il flusso di lavoro è progettato per:

- utilizzare artefatti di build locali (non è richiesto il download di simboli),
- verificare che i simboli di debug siano effettivamente presenti,
- avvia GDB con la modalità simboli offline per impostazione predefinita.

## Prerequisiti

- Linux basato su Debian (base del progetto: Debian 13)
- Albero sorgente Lumi già clonato

## Configurazione GDB una tantum (facoltativa ma consigliata)

Installa gli strumenti GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configurazione della registrazione locale opzionale:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Nota: gli script di debug locale di Lumi disabilitano `debuginfod` per impostazione predefinita per mantenere la risoluzione dei simboli locale e riproducibile.

## Avvio rapido

Dalla directory degli script:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debug build + avvio (impostazione predefinita)

Usalo per le normali sessioni di debug.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Questo comando:

1. costruisce Lumi in modalità debug,
2. verifica i simboli di debug,
3. avvia Lumi sotto GDB.

### Solo build di debug (per sessioni TTY/remote successive)

Utilizzalo quando desideri creare ora e avviare/debug in seguito.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Utilizzo dei TTY in Linux

I TTY (console di testo) sono spesso il modo più affidabile per eseguire il debug degli hard freeze.

- Passa a un TTY da `Ctrl + Alt + F1` a `Ctrl + Alt + F6`
- Accedi dal messaggio di testo
- Ritorna alla sessione grafica con `Ctrl + Alt + F7` (o `F2` su alcuni sistemi)

Perché è importante: se la sessione desktop è bloccata, un TTY spesso risponde ancora, quindi puoi collegare GDB, acquisire un backtrace e recuperare dati utili sugli arresti anomali.

## Opzionale: debug remoto/TTY

Per blocchi hardware o blocchi del display, utilizzare `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Quindi da un TTY (consigliato per scenari di blocco) o da un altro terminale:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Per un avvio GDB locale (percorso non TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota sulle prestazioni

Le build di debug sono più lente in base alla progettazione. Una volta terminato il debug, torna a una build più veloce:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```