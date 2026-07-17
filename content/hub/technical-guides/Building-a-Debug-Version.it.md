---
title: "Compilare una versione di debug"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Questa guida descrive il **flusso di lavoro di debug locale** di Lumi usando gli script in `build/lumi/scripts`.

Il flusso di lavoro è pensato per:

- usare artefatti di build locali (non sono richiesti download di simboli),
- verificare che i simboli di debug siano effettivamente presenti,
- avviare GDB con la modalità simboli offline attiva per impostazione predefinita.

## Prerequisiti

- Linux basato su Debian (riferimento del progetto: Debian 13)
- Albero sorgente di Lumi già clonato

## Configurazione GDB una tantum (facoltativa ma consigliata)

Installa gli strumenti GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Configurazione opzionale del logging locale:

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

### Build di debug e avvio (impostazione predefinita)

Usalo per le sessioni di debug normali.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Questo comando:

1. compila Lumi in modalità debug,
2. verifica i simboli di debug,
3. avvia Lumi sotto GDB.

### Solo build di debug (per una sessione TTY/remota successiva)

Usalo quando vuoi compilare ora e avviare o eseguire il debug in seguito.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Usare le TTY in Linux

Le TTY (console testuali) sono spesso il modo più affidabile per eseguire il debug di blocchi completi.

- Passa a una TTY con `Ctrl + Alt + F1` fino a `Ctrl + Alt + F6`
- Accedi dal prompt testuale
- Torna alla sessione grafica con `Ctrl + Alt + F7` (o `F2` su alcuni sistemi)

Perché è importante: se la sessione desktop si blocca, una TTY spesso risponde ancora, così puoi collegare GDB, acquisire un backtrace e recuperare dati utili sull'arresto anomalo.

## Facoltativo: debug remoto/TTY

Per blocchi completi o blocchi del display, usa `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Poi da una TTY (consigliato per scenari di blocco) o da un altro terminale:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Per un avvio locale di GDB (percorso senza TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Nota sulle prestazioni

Le build di debug sono più lente per progettazione. Quando hai finito di eseguire il debug, torna a una build più veloce:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
