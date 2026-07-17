---
title: "Budowanie wersji debug"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

Ten przewodnik opisuje **lokalny przepływ pracy debugowania** Lumi przy użyciu skryptów w `build/lumi/scripts`.

Przepływ pracy ma:

- korzystać z lokalnych artefaktów kompilacji (bez pobierania symboli),
- weryfikować, czy symbole debugowania są rzeczywiście obecne,
- domyślnie uruchamiać GDB w trybie symboli offline.

## Wymagania wstępne

- Linux oparty na Debianie (punkt odniesienia projektu: Debian 13)
- Drzewo źródeł Lumi już sklonowane

## Jednorazowa konfiguracja GDB (opcjonalna, ale zalecana)

Zainstaluj narzędzia GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Opcjonalna konfiguracja lokalnego logowania:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Uwaga: lokalne skrypty debugowania Lumi domyślnie wyłączają `debuginfod`, aby rozwiązywanie symboli pozostało lokalne i powtarzalne.

## Szybki start

Z katalogu skryptów:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Kompilacja debug + uruchomienie (domyślnie)

Użyj tego w normalnych sesjach debugowania.

```bash
bash lumi-debug-local.sh lumi-dev build
```

To polecenie:

1. buduje Lumi w trybie debug,
2. weryfikuje symbole debugowania,
3. uruchamia Lumi pod GDB.

### Tylko kompilacja debug (na późniejszą sesję TTY/zdalną)

Użyj tego, gdy chcesz teraz skompilować, a uruchomić/debugować później.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Korzystanie z TTY w Linuksie

TTY (konsola tekstowa) to często najpewniejszy sposób debugowania twardych zawieszeń.

- Przełącz się na TTY za pomocą `Ctrl + Alt + F1` do `Ctrl + Alt + F6`
- Zaloguj się z poziomu promptu tekstowego
- Wróć do sesji graficznej przez `Ctrl + Alt + F7` (lub `F2` w niektórych systemach)

Dlaczego to ma znaczenie: jeśli sesja pulpitu się zawiesi, TTY często nadal odpowiada, więc możesz podłączyć GDB, przechwycić backtrace i odzyskać użyteczne dane o awarii.

## Opcjonalnie: debugowanie zdalne/TTY

Przy twardych zawieszeniach lub blokadzie ekranu użyj `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Następnie z TTY (zalecane przy zawieszeniach) lub innego terminala:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Do lokalnego uruchomienia GDB (bez TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Uwaga o wydajności

Kompilacje debug są z założenia wolniejsze. Po zakończeniu debugowania wróć do szybszej kompilacji:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
