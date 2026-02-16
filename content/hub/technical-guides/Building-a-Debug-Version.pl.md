---
title: "Tworzenie wersji debugującej"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
Ten przewodnik opisuje **lokalny przebieg debugowania** dla Lumi przy użyciu skryptów w `build/lumi/scripts`.

Przepływ pracy ma na celu:

- używaj lokalnych artefaktów kompilacji (nie jest wymagane pobieranie symboli),
- sprawdź, czy symbole debugowania są rzeczywiście obecne,
- domyślnie uruchamiaj GDB w trybie symboli offline.

## Warunki wstępne

- Linux oparty na Debianie (baza projektu: Debian 13)
- Drzewo źródeł Lumi zostało już sklonowane

## Jednorazowa konfiguracja GDB (opcjonalna, ale zalecana)

Zainstaluj narzędzia GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Opcjonalna konfiguracja lokalnego rejestrowania:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Uwaga: lokalne skrypty debugowania Lumi domyślnie wyłączają `debuginfod`, aby zachować lokalną i powtarzalną rozdzielczość symboli.

## Szybki start

Z katalogu skryptów:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Debuguj kompilację + uruchamianie (domyślnie)

Użyj tego do normalnych sesji debugowania.

```bash
bash lumi-debug-local.sh lumi-dev build
```

To polecenie:

1. buduje Lumi w trybie debugowania,
2. weryfikuje symbole debugowania,
3. uruchamia Lumi pod GDB.

### Tylko kompilacja debugowania (na późniejszą sesję TTY/zdalną)

Użyj tego, jeśli chcesz teraz skompilować i uruchomić/debugować później.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Używanie TTY w Linuksie

TTY (konsole tekstowe) są często najbardziej niezawodnym sposobem na debugowanie twardych zawieszeń.

- Przełącz na TTY za pomocą `Ctrl + Alt + F1` do `Ctrl + Alt + F6`
- Zaloguj się z podpowiedzi tekstowej
- Wróć do sesji graficznej z `Ctrl + Alt + F7` (lub `F2` w niektórych systemach)

Dlaczego to ma znaczenie: jeśli sesja pulpitu zostanie zatrzymana, TTY często nadal odpowiada, dzięki czemu można podłączyć GDB, przechwycić ślad wstecz i odzyskać przydatne dane o awarii.

## Opcjonalnie: debugowanie zdalne/TTY

W przypadku twardego zawieszania się lub zawieszania wyświetlania użyj `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Następnie z TTY (zalecane w przypadku scenariuszy zawieszenia) lub innego terminala:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

W przypadku lokalnego uruchomienia GDB (ścieżka inna niż TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Uwaga dotycząca wydajności

Kompilacje debugowania są z założenia wolniejsze. Po zakończeniu debugowania wróć do szybszej kompilacji:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```