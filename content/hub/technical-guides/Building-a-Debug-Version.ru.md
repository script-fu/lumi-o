---
title: "Сборка отладочной версии"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---

В этом руководстве описан **локальный рабочий процесс отладки** Lumi с использованием скриптов в `build/lumi/scripts`.

Процесс предназначен для того, чтобы:

- использовать локальные артефакты сборки (без загрузки символов),
- проверять, что символы отладки действительно присутствуют,
- по умолчанию запускать GDB в автономном режиме символов.

## Предварительные требования

- Linux на базе Debian (базовая версия проекта: Debian 13)
- Дерево исходников Lumi уже клонировано

## Одноразовая настройка GDB (необязательно, но рекомендуется)

Установите инструменты GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Необязательная настройка локального логирования:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Примечание: локальные скрипты отладки Lumi по умолчанию отключают `debuginfod`, чтобы разрешение символов оставалось локальным и воспроизводимым.

## Быстрый старт

Из каталога скриптов:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Отладочная сборка + запуск (по умолчанию)

Используйте это для обычных сеансов отладки.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Эта команда:

1. собирает Lumi в режиме отладки,
2. проверяет символы отладки,
3. запускает Lumi под GDB.

### Только отладочная сборка (для последующего сеанса TTY/удалённого)

Используйте это, если хотите собрать сейчас, а запустить/отладить позже.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Использование TTY в Linux

TTY (текстовые консоли) часто — самый надёжный способ отладки жёстких зависаний.

- Переключитесь на TTY с помощью `Ctrl + Alt + F1` через `Ctrl + Alt + F6`
- Войдите через текстовую подсказку
- Вернитесь в графический сеанс с помощью `Ctrl + Alt + F7` (или `F2` в некоторых системах)

Почему это важно: если сеанс рабочего стола завис, TTY часто всё ещё отвечает, и вы можете подключить GDB, снять backtrace и получить полезные данные о сбое.

## Необязательно: удалённая отладка/TTY

При жёстких зависаниях или блокировке дисплея используйте `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Затем из TTY (рекомендуется при зависаниях) или другого терминала:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Для локального запуска GDB (без TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Примечание о производительности

Отладочные сборки по замыслу медленнее. Когда отладка завершена, вернитесь к более быстрой сборке:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
