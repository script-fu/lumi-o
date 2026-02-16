---
title: "Создание отладочной версии"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
---
В этом руководстве описывается **рабочий процесс локальной отладки** для Lumi с использованием сценариев в `build/lumi/scripts`.

Рабочий процесс предназначен для:

- использовать локальные артефакты сборки (загрузка символов не требуется),
- убедитесь, что символы отладки действительно присутствуют,
- по умолчанию запускать GDB в автономном символьном режиме.

## Предварительные условия

- Linux на базе Debian (базовая версия проекта: Debian 13)
- Исходное дерево Lumi уже клонировано.

## Одноразовая установка GDB (необязательно, но рекомендуется)

Установите инструменты GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Дополнительная настройка локального ведения журнала:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Примечание. Локальные сценарии отладки Lumi по умолчанию отключают `debuginfod`, чтобы сохранить локальное и воспроизводимое разрешение символов.

## Быстрый старт

Из каталога скриптов:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Отладка сборки + запуск (по умолчанию)

Используйте это для обычных сеансов отладки.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Эта команда:

1. собирает Lumi в режиме отладки,
2. проверяет символы отладки,
3. запускает Lumi под GDB.

### Только отладка сборки (для более позднего сеанса TTY/удаленного сеанса)

Используйте это, если хотите выполнить сборку сейчас, а запустить/отладить позже.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Использование TTY в Linux

TTY (текстовые консоли) часто являются наиболее надежным способом устранения сильных зависаний.

- Переключитесь на телетайп с помощью `Ctrl + Alt + F1` через `Ctrl + Alt + F6`.
- Войдите в систему из текстовой подсказки.
- Вернитесь в графический сеанс с помощью `Ctrl + Alt + F7` (или `F2` в некоторых системах)

Почему это важно: если сеанс рабочего стола завис, TTY часто все еще отвечает, поэтому вы можете подключить GDB, записать обратную трассировку и восстановить полезные данные о сбоях.

## Необязательно: удаленная отладка/отладка TTY

В случае сильного зависания или блокировки дисплея используйте `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Затем из телетайпа (рекомендуется для сценариев зависания) или другого терминала:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Для локального запуска GDB (путь без TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Примечание по производительности

Отладочные сборки изначально медленнее. Когда вы закончите отладку, вернитесь к более быстрой сборке:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Full release reset of all major components
bash lumi-debug-reset-release.sh lumi-dev

# Optional faster local-only variant
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```