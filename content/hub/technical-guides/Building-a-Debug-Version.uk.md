---
title: "Збірка версії для налагодження"
type: docs
url: "hub/technical-guides/Building-a-Debug-Version"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: fecc781e73b4f30881c5150c958ae9b2df4164acd4cf86926186acb8e2021d5f
---
У цьому посібнику описано **локальний робочий процес налагодження** Lumi за допомогою скриптів у `build/lumi/scripts`.

Робочий процес призначений для того, щоб:

- використовувати локальні артефакти збірки (завантаження символів не потрібне),
- перевіряти наявність символів налагодження,
- за замовчуванням запускати GDB у локальному режимі символів.

## Передумови

- Linux на базі Debian (базова лінія проєкту: Debian 13)
- дерево вихідного коду Lumi уже склоновано

## Одноразове налаштування GDB (необов’язково, але рекомендовано)

Встановіть інструменти GDB:

```bash
sudo apt update
sudo apt install gdb gdbserver
```

Додаткове налаштування локального журналювання:

```bash
mkdir -p ~/code/gdb_logs
cat > ~/.gdbinit <<'EOF'
set logging file ~/code/gdb_logs/gdb_log.txt
set logging enabled on
set logging overwrite on
EOF
```

Примітка: локальні скрипти налагодження Lumi за замовчуванням вимикають `debuginfod`, щоб розв’язання символів залишалося локальним і відтворюваним.

## Швидкий старт

З каталогу скриптів:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
```

### Збірка для налагодження + запуск (за замовчуванням)

Використовуйте для звичайних сеансів налагодження.

```bash
bash lumi-debug-local.sh lumi-dev build
```

Ця команда:

1. збирає Lumi в режимі налагодження,
2. перевіряє символи налагодження,
3. запускає Lumi під GDB.

### Лише збірка для налагодження (для подальшого TTY/віддаленого сеансу)

Використовуйте, якщо хочете зібрати зараз, а запустити чи налагодити пізніше.

```bash
bash lumi-build-debug.sh lumi-dev build
```

## Використання TTY в Linux

TTY (текстові консолі) часто — найнадійніший спосіб налагодити жорстке зависання.

- Перейдіть на TTY клавішами `Ctrl + Alt + F1` … `Ctrl + Alt + F6`
- Увійдіть у текстовий сеанс
- Поверніться до графічного сеансу клавішами `Ctrl + Alt + F7` (або `F2` на деяких системах)

Чому це важливо: якщо графічний сеанс завис, TTY часто все ще відповідає — можна підключити GDB, отримати зворотний трейс і зібрати корисні дані про збій.

## Додатково: віддалене/TTY-налагодження

Для жорстких зависань або блокування дисплея використовуйте `gdbserver`:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash gdbserver.sh
```

Потім з TTY (рекомендовано для випадків зависання) або іншого терміналу:

```bash
gdb /home/mark/code/lumi-dev/bin/lumi-0.1
(gdb) target remote localhost:9999
(gdb) continue
```

Для локального запуску GDB (без TTY):

```bash
bash lumi-debug-launch.sh --repo lumi-dev
```

## Примітка щодо продуктивності

Збірки для налагодження за задумом повільніші. Після завершення налагодження поверніться до швидшої збірки:

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# Повне скидання release для всіх основних компонентів
bash lumi-debug-reset-release.sh lumi-dev

# Необов’язковий швидший локальний варіант
bash lumi-build-script.sh --scope build --dir lumi-dev --type debugoptimized
```
