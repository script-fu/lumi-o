---
title: "Установка"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---

Для начального клонирования, описанного ниже, вам понадобится Git. Если Git ещё не установлен, сначала установите его (Debian/Ubuntu: `sudo apt install git`) или следуйте инструкции: [Использование Git в Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Клонировать Lumi (первая настройка)

Создайте каталог для Lumi и используйте Git для клонирования исходного кода.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Clone via SSH (matches the Git guide above)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Or clone via HTTPS (no SSH key setup)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Установить зависимости (первая настройка)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Собрать Lumi (первая настройка)

Первая полная сборка с настройкой (в первый раз или после крупных изменений):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Запустить Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Необязательно: пересборка / компиляция

Обычная пересборка после изменений в коде:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Быстрый путь только для компиляции:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Собрать один интегрированный компонент (замените `babl` на `gegl` или `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Необязательно: типы сборки

При необходимости используйте `--type`:

- `debug` – для отладочных сценариев
- `debugoptimized` – сбалансированный вариант по умолчанию для разработки
- `release` – максимальная скорость выполнения

Пример:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
