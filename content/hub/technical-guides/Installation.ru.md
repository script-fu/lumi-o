---
title: "Установка"
type: docs
---
Вам понадобится Git для начального этапа клонирования, описанного ниже. Если Git еще не установлен, сначала установите его (Debian/Ubuntu: `sudo apt install git`) или следуйте инструкциям: [Using Git on Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Клонировать Люми (первая установка)

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

## 2) Установить зависимости (первая установка)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Сборка Lumi (первая установка)

Первая полная сборка установки (первый раз или после серьезных изменений):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Запустить Люми

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Необязательно: перестроить/компилировать

Обычная перестройка после изменения кода:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Путь только для быстрой компиляции:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Создайте единый интегрированный компонент (замените `babl` на `gegl` или `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Необязательно: типы сборки

Используйте `--type` при необходимости:

- `debug` – рабочие процессы отладки
- `debugoptimized` – сбалансированный по умолчанию для разработки
- `release` – самое быстрое время работы

Пример:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```