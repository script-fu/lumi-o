---
title: "Установка"
type: docs
url: "hub/technical-guides/Installation"
---
В этом руководстве используются текущие сценарии сборки Lumi:

`~/code/lumi-dev/build/lumi/scripts`

## 1) Установить зависимости (первая установка)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 2) Постройте Люми

```bash
cd ~/code/lumi-dev/build/lumi/scripts

# First full setup build (BABL + lumi-gtk3 + GEGL + Lumi)
bash lumi-build-script.sh --scope setup --dir lumi-dev

# Normal rebuild after code changes
bash lumi-build-script.sh --scope build --dir lumi-dev

# Quick compile-only path
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

## 3) Запустить Люми

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Типы сборки

Используйте `--type` при необходимости:

- `debug` – рабочие процессы отладки
- `debugoptimized` – сбалансированный по умолчанию для разработки
- `release` – самое быстрое время работы

Пример:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```