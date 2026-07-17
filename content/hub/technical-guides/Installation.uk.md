---
title: "Встановлення"
type: docs
url: "hub/technical-guides/Installation"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: ff429321515ea8c3b77a6f1f0cfd2486c8042e168032b9b0bec97b497930e25e
---
Для початкового клонування нижче потрібен Git. Якщо Git ще не встановлено, спочатку встановіть його (Debian/Ubuntu: `sudo apt install git`) або дотримуйтеся інструкцій: [Git у Linux](/hub/technical-guides/Using-Git-on-Linux/)

## 1) Клонування Lumi (перше налаштування)

Створіть каталог для Lumi та склонуйте вихідний код за допомогою Git.

```bash
sudo apt install git

mkdir -p ~/code
cd ~/code

# Клонування через SSH (відповідає посібнику з Git вище)
git clone git@ssh.gitlab.gnome.org:pixelmixer/lumi-dev.git lumi-dev

# Або клонування через HTTPS (без налаштування SSH-ключа)
# git clone https://gitlab.gnome.org/pixelmixer/lumi-dev.git lumi-dev
```

## 2) Встановлення залежностей (перше налаштування)

```bash
cd ~/code/lumi-dev/build/lumi/scripts
sudo bash lumi-install-packages.sh
```

## 3) Збірка Lumi (перше налаштування)

Перша повна збірка з налаштуванням (вперше або після суттєвих змін):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope setup --dir lumi-dev
```

## 4) Запуск Lumi

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-launch-active.sh lumi-dev
```

## Додатково: перезбірка / компіляція

Звичайна перезбірка після змін коду:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev
```

Швидкий шлях лише з компіляцією:

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope compile --dir lumi-dev
```

Збірка одного інтегрованого компонента (замініть `babl` на `gegl` або `gtk3`):

```bash
cd ~/code/lumi-dev/build/lumi/scripts
bash lumi-build-script.sh --scope build --dir lumi-dev --component babl
```

## Додатково: типи збірок

За потреби використовуйте `--type`:

- `debug` – робочі процеси налагодження
- `debugoptimized` – збалансований варіант за замовчуванням для розробки
- `release` – найшвидший час виконання

Приклад:

```bash
bash lumi-build-script.sh --scope build --dir lumi-dev --type release
```
