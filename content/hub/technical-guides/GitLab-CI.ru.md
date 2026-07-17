---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/GitLab-CI"
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 9917cebc417adeeae24d91b05b919b679a397d5db652cf4442d4330c0f8eeea5
---

Непрерывная интеграция (CI) — это способ автоматически тестировать, собирать и проверять код при каждом внесении изменений.

**GitLab** предоставляет встроенные возможности CI/CD через файл `.gitlab-ci.yml`. Этот файл, размещённый в корне репозитория, указывает GitLab, как собирать и тестировать проект. Он определяет stages и scripts, которые выполняются в чистой среде при каждой отправке изменений.

В этом документе описано, как работает CI/CD pipeline GitLab в Lumi, включая роль файла `.gitlab-ci.yml`, shell-скриптов и внешних инструментов, таких как Meson и Ninja.

Подробную техническую документацию процесса CI-сборки Lumi см. в [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) в репозитории.

## Основы GitLab CI/CD

CI управляется файлом с именем `.gitlab-ci.yml`. Этот файл определяет:

- **Stages**: упорядоченные группы jobs (например, `build-this`, `build-that`, `package-up`)
- **Jobs**: отдельные задачи, выполняемые в каждом stage
- **Scripts**: shell-команды, выполняемые для каждого job
- **Runners**: компьютеры, которые GitLab использует для выполнения jobs, определённых в pipeline

В Lumi stages pipeline:

- `dependencies`
- `build lumi`
- `appimage`

## Сборки на основе контейнеров

Pipeline Lumi использует контейнеризацию для согласованных сборок:

1. **Создание контейнера сборки**: на первом stage Buildah создаёт Docker-образ со всеми зависимостями
2. **Использование контейнера**: последующие stages выполняются внутри этого контейнера, обеспечивая согласованную среду
3. **Воспроизводимые сборки**: изоляция контейнера гарантирует одинаковые результаты на разных runners

Такой подход обеспечивает одинаковую работу сборок на любом runner GitLab и предоставляет контролируемую среду для сложных процессов сборки.

### Интегрированные источники зависимостей

CI-образ зависимостей Lumi собирает форкнутый стек из **интегрированных источников в репозитории** (без внешних клонов):

- `lumi-babl/` (BABL)
- `lumi-gegl/` (GEGL)
- `lumi-gtk3/` (GTK3)

Эти каталоги копируются в контекст сборки контейнера и компилируются в префикс зависимостей (обычно `/opt/lumi-deps`). Это сохраняет воспроизводимость CI и гарантирует, что сборка AppImage использует тот же источник истины, что и локальная разработка.

## Роль shell-скриптов

Jobs в `.gitlab-ci.yml` обычно вызывают shell-команды напрямую. Сложные операции часто выносятся в отдельные скрипты, хранящиеся в репозитории.

CI Lumi использует модульные shell-скрипты для организации логики сборки:

**Пример вызова скрипта:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Преимущества такого подхода:**
- **Чистый YAML**: файл `.gitlab-ci.yml` остаётся сосредоточенным на структуре jobs
- **Удобство сопровождения**: сложную логику проще отлаживать и изменять в shell-скриптах
- **Повторное использование**: скрипты можно использовать в разных контекстах или средах
- **Модульность**: разные аспекты сборки можно разделить на специализированные скрипты

Это сохраняет конфигурацию CI чистой и при этом позволяет реализовывать сложные процессы сборки.

## Интеграция с системами сборки

Lumi использует **Meson** и **Ninja** для подготовки и последующей сборки кода.

Например:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Здесь:

- `meson setup` подготавливает каталог сборки и генерирует `build.ninja`
- `ninja` выполняет команды сборки согласно определению

## Структура системы сборки Meson

Система сборки **Meson** использует корневой файл `meson.build`, размещённый в корневом каталоге проекта. Этот файл определяет конфигурацию сборки верхнего уровня и точку входа в процесс сборки.

- Корневой `meson.build` обычно находится в том же каталоге, что и `.gitlab-ci.yml`
- Оттуда он **рекурсивно каскадирует** в подкаталоги, каждый из которых может иметь свой файл `meson.build`
- Эти файлы подкаталогов определяют targets, исходники, зависимости и инструкции сборки, относящиеся к данному каталогу

## Переменные окружения

Ключевые переменные в pipeline Lumi включают:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Переменные, специфичные для job:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

Эти переменные управляют поведением сборки и обеспечивают согласованность между разными stages и runners.

## Пример структуры

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

В этой структуре:

- Корневой файл `meson.build` настраивает общую среду сборки
- Файлы `meson.build` в подкаталогах обрабатывают детали компиляции отдельных компонентов или модулей
- Такая иерархическая организация сохраняет логику сборки модульной и удобной для сопровождения

## Artifacts между stages

Artifacts — это файлы, созданные jobs, которые нужны на последующих stages:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Stages pipeline и зависимости

Pipeline Lumi состоит из трёх основных stages:

1. **Dependencies**: создаёт контейнеризированную среду сборки со всеми необходимыми инструментами и библиотеками
2. **Build Lumi**: компилирует Lumi с помощью Meson и Ninja в подготовленной среде
3. **AppImage**: упаковывает собранное приложение в распространяемый формат AppImage

**Зависимости между stages:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Каждый stage выполняется только после успешного завершения зависимостей, что обеспечивает правильный порядок сборки и доступность artifacts.

## Текущие имена jobs

В текущем `.gitlab-ci.yml` Lumi определены следующие имена jobs:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Итог

- `.gitlab-ci.yml` определяет структуру и логику pipeline
- Jobs содержат shell-команды или внешние скрипты
- Инструменты вроде Meson и Ninja используются внутри jobs как часть процесса сборки

Lumi использует GitLab CI для автоматической сборки AppImage для платформ на базе Debian. Pipeline собирает зависимости, компилирует Lumi и затем упаковывает AppImage.

Для деталей на уровне исходного кода используйте:

- `.gitlab-ci.yml` в корне репозитория Lumi
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

Полные технические сведения о процессе CI-сборки Lumi, включая настройку среды, архитектуру скриптов и устранение неполадок, см. в [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
