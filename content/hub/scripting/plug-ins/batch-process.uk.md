---
title: "Пакетна обробка"
type: docs
translation_provenance: ai-reviewed
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
translation_lock: true
---
Практичний наскрізний приклад обробки багатьох файлів за один раз.

## Де знаходиться код

- [Переглянути джерело](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Де з’являється в Lumi

- **Файл → Пакетна обробка**

## Що демонструє

- Параметри `SF-DIRNAME` для каталогів джерела та призначення
- Перевірку шляхів GUI з резервними варіантами (`validate-path-and-dir`)
- Рекурсивне сканування каталогу та ітерацію
- Звіт про прогрес тривалих операцій
