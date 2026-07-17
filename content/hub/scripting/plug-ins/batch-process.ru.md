---
title: "Пакетный процесс"
type: docs
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 141b5ee23e77ecfc8ef5e8112706cfaebc1a2a528518218296dffda5b9dee6d1
url: "hub/scripting/plug-ins/batch-process"
---
Практичный комплексный пример обработки множества файлов за один раз.

## Исходный код

- [Посмотреть исходный код](https://gitlab.gnome.org/pixelmixer/lumi-dev/-/blob/main/plug-ins/lumi/batch-process/batch-process.scm)

## Меню в Lumi

- **Файл → Пакетная обработка**

## Что демонстрирует

- параметры `SF-DIRNAME` для каталогов источника/назначения.
- Проверка путей графического интерфейса с резервными вариантами (`validate-path-and-dir`)
- Рекурсивное сканирование и итерация каталогов
- Отчеты о ходе выполнения длительных операций.
