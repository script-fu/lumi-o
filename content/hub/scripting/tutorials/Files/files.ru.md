---
title: "Файлы"
type: docs
weight: 7
---
Работа с файлами и каталогами важна для разработки Scheme. Независимо от того, сохраняете ли вы выходные данные, загружаете ресурсы или организуете структуру проекта, понимание файловых операций сделает ваши сценарии более надежными и удобными для пользователя.

На этой странице описаны общие задачи с файлами и каталогами: чтение путей, создание каталогов и сбор входных данных о папках с помощью параметров графического интерфейса.

## Домашний каталог пользователя

Lumi предназначен только для Linux, поэтому домашний каталог пользователя определяется переменной среды `HOME`.

Чтобы получить домашний каталог пользователя в виде строки:

```scheme
(getenv "HOME")
```

Пример вывода:

```scheme
"/home/username"
```

## НАПРАВЛ.-СЕПАРАТОР

Существует также глобальная переменная `DIR-SEPARATOR`, которая является разделителем путей для конкретной платформы. В Lumi (Linux) это всегда `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Получение местоположения каталога

Мы можем запросить у пользователя расположение каталога в диалоговом окне «Схема» для плагина.

```scheme
(scheme-register
  "scheme-batch-process"
  "Batch Process"
  "Iteratively open the source files, then process, export and close"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2025"
  ""
  SF-DIRNAME "Loca_tion of Source"       ""
  SF-DIRNAME "Location of _Destination"  ""
  SF-TOGGLE  "S_how Loaded Images"       0
  SF-TOGGLE  "Only Process Open I_mages" 0)
```

`SF-DIRNAME` предоставляет браузер для каталога.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Здесь мы проверяем два входных параметра каталога (источник и пункт назначения) и возвращаемся к значениям по умолчанию, если пути графического интерфейса пусты/недействительны.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Если вас интересуют подробности реализации, найдите исходный код плагина `validate-path-and-dir`.

## Создание каталога

В Scheme предусмотрена команда ```dir-make``` для создания каталога. Эта команда использует путь, разделенный знаком «/», и создает один каталог с необязательным параметром для привилегий. Мы не указываем пути для конкретной платформы.

Обычно нам нужно создать несколько каталогов для практического пути. Мы можем использовать оболочку для ```dir-make```, чтобы помочь нам здесь.

```scheme
;; Purpose: A wrapper for (dir-make) that creates a given path from a platform
;;          supplied path. Always emits Linux style separators for dir-make.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Root dir
    ;; Create the rest of the directories step-by-step
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; build the path
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Примечание. Эта функция также использует встроенный ```file-exists?``` для пропуска ненужных вызовов. Он возвращает #t, если указанный файл или каталог существует, и #f, если он не существует или недоступен запрашивающему пользователю.

## Построение пути

Нам также необходимо разбить и перестроить пути в Scheme.

Чтобы разделить путь на части, используйте ```strbreakup```:

### Примеры путей Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Примечание. В результирующем списке начальные и конечные косые черты становятся пустыми строковыми элементами.

Чтобы перестроить путь, используйте ```string-append```:

### Построение путей Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```