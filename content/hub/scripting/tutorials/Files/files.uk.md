---
title: "Файли"
type: docs
weight: 7
translation_provenance: ai-reviewed
translation_source_sha256: a68dc9328daa1e5b96aee6bf0949a8454b7826df85bdae254502ad9a24864992
url: "hub/scripting/tutorials/files"
translation_lock: true
---
Робота з файлами та каталогами важлива для розробки на Scheme. Незалежно від того, чи зберігаєте ви результат, завантажуєте ресурси чи організовуєте структуру проєкту, розуміння файлових операцій робить скрипти надійнішими та зручнішими.

Ця сторінка охоплює типові завдання з файлами та каталогами: читання шляхів, створення каталогів і збір введення папок через параметри GUI.

## Домашній каталог користувача

Lumi працює лише на Linux, тому домашній каталог користувача береться зі змінної середовища `HOME`.

Щоб отримати домашній каталог як рядок:

```scheme
(getenv "HOME")
```

Приклад результату:

```scheme
"/home/username"
```

## DIR-SEPARATOR

Також існує глобальна змінна `DIR-SEPARATOR` — роздільник шляхів для платформи. У Lumi (Linux) це завжди `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Отримання розташування каталогу

Можна запитати у користувача каталог у діалозі Scheme для плагіна.

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

`SF-DIRNAME` надає браузер каталогів.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Тут перевіряються два каталоги (джерело та призначення) і за потреби використовуються значення за замовчуванням, якщо шляхи з GUI порожні або недійсні.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

Якщо цікавлять деталі реалізації, знайдіть у коді плагіна `validate-path-and-dir`.

## Створення каталогу

Scheme надає команду `dir-make` для створення каталогу. Вона приймає шлях, розділений `/`, і створює один каталог з необов’язковим параметром прав доступу. Не передавайте їй шляхи, специфічні для інших платформ.

Зазвичай потрібно створити кілька каталогів для повного шляху. Тут допоможе обгортка навколо `dir-make`.

```scheme
;; Призначення: обгортка для (dir-make), що створює шлях з платформного
;;              шляху. Завжди передає в dir-make роздільники у стилі Linux.
(define (make-dir-path path)
  (let* ((path-parts (strbreakup path DIR-SEPARATOR))
         (current-path (car path-parts))) ; Кореневий каталог
    ;; Створювати решту каталогів покроково
    (for-each
     (lambda (part)
       (set! current-path (string-append current-path "/" part)) ; Зібрати шлях
       (if (file-exists? current-path)
         (debug-message "Directory exists: " current-path)
         (if (dir-make current-path)
           (debug-message "Made directory: " current-path)
           (warning-message "Failed to make directory: " current-path))))
     (cdr path-parts))))
```

Примітка: ця функція також використовує вбудований `file-exists?`, щоб пропускати зайві виклики. Вона повертає `#t`, якщо файл або каталог існує, і `#f`, якщо ні або якщо він недоступний користувачу, що запитує.

## Побудова шляху

У Scheme також потрібно розбирати та збирати шляхи.

Щоб розділити шлях на частини, використовуйте `strbreakup`:

### Приклади шляхів Linux

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Примітка: початкові та кінцеві скісні риски стають порожніми рядковими елементами в результуючому списку.

Щоб зібрати шлях, використовуйте `string-append`:

### Побудова шляху Linux

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
