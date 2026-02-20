---
title: "包装"
type: docs
weight: 4
---
方案命令在较低级别上运行，这意味着即使是简单的任务也可能需要多个步骤。然而，这种粒度提供了灵活性，我们可以将命令捆绑到小的、可重用的函数中，从而完全满足我们的需要。环绕并不是一个非黑即白的概念，而是一个非黑即白的概念。它的范围可以从常用命令的简单别名到管理整个工作流程的更复杂的功能。有时，包装器只是一个提高可读性的便利函数，而在其他情况下，它会演变成封装多个操作的全功能实用程序。

### 为什么要包装函数？

包装函数有几个主要好处：

- **简化重复任务** – 不要重复低级命令，而是将它们包装在辅助函数中并重用它。
- **提高可读性** – 为我们的包装函数提供清晰、描述性的名称，使我们的代码更容易一目了然。
- **封装复杂性** – 我们可以将它们分解为更小的、结构良好的辅助函数，而不是处理长而神秘的命令列表、深层嵌套循环或复杂的消息语句。
- **增强可维护性** – 如果命令的核心功能发生变化，我们只需更新我们的包装函数一次，从而使我们的插件不受这些更改的细节影响。
- **鼓励代码重用** – 每个助手都成为库的一部分，使未来的脚本可以更快地编写和调试。

随着插件的增长，包装器可以帮助您保持核心逻辑的可读性并隔离重复的细节。

包装函数的另一个优点是将它们集成到语法突出显示器中，例如 Visual Studio Code。这提高了可读性和导航性，使脚本更加清晰。在使用自定义函数的插件中，任何绿色突出显示的函数都确认它已从我们的库中正确引用。

如果您维护自己的帮助程序库，请考虑将项目的函数名称添加到编辑器的语法突出显示中。它使导航和重构更快。

示例：

### 随机种子

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (msrg-rand))
```

虽然我们可以直接在代码中使用 ***msrg-rand***，但将其包装在名为 ***random-seed*** 的函数中可以提高可读性。通过为函数指定一个清晰且具有描述性的名称，可以更容易一目了然地理解其用途。

此外，将***随机种子***定义为独立函数使我们可以在插件中的任何位置使用它，同时将实现集中在单个位置。如果我们需要更改种子的生成方式，我们只需要更新此函数，而其余代码保持不变。

例如，如果我们决定切换到***随机***：

```scheme
;; Purpose: Returns a random int for seeding a filter
(define (random-seed)
  (random 1000))
```

函数名称保持不变，确保我们的脚本无需修改即可继续工作。这种方法使我们的代码灵活、可维护且易于阅读。

### JPEG 导出

Scheme 中的 JPEG 导出功能带有许多参数，可以对图像的保存方式进行精细控制。然而，在大多数情况下，我们只关心一些关键设置，例如文件名和质量。为了简化过程，我们可以包装该函数。

```scheme
;; Purpose: Saves an image as a JPEG with a specified quality
(define (file-jpg-save image file quality)
  (let ((export-file (if (has-substring? file ".jpg")
                         file
                         (string-append file ".jpg")))) ;; Avoid jpg.jpg
    (debug-message "Exporting: " export-file)
    (file-jpeg-export #:run-mode RUN-NONINTERACTIVE
                      #:image image
                      #:file export-file
                      #:options -1
                      #:quality (* 0.01 quality)
                      #:smoothing 0.0
                      #:optimize 1
                      #:progressive 1
                      #:cmyk 0
                      #:sub-sampling "sub-sampling-1x1"
                      #:baseline 1
                      #:restart 0
                      #:dct "integer")))
```

在此包装函数中，大多数导出选项都是硬编码的，仅公开我们可能调整的参数：文件名和质量。这种方法提高了可读性并使保存图像更简单。Additionally, if Lumi's exporter changes in the future, we only need to update this one function rather than modifying every script that exports a JPEG.

### Using the Wrapper

To export a JPEG in our plug-ins, we simply include the library and call our custom function:

```scheme
(file-jpg-save image "/home/mark/pictures/my-picture" 85)
```

This keeps our code clean, readable, and adaptable while allowing us to export JPEGs efficiently with minimal effort.

### Car Replacement

The ***car*** function can be cryptic and prone to scripting errors. It’s easy to mistakenly apply ***car*** to a vector or a non-list item, leading to unexpected behaviour. To make our code more robust and readable, we can wrap this functionality in a safer function.

```scheme
;; Purpose: Returns the first item of a list or vector.
;;          Warns if the input is invalid or empty.
(define (first-item collection)
  (cond
    ;; Handle non-empty lists
    ((and (list? collection) (not (null? collection)))
     (list-ref collection 0))
    ;; Handle non-empty vectors
    ((and (vector? collection) (> (vector-length collection) 0))
     (vector-ref collection 0))
    ;; Invalid or empty input
    (else
     (begin
       (warning-message "first-item: Expected a non-empty list or vector, but received: " collection)
       #f))))
```

This function safely retrieves the first item of a list or vector while providing helpful warnings when invalid or empty inputs are encountered. By using ***first-item*** instead of ***car***, we reduce the risk of accidental errors and improve the clarity of our scripts.

#### Why Use This Wrapper?

- **Prevents script crashes** – Avoids errors caused by applying ***car*** to non-lists.
- **Supports both lists and vectors** – Expands usability beyond just lists.
- **Provides meaningful warnings** – Helps debug unexpected input issues.
- **Improves readability** – The function name clearly conveys its purpose.

By encapsulating this logic in first-item, we make our plug-ins more robust and easier to maintain. Of course, this comes down to personal preference, you may be completely comfortable using car, caar, cadr, and similar Scheme functions directly.

### Wrapping a Wrapped Function

Wrapping a function that is already wrapped can further improve readability and maintainability. For example, when working with coordinate pairs like ***pixel-coords (list 100 200)***, we could use:

```scheme
(first-item pixel-coords)
```

to retrieve the ***x*** coordinate. However, while functional, this is not very expressive. Instead, we can wrap ***first-item*** in a more appropriate definition to make our intent clearer.

```scheme
;; Purpose: Return the x-coordinate, for readability
(define (x-coord pixel-coords)
  (first-item pixel-coords))

;; Purpose: Return the y-coordinate, for readability
(define (y-coord pixel-coords)
  (second-item pixel-coords))
```

### Why Use This Approach?

- **Enhances code clarity** – Instead of using generic list access functions, we explicitly define functions that describe their purpose.
- **Improves maintainability** – If our coordinate representation changes (e.g., using vectors instead of lists), we only need to update these small functions.
- **Encourages consistency** – Using ***x-coord*** and ***y-coord*** makes the script easier to read and understand at a glance.

Now, instead of writing in generic Scheme:

```scheme
(car pixel-coords) ;; Gets the x-coordinate
(cadr pixel-coords) ;; Gets the y-coordinate
```

We can write in _our_ Scheme:

```scheme
(x-coord pixel-coords)
(y-coord pixel-coords)
```

By wrapping low-level functions in meaningful names, we create a more intuitive way to work with data, reducing confusion and potential errors.

### Shipped Wrappers: the Utility Stdlib

Lumi ships a set of ready-made wrappers loaded automatically at startup, so they are available in any plug-in or in the Scheme Console without any `(load ...)` call. These libraries — `common.scm`, `files.scm`, `gegl.scm`, `images.scm`, `layers.scm`, `parasites.scm`, and `paths.scm` — are built on exactly the same principle as the examples above: they give clear names to low-level operations, hide repetitive boilerplate, and provide a single place to update if the underlying command changes.

例如，`images.scm` 提供 `image-get-open-list` 作为原始 PDB 调用的可读包装器，`files.scm` 公开路径构建帮助程序，否则需要重复的 `string-append` 链。

您可以浏览每个导出的名称，阅读其文档字符串，并在 **[Utility Browser](@@LUMI_TOKEN_21@@)**（帮助 → 编程 → 实用程序浏览器）中查看它来自哪个库。它是大规模包装的实际演示，也是构建您自己的帮助程序库时可以借用的有用模式来源。

### 结论

包装函数是简化Scheme开发的有效方法，使脚本更具可读性、可维护性和健壮性。通过封装复杂性并仅公开必要的细节，我们创建了一种更加结构化的插件编写方法。

这种方法的主要要点：

- **简化重复性任务** – 我们创建可重用的函数，而不是手动重复低级命令。
- **提高代码可读性** – 命名良好的包装器使脚本更易于理解。
- **封装复杂性** – 低级细节在包装器内部处理，保持主脚本干净。
- **增强可维护性** – 如果核心功能发生变化，我们只需要更新包装器，而不是每个依赖它的脚本。
- **鼓励重用和一致性** – 我们的个人函数库随着时间的推移而不断增长，使开发更快、更高效。

通过一致地使用函数包装，我们可以改变编写Scheme插件的方式，创建一个更加模块化和更具表现力的脚本环境。牢记这些原则，我们可以继续完善我们的方法，开发更高效、更定制的方案版本，以满足我们的特定需求。

后续步骤：识别脚本中的重复块并提取具有清晰名称的小助手。