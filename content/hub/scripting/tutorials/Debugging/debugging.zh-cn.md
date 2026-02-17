---
title: "调试"
type: docs
weight: 5
---
在脚本编写中，没有任何函数是绝对正确的。当遇到意外的输入或条件时，即使是最可靠的命令也可能会失败。为了防止这种情况，我们可以实现自定义调试系统并采用防御性编程技术。通过用错误处理机制包装标准函数并提供信息反馈，我们可以使我们的脚本更加健壮并且更容易排除故障。

该策略的关键部分是使用全局调试标志来控制详细输出，使我们能够在需要时启用详细的调试信息，同时在正常执行期间保持输出干净。

## 全局调试标志

全局调试标志是控制脚本执行期间信息输出级别的简单而有效的方法。启用后，它会提供详细的调试消息，这对于跟踪问题非常有用。禁用后，它可以保持输出简洁以供生产使用。

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

默认情况下，调试是关闭的。要在开发期间启用详细输出，只需将标志设置为`#t`：

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

我们还可以使用辅助函数临时启用或禁用特定代码部分的调试。

### 本地调试控制

为了更好地控制，我们可以使用辅助函数在脚本的特定部分打开或关闭调试。

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

这使我们能够动态控制调试：

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## 调试消息系统

为了有效地处理Scheme中的调试输出，我们使用了涉及多个辅助函数的结构化方法。这些功能确保调试和警告消息清晰、可读且可维护。

### 调试消息系统概述

我们的调试消息系统由以下组件组成：

1. `debug-message` – 启用调试时显示调试消息。
2. `serialize-item` – 将各种Scheme 数据类型转换为字符串表示形式。
3. `concat` – 将多个项目连接成一个字符串。
4. `list->string` – 将列表格式化为可读字符串。
5. `message` – 在 Lumi 的消息控制台中显示输出。
6. `warning-message` – 启用警告时显示警告消息。

每个函数都在格式化和显示结构化消息方面发挥作用。

---

### 调试消息功能

`debug-message` 函数是显示调试输出的核心方法。它确保仅在启用调试时显示消息。

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug` 条件确保仅在启用调试时才显示消息。
- 为了清楚起见，消息以 `"> "` 为前缀。
- 该函数使用`concat`来格式化消息内容。
- 最后，它调用 `message` 将输出发送到 Lumi 的消息控制台。

用法示例：

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

启用调试后，输出可能是：

```scheme
> item: background-layer has tree position : 3
```

### 序列化调试消息数据

消息可能包含不同的数据类型，例如列表、向量和数字。为了确保它们的格式正确，我们使用`serialize-item`。

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

用法示例：

```scheme
(serialize-item '(1 2 3))
```

输出：

```scheme
list:
1
2
3
```

### 消息串联

要将多个消息组件合并为单个字符串，我们使用`concat`。

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

用法示例：

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### 将列表格式化为字符串

`list->string` 函数将列表转换为格式化字符串。

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### 警告消息`warning-message` 函数的工作方式与 `debug-message` 类似，但即使禁用调试，它也会显示警告。

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- 确保仅在启用警告时显示消息（`warning` 标志在`common.scm` 中设置为`#t`）。
- 调用`concat`来格式化消息内容。
- 使用`message` 将输出发送到Lumi。

## 增强标准功能

一旦调试系统到位，我们就可以通过合并详细消息来增强我们的函数库。这提供了对项目状态、变量值和函数调用的深入了解。

一个常见的示例是`item-is-valid?`，它包装`lumi-item-id-is-valid`以返回`#t`或`#f`。如果返回`#f`，我们可以在调用代码中触发`warning-message`，如果输入不是数字，我们可以在函数中发出警告。

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## 实际使用

在开发Scheme插件时，以这种方式包装函数可以显着减少调试时间并确保代码的健壮性、可维护性。有了我们的调试系统，我们只需轻按一下开关就可以在错误控制台中生成结构化的调试流。

在此调试流中，函数调用标有星号 (*)，从而更容易跟踪脚本执行和查明故障，特别是在复杂的插件中。这种可见性有助于我们了解操作流程并有效地诊断意外行为。

我们的消息函数的包装器使用 `*`

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

`call` 实际使用的示例：

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

作为插件执行的调试流示例：

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

这种结构化日志提供了函数调用和数据更改的清晰时间表，使调试和性能分析变得更加容易。

## 结论

通过实施结构化调试系统，我们创建了更安全、更易于维护的脚本，可以实时洞察其执行情况。

### 要点

- **控制详细程度** – 使用全局调试标志来管理输出级别。
- **提供清晰的反馈** – 用信息丰富的调试消息包装标准功能。
- **增强稳健性** – 妥善处理意外输入以防止错误。
- **简化故障排除** – 结构化调试消息使诊断和修复问题变得更加容易。

通过这种方法，我们的脚本在处理数据时可以有效地“自我解释”，从而减少挫败感并提高工作流程效率。调试成为一种主动的工具，而不是被动的苦差事，使我们的脚本编写过程变得更加顺利和更有价值。