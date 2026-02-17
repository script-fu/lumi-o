---
title: "文件"
type: docs
weight: 7
---
使用文件和目录对于Scheme 开发至关重要。无论您是保存输出、加载资源还是组织项目结构，了解文件操作都将使您的脚本更加健壮和用户友好。

本页涵盖常见的文件和目录任务：读取路径、创建目录以及通过 GUI 参数收集文件夹输入。

## 用户的主目录

Lumi 仅适用于 Linux，因此用户的主目录来自 `HOME` 环境变量。

获取字符串形式的用户主目录：

```scheme
(getenv "HOME")
```

输出示例：

```scheme
"/home/username"
```

## 目录分隔符

还有全局变量`DIR-SEPARATOR`，它是特定于平台的路径分隔符。在 Lumi (Linux) 中，它始终是`/`。

```scheme
> DIR-SEPARATOR
"/"
```

## 获取目录位置

我们可以在方案对话框中询问用户插件的目录位置。

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

`SF-DIRNAME` 提供目录浏览器。

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

在这里，我们验证两个目录输入（源和目标），如果 GUI 路径为空/无效，则回退到默认值。

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

如果您对实现细节感兴趣，请搜索插件源`validate-path-and-dir`。

## 制作目录

Scheme 提供```dir-make``` 命令来创建目录。该命令采用“/”分隔路径并创建一个带有可选权限参数的目录。我们不给它特定于平台的路径。

通常我们需要为实际路径创建多个目录。我们可以使用 ```dir-make``` 的包装器来帮助我们。

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

注意：该函数还使用内置的```file-exists?```来跳过不必要的调用。如果指定的文件或目录存在，则返回#t；如果不存在或者请求用户无法访问它，则返回#f。

## 构建路径

我们还需要对Scheme中的路径进行分解和重建。

要将路径拆分为多个部分，请使用 ```strbreakup```：

### Linux 路径示例

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> 注意：前导斜杠和尾随斜杠将成为结果列表中的空字符串元素。

要重建路径，请使用```string-append```：

### Linux 路径构建

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
````