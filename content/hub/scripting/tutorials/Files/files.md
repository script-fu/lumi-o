---
title: Files
type: docs
weight: 7
url: "hub/scripting/tutorials/files"
---

Working with files and directories is essential for Scheme development. Whether you're saving output, loading resources, or organizing your project structure, understanding file operations will make your scripts more robust and user-friendly.

This page covers common file and directory tasks: reading paths, creating directories, and collecting folder inputs via GUI parameters.

## User's Home Directory

Lumi is Linux-only, so the user's home directory comes from the `HOME` environment variable.

To get the user's home directory as a string:

```scheme
(getenv "HOME")
```

Example output:

```scheme
"/home/username"
```

## DIR-SEPARATOR

There is also the global variable `DIR-SEPARATOR`, which is the platform-specific path separator. In Lumi (Linux), it is always `/`.

```scheme
> DIR-SEPARATOR
"/"
```

## Getting a Directory Location

We can ask the user for a directory location in the Scheme dialogue for a plug-in.

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

The `SF-DIRNAME` provides a browser to a directory.

```scheme
(define (batch-process-file-system src-dir src-dir-fallback extension dst-dir dst-dir-fallback show-images process-fn export-fn)
  (let* ((validated-src-dir (validate-path-and-dir src-dir src-dir-fallback "Source"))
         (validated-dst-dir (validate-path-and-dir dst-dir dst-dir-fallback "Destination"))
         (files (discover-files validated-src-dir extension)))
    ;; ...
    ))
```

Here we validate the two directory inputs (source and destination) and fall back to defaults if the GUI paths are empty/invalid.

[/hub/scripting/plug-ins/batch-process/](/hub/scripting/plug-ins/batch-process/)

If you're interested in the implementation details, search the plug-in source for `validate-path-and-dir`.

## Making a Directory

Scheme provides the ```dir-make``` command to create a directory. This command takes a "/" separated path and creates a single directory with an optional parameter for the privileges. We do not give it platform-specific paths.

Usually we need to create multiple directories for a practical path. We can use a wrapper for ```dir-make``` to help us here.

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

Note: This function also uses the built-in ```file-exists?``` to skip unnecessary calls. It returns #t if the indicated file or directory exists, and #f if it does not exist or if it is not accessible to the requesting user.

## Constructing a Path

We also need to break down and rebuild paths in Scheme.

To split a path into parts, use ```strbreakup```:

### Linux Path Examples

```scheme
> (strbreakup (getenv "HOME") DIR-SEPARATOR)
("" "home" "username")

> (strbreakup "/this/path/" DIR-SEPARATOR)
("" "this" "path" "")
```

> Note: Leading and trailing slashes become empty string elements in the resulting list.

To rebuild a path, use ```string-append```:

### Linux Path Building

```scheme
> (string-append (getenv "HOME") DIR-SEPARATOR "myfolder" DIR-SEPARATOR "myfile.xcf")
"/home/username/myfolder/myfile.xcf"
```
```

