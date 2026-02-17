---
title: Final Thoughts
type: docs
weight: 10
---

You now have a working procedure plug-in and a small helper library. This series introduced the core patterns you’ll use in most Lumi scripts:

- Functions: The building blocks of our plug-ins.
- Refactoring: Improving code structure while maintaining functionality.
- Code Libraries: Centralizing reusable functions to keep our code clean and modular.
- Validation Techniques: Ensuring that inputs are valid before executing our core logic.

You also saw the basics of using Git to track changes and keep a clean project structure. That workflow makes it easier to iterate without losing working versions.

Here’s the final version of our main plug-in code:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/your-name/code/path/to/repo/funky-library/messages.scm")
(load "/path/to/your/library/messages.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'status-bar)
    (send-message message 'dialog-box)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in example"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Scheme")
```

Library code:

```scheme
;; Purpose: Sends a message to the status bar, returns #t if successful
(define (send-to-status-bar message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the dialog box, returns #t if successful
(define (send-to-dialog-box message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler MESSAGE-BOX)
      (lumi-message (string-append message "\n"))
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Sends a message to the error console, returns #t if successful
(define (send-to-error-console message)
  (if (is-valid-string? message)
    (begin
      (lumi-message-set-handler ERROR-CONSOLE)
      (lumi-message message)
      #t)
    #f))

;; Purpose: Sends a message to the terminal, returns #t if successful
(define (send-to-terminal message)
  (if (is-valid-string? message)
    (begin
      (display message)
      (lumi-message-set-handler ERROR-CONSOLE)
      #t)
    #f))

;; Purpose: Dispatches a message to the appropriate output, returns #t if successful
(define (send-message message output)
  (if (is-valid-string-output? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))
    #f))

;; Purpose: Validates that the message is a non-empty string, returns #t if valid
(define (is-valid-string? message)
  (if (or (not (string? message)) (string=? message ""))
    (begin
      (error "Message must be a non-empty string")
      #f)
    #t))

;; Purpose: Validates that the output is a valid destination, returns #t if valid
(define (is-valid-string-output? output)
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (begin
      (error "Invalid output destination: " output)
      #f)
    #t))
```

## Conclusion

By refactoring the messaging helpers into a small library, the plug-in stays focused on intent and the library contains the implementation details. Validation and consistent message routing keep failures predictable.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

Next steps:

- Move reusable helpers into a dedicated library file.
- Keep plug-ins small and name procedures for what they do.
- Add validation at boundaries (inputs, file paths, menu options).

Keep the final result as two files in your plug-ins repo:

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`