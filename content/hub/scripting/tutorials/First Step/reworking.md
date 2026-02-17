---
title: Reworking
type: docs
weight: 7
---

This step fixes a subtle behaviour in the messaging example.

We were passing the string "Hello world!\n" as the message. The "\n" is a special kind of character, an "escape" character. It tells the output printing to start a newline. In Scheme, it will also force a message sent to the Status Bar to pop up as a GUI box.

The helper `send-to-gui` sends messages to a Lumi dialog box.

Update the message contents and destinations so the example behaves consistently.

Removing the escape character and extending the functions:
```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/path/to/your/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!"))
    (send-message message 'dialog-box)
    (send-message message 'status-bar)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

Replace magic numbers with the constants provided by Lumi (for example, `MESSAGE-BOX` and `ERROR-CONSOLE`).

Then split validation into two functions so it can be reused from multiple call sites.

- (is-valid-string?) To check a string is a string and not an empty string, within a send-to* function.
- (is-valid-output-display?) To check a given output destination is valid, in the send-message function.

Rework the library:

```scheme
(define (send-to-status-bar message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)
  (lumi-message message)
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-dialog-box message)
  (is-valid-string? message)
  (lumi-message-set-handler MESSAGE-BOX)

  ;; Append a newline to force a box the message
  (lumi-message (string-append message "\n"))
  (lumi-message-set-handler ERROR-CONSOLE))

(define (send-to-error-console message)
  (is-valid-string? message)
  (lumi-message-set-handler ERROR-CONSOLE)
  (lumi-message message))

(define (send-to-terminal message)
  (is-valid-string? message)
  (display message)
  (lumi-message-set-handler ERROR-CONSOLE))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (is-valid-output-display? output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'dialog-box) (send-to-dialog-box message))
    ((eq? output 'status-bar) (send-to-status-bar message))
    ((eq? output 'terminal) (send-to-terminal message))))

;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string")))

;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
      (error "Invalid output destination: " output)))
```

## Conclusion

By reworking our messaging library, we've made it more robust and reliable. We fixed the hidden issue with the newline character, introduced constants for better clarity, and expanded the functionality by adding support for the status bar and dialog box outputs. Additionally, separating the validation logic into smaller, focused functions ensures that our code is easier to maintain and extend in the future.

This rework demonstrates how small changes can enhance the overall structure and functionality of our library, paving the way for more flexibility and reusability as our project grows.
