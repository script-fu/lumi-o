---
title: "재작업"
type: docs
weight: 7
---
이 단계에서는 메시징 예제의 미묘한 동작을 수정합니다.

우리는 "Hello world!\n" 문자열을 메시지로 전달하고 있었습니다. "\n"은 특별한 종류의 문자, 즉 "이스케이프" 문자입니다. 출력 인쇄에 개행을 시작하도록 지시합니다. Scheme에서는 상태 표시줄에 전송된 메시지가 GUI 상자로 팝업되도록 강제합니다.

도우미 `send-to-gui`는 Lumi 대화 상자에 메시지를 보냅니다.

예제가 일관되게 작동하도록 메시지 내용과 대상을 업데이트합니다.

이스케이프 문자를 제거하고 기능을 확장합니다.
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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

매직 넘버를 Lumi에서 제공하는 상수(예: `MESSAGE-BOX` 및 `ERROR-CONSOLE`)로 바꿉니다.

그런 다음 유효성 검사를 두 개의 기능으로 분할하여 여러 호출 사이트에서 재사용할 수 있습니다.

- (is-valid-string?) 문자열을 확인하려면 send-to* 함수 내에서 빈 문자열이 아닌 문자열입니다.
- (is-valid-output-display?) send-message 기능에서 주어진 출력 대상이 유효한지 확인합니다.

라이브러리 재작업:

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

## 결론

메시징 라이브러리를 재작업하여 더욱 강력하고 안정적으로 만들었습니다. 개행 문자와 관련된 숨겨진 문제를 해결하고, 더 나은 명확성을 위해 상수를 도입했으며, 상태 표시줄 및 대화 상자 출력에 대한 지원을 추가하여 기능을 확장했습니다. 또한 유효성 검사 논리를 더 작고 집중된 기능으로 분리하면 향후 코드를 더 쉽게 유지 관리하고 확장할 수 있습니다.

이번 재작업은 작은 변화가 라이브러리의 전체 구조와 기능을 어떻게 향상시켜 프로젝트가 성장함에 따라 유연성과 재사용성을 높일 수 있는지를 보여줍니다.