---
title: "최종 생각"
type: docs
weight: 10
---
이제 작업 절차 플러그인과 작은 도우미 라이브러리가 생겼습니다. 이 시리즈에서는 대부분의 Lumi 스크립트에서 사용할 핵심 패턴을 소개했습니다.

- 기능: 플러그인의 구성 요소입니다.
- 리팩토링: 기능을 유지하면서 코드 구조를 개선합니다.
- 코드 라이브러리: 재사용 가능한 기능을 중앙 집중화하여 코드를 깔끔하고 모듈식으로 유지합니다.
- 검증 기법: 핵심 로직을 실행하기 전에 입력이 유효한지 확인합니다.

또한 Git을 사용하여 변경 사항을 추적하고 깔끔한 프로젝트 구조를 유지하는 기본 사항도 살펴보았습니다. 이 워크플로를 사용하면 작업 버전을 잃지 않고 쉽게 반복할 수 있습니다.

주요 플러그인 코드의 최종 버전은 다음과 같습니다.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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

도서관 코드:

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

## 결론

메시징 도우미를 작은 라이브러리로 리팩터링함으로써 플러그인은 의도에 계속 집중하고 라이브러리에는 구현 세부 정보가 포함됩니다. 검증 및 일관된 메시지 라우팅을 통해 오류를 예측 가능하게 유지합니다.

```scheme
(message "Hello world!")
(send-message message 'status-bar)
(send-message message 'dialog-box)
(send-message message 'error-console)
(send-message message 'terminal)
```

다음 단계:

- 재사용 가능한 도우미를 전용 라이브러리 파일로 이동합니다.
- 플러그인을 작게 유지하고 수행하는 작업에 대한 이름 절차를 지정하세요.
- 경계(입력, 파일 경로, 메뉴 옵션)에 유효성 검사를 추가합니다.

최종 결과를 플러그인 저장소에 두 개의 파일로 유지하세요.

- `hello-world/hello-world.scm`
- `funky-library/messages.scm`