---
title: "다시 리팩터링"
type: docs
weight: 5
---
도우미 라이브러리가 커질수록 한눈에 따라가기가 어려워집니다. 각 기능을 작고 단일 목적으로 유지하도록 다시 리팩터링합니다.

### 복잡성 해소

기능을 더 쉽게 따르고 유지 관리하려면 더 작고 집중된 기능으로 나누세요. 메시지 라우팅과 유효성 검사를 분리하는 것부터 시작하세요.

### 검증 함수 생성

`message` 및 `output` 인수의 유효성을 검사하는 함수의 일부를 가져와 별도의 함수로 이동할 수 있습니다. 이렇게 하면 핵심 `send-message` 함수가 유효성 검사에 대해 걱정할 필요가 없어 따라가기가 더 쉬워집니다.

```scheme
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

### 메시지 전송 단순화

이제 유효성 검사가 별도의 함수로 이동되었으므로 `send-message` 함수는 메시지 전송에만 집중할 수 있습니다. 메시지를 올바른 대상으로 전달하는 특정 작업만 처리하므로 훨씬 더 간단해집니다.

```scheme
(define (send-message message output)
  ;; Call the validation function before proceeding
  (validate-message message output)

  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 추가 분석: 각 출력 핸들러 분리

각 메시지 출력 유형(GUI, 오류 콘솔, 터미널)을 자체 기능으로 이동할 수 있습니다. 이를 통해 향후 테스트, 수정 및 잠재적인 확장이 더 쉬워집니다.

```scheme
(define (send-to-gui message)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  (display message))

(define (send-message message output)
  ;; Send to the appropriate output
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 각 전송 기능에서 유효성 검사 재사용

유효성 검사는 메시지와 출력이 모두 올바른지 확인하는 중요한 부분이므로 각 `send-*` 함수가 자체 유효성 검사를 수행하는 것이 좋습니다. 이렇게 하면 어떤 출력이 호출되든 항상 입력을 먼저 확인하게 됩니다.

```scheme
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))
```

메시지 보내기 기능에서 유효성 검사를 제거하고 책임을 각 개별 출력 기능으로 옮겼는지 확인하세요. 이러한 변경을 통해 각 대상(GUI, 오류 콘솔, 터미널)이 자체 유효성 검사를 처리하고 메시지 보내기 기능을 간소화하며 유효성 검사 논리를 필요한 위치에 더 가깝게 유지할 수 있습니다.

이 접근 방식은 메시지 전송 기능을 단순화하여 _디스패처_로 만드는 동시에 각 전송* 기능이 처리 전에 메시지의 유효성을 올바르게 검사하도록 보장할 수 있습니다.

유효성 검사를 각 send-to-* 함수로 이동하여 독립형 함수로 재사용할 수 있도록 만들었습니다. 이는 메시지 보내기 디스패처 기능에 의존하지 않고 gui로 보내기, 오류 콘솔로 보내기 또는 터미널로 보내기 기능을 직접 호출할 수 있음을 의미합니다. 이제 이러한 각 기능은 자체 로직을 완벽하게 처리하고 코드의 다른 부분이나 다른 플러그인에서 독립적으로 사용할 수 있으므로 코드가 더욱 모듈화되고 유연해집니다.

## 리팩토링의 이점

- **명확한 우려 사항 분리**: 이제 각 기능은 하나의 책임만 처리하므로 코드를 더 쉽게 이해할 수 있습니다.
- **확장성**: 새로운 출력 유형을 추가하는 것은 간단합니다. `send-to-file` 또는 `send-to-logger`과 같은 새 함수를 정의한 다음 `cond` 문에 사례를 추가하기만 하면 됩니다.
- **재사용성**: 각 출력 처리 기능은 프로젝트의 다른 곳에서 재사용하거나 여러 플러그인 간에 공유할 수 있습니다.
- **일관성**: 각 `send-to-*` 함수에서 유효성 검사 기능을 재사용하면 모든 출력의 유효성이 올바르게 검사되어 코드가 더욱 강력해집니다.

리팩터링된 라이브러리 버전:

```scheme
;; Purpose: Sends a message to the GUI dialog box
(define (send-to-gui message)
  ;; Validate the message before proceeding
  (validate-message message 'gui)
  (lumi-message-set-handler 0)
  (lumi-message message))

;; Purpose: Sends a message to the Error Console
(define (send-to-error-console message)
  ;; Validate the message before proceeding
  (validate-message message 'error-console)
  (lumi-message-set-handler 2)
  (lumi-message message))

;; Purpose: Sends a message to the terminal window
(define (send-to-terminal message)
  ;; Validate the message before proceeding
  (validate-message message 'terminal)
  (display message))

;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (cond
    ((eq? output 'error-console) (send-to-error-console message))
    ((eq? output 'gui) (send-to-gui message))
    ((eq? output 'terminal) (send-to-terminal message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

;; Purpose: Validates that the message is a non-empty string and the output is valid
(define (validate-message message output)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
      (error "Message must be a non-empty string"))

  ;; Check if the output is one of the expected destinations
  (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)))
```

그게 우리가 할 수 있는 전부인가요? 아니요! 아직 할 일이 더 있으니 계속 읽어주세요.