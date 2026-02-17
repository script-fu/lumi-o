---
title: "확인"
type: docs
weight: 4
---
강력한 플러그인을 구축할 때 오용이나 예상치 못한 입력이 있는 경우에도 함수가 오류를 우아하게 처리하고 예상대로 작동하는지 확인하는 것이 중요합니다. 유효성 검사는 기능의 무결성을 보호하고 충돌이나 의도하지 않은 동작을 방지하는 데 도움이 됩니다.

입력을 올바르게 처리하는지 확인하기 위해 유효성 검사를 추가하여 `send-message` 함수를 개선할 수 있는 방법을 살펴보겠습니다.

### 입력 확인

메시지를 보내기 전에 `send-message` 함수에 전달된 `output` 인수가 유효한지 확인해야 합니다. 출력 대상이 예상 값(gui, 오류 콘솔 또는 터미널) 중 하나인지 확인하는 검사를 추가할 수 있습니다.

예:

```scheme
(define (send-message message output)
  ;; Validate the output argument
  (if (not (member output '(gui error-console terminal)))
    (error "Invalid output destination: " output)
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
         (display message))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

이 예에서는 `member`을 사용하여 `output` 인수가 유효한지 확인합니다. 그렇지 않은 경우 함수는 명확한 메시지와 함께 오류를 발생시켜 잘못된 값이 문제를 일으키는 것을 방지합니다.

### 빈 메시지 처리

`message` 인수가 유효한지 확인하는 것도 유용합니다. 예를 들어 빈 문자열이나 #f(false)가 메시지로 전달되면 함수는 이를 적절하게 처리해야 합니다.

빈 메시지 처리 예:

```scheme
(define (send-message message output)
  ;; Check if the message is empty
  (if (or (not message) (string=? message ""))
    (error "Message cannot be empty")
    (cond
      ((eq? output 'error-console)
         (lumi-message-set-handler 2)
         (lumi-message message))
      ((eq? output 'gui)
         (lumi-message-set-handler 0)
         (lumi-message message))
      ((eq? output 'terminal)
         (display message))))

  (lumi-message-set-handler 2))
```

이 접근 방식은 함수가 항상 유효한 입력을 받도록 보장하여 안정성을 향상하고 예상치 못한 동작을 방지합니다.

### 결합 검증 예시

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)

  ;; Validate the message and output arguments
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")
    (if (not (member output '(gui error-console terminal)))
      (error "Invalid output destination: " output)
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
           (display message)))))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

이 버전에서는:
- 이 함수는 `message`이 비어 있거나 유효하지 않은지 먼저 확인합니다. 메시지가 유효하면 `output`이 허용되는 값(`gui`, `error-console` 또는 `terminal`) 중 하나인지 확인하는 단계로 넘어갑니다.
- 두 검사가 모두 통과되면 메시지가 적절한 출력으로 전송됩니다. 그렇지 않으면 명확한 설명과 함께 오류 메시지가 표시됩니다.
- 메시지도 문자열인지 확인하기 위해 추가 검사가 수행됩니다.

이 결합된 유효성 검사 기능은 코드를 더욱 깔끔하게 유지하고 작업을 수행하기 전에 두 입력 모두 유효성을 검사하여 기능을 더욱 강력하게 만듭니다. 우리는 또한 디버그 메시징 시스템을 구축하고 있습니다. 때
코드가 실패하면 우리는 이유를 알게 됩니다. 우리가 직접 작성한 이유입니다.

```
Execution error for 'Hello loaded!':
Error: Message must be a non-empty string
```

```
Execution error for 'Hello loaded!':
Error: Invalid output destination:  gu
```