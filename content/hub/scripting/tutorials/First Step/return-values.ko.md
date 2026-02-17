---
title: "반환 값"
type: docs
weight: 8
---
반환 값은 추가 상태 없이 흐름을 제어할 수 있게 해주기 때문에 중요합니다. Scheme에서는 마지막으로 평가된 표현식이 반환 값이 됩니다.

이 페이지에서는 메시징 예제의 유효성 검사 도우미를 사용하여 명시적인 반환 값이 코드를 더 쉽게 구성하는 방법을 보여줍니다.

### 반환 값이란 무엇입니까?

Scheme에서 함수의 반환 값은 함수가 평가하는 마지막 표현식에 의해 결정됩니다. 이는 함수의 마지막 코드 줄이 평가하는 모든 것이 함수의 결과로 반환된다는 것을 의미합니다. 값이 명시적으로 반환되지 않으면 함수는 `#f`(false) 또는 `undefined`을 반환합니다.

유효성 검사 함수(is-valid-string?)를 다시 살펴보겠습니다.

```scheme
;; Purpose: Validates that the message is a non-empty string
(define (is-valid-string? message)
  ;; Check if the message is a non-empty string
  (if (or (not (string? message)) (string=? message ""))
    (error "Message must be a non-empty string")))
```

이 함수에서는 메시지가 유효하지 않으면 오류가 발생합니다. 그러나 메시지가 유효한 경우 명시적인 반환 값이 제공되지 않으며 함수는 기본적으로 `#f`을 반환합니다.

### 반환 값을 명시적으로 만들기

반환 값을 더 명확하게 만들어 이를 개선할 수 있습니다. 예를 들어 메시지가 유효한 경우 `#t`(true)를 반환할 수 있습니다.

```scheme
;; Purpose: Validates that the message is sent to a valid output
(define (is-valid-output-display? output)
  ;; Check if the output is one of the expected display destinations
  (if (not (member output '(dialog-box status-bar error-console terminal)))
    (error "Invalid output destination: " output)
    #t))
```

이 버전에서는 메시지가 유효할 때 함수가 `#t`을 반환하여 명확한 결과를 제공합니다. 이를 통해 부울 결과가 필요한 다른 상황에서 함수를 보다 유연하게 사용할 수 있습니다.

### 반환 값을 효과적으로 사용하기

함수가 무엇을 반환하는지 결정함으로써 함수를 더 예측 가능하고 유용하게 만들 수 있습니다. `#t`, `#f`과 같은 값을 반환하거나 특정 결과를 반환하면 함수가 나머지 코드와 상호 작용하는 방식을 더 효과적으로 제어할 수 있습니다. 예를 들어, 반환 값을 사용하여 호출 함수에서 추가 결정을 내리거나 이를 다른 함수에 인수로 전달할 수 있습니다.

다음은 반환 값을 사용하여 논리 흐름을 제어하는 간단한 예입니다.

```scheme
;; Purpose: Dispatches a message to the appropriate output destination
(define (send-message message output)
  (if (is-valid-output-display? output)
    (cond
      ((eq? output 'error-console) (send-to-error-console message))
      ((eq? output 'dialog-box) (send-to-dialog-box message))
      ((eq? output 'status-bar) (send-to-status-bar message))
      ((eq? output 'terminal) (send-to-terminal message)))))
```

이 경우 (send-message)는 (is-valid-output-display?)의 반환 값에 따라 계속할지 여부를 결정합니다.
첫 번째 테스트가 실패하면 조건문 `cond`을 건너뜁니다. 또한 유효한 출력 표시인 경우 상당히 자연스러운 방식으로 읽는 방법을 확인하세요.

## 체계의 If 문 논리

리팩터링된 라이브러리 예제에 앞서 조건부 논리를 간단히 검토해 보겠습니다. 구성표는 `if`을 사용하여 두 경로 중 하나를 선택합니다.

다음은 `if` 문의 간단한 형식입니다.

```scheme
(if (conditional test)
  do if true
  do if false)
```

이 구조는 조건을 확인하고, 조건이 true이면 첫 번째 작업을 실행합니다. 조건이 false이면 두 번째 작업을 실행합니다.

조건이 true 또는 false일 때 여러 작업을 수행해야 하는 경우 `begin`을 사용하여 그룹화할 수 있습니다.

```scheme
(if (conditional test)
  (begin
    do if true)
  (begin
    do if false))
```

이를 통해 조건부 테스트 결과에 따라 여러 표현식이나 문을 실행해야 하는 더 복잡한 상황을 처리할 수 있습니다.

좋아, 여기에 반환 값이 포함되어 실행 프로세스를 제어하는 ​​데 사용되는 라이브러리 코드가 있습니다.

### 반환 값으로 리팩토링됨

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

반환 값은 함수를 유연하고 재사용 가능하게 만드는 기본 부분입니다. 각 함수가 무엇을 반환해야 하는지 신중하게 결정함으로써 함수가 서로 잘 상호 작용하고 나머지 코드에 유용한 정보를 제공할 수 있습니다. `#t` 또는 `#f`을 반환하든, 아니면 더 구체적인 것을 반환하든, 반환 값은 프로그램의 흐름을 제어하고 다양한 결과를 처리할 수 있는 방법을 제공합니다.