---
title: "리팩토링"
type: docs
weight: 2
---
함수가 작동하면 한 걸음 물러나서 코드를 구성하는 가장 좋은 방법에 대해 생각해 볼 수 있습니다. 목표는 플러그인을 최대한 명확하고 이해하기 쉽고 유지 관리하기 쉽게 만드는 것입니다. 동작을 변경하지 않고 기존 코드의 구조를 개선하고 개선하는 프로세스를 리팩토링이라고 합니다.

다시 초기 함수는 다음과 같습니다.

```scheme
(define (scheme-hello-world)
  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))
```

function-name은 함수의 이름이고, 매개변수는 함수가 입력으로 받아들이는 것입니다. 본문은 함수가 호출될 때 실행되는 코드 블록입니다.

추상적인 형태:

```scheme
(define (function-name parameter)
  body)
```

### 코드 반복

반복을 일찍 제거하십시오. `(lumi-message "Hello world!\n")`이 2번 반복되고, 메시지 문자열이 3번 반복됩니다. 변수는 반복되는 문자열을 해결합니다.

### 변수

Scheme에서 변수에는 알려진 "범위"가 있으며 해당 범위는 `let` 문을 사용하여 설정됩니다. 변수는 바인딩 부분의 값에 바인딩되고 변수는 let 본문에 범위를 갖습니다. 변수는 let 블록 내부에서만 알려지며 외부에서는 액세스할 수 없습니다.

```scheme
(let ((variable value))
  body)
```

"메시지"라는 변수를 소개합니다.

```scheme
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    ;; Set the message handler to output the message to a GUI dialog box
    (lumi-message-set-handler 0)
    (lumi-message message)

    ;; Set the message handler to output the message to the Error Console
    (lumi-message-set-handler 2)
    (lumi-message message)

    ;; Send the message to the terminal, the OS window that launched Lumi
    (display message)))
```

이 예에서는 "Hello world!\n" 문자열에 바인딩된 "message"라는 변수를 사용했습니다. 이를 통해 메시지 내용을 세 번이 아닌 한 번 변경할 수 있으므로 오류 가능성이 줄어들고 코드가 더욱 유연해집니다.

### 추출 기능

함수형 프로그래밍에서는 재사용 가능한 논리를 별도의 함수로 추출하기 위해 코드를 리팩토링하는 것이 일반적인 관행입니다. 이렇게 하면 **주 함수**는 훨씬 더 단순해지고 상위 수준 목표에 더 집중하게 되는 반면, **추출된 함수**는 세부 논리를 처리하기 때문에 더 복잡해 보입니다. 이는 의도적인 것이며 모듈성, 관심사 분리 및 가독성과 같은 함수형 프로그래밍의 핵심 원칙에 부합합니다. 리팩토링된 내용은 다음과 같습니다.
안녕하세요 월드! 추출 후.

논리 추출:
```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

;; Main Function
(define (scheme-hello-world)
  (let ((message "Hello world!\n"))

    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

#### 기호
위의 예에서는 'gui.'와 같이 심볼이라는 데이터 유형이 사용되었습니다. 기호는 메시지 보내기 기능에 매개변수로 전달되며 간단한 조건부 결정을 내리는 데 사용할 수 있습니다. 기호 키와 마찬가지로 고유 식별자입니다. 기호에 대한 자세한 내용을 보려면 [this page.](/hub/scripting/fundamentals/variables-and-scope/symbols/)을 방문하세요.

### 주요 기능 단순화

원래(scheme-hello-world) 함수에서는 다양한 출력(GUI, 오류 콘솔, 터미널)으로 메시지를 보내는 모든 논리가 기본 함수에 혼합되었습니다. 리팩토링 후 기본 기능은 단순히 **수행해야 할 작업**에만 초점을 맞추고 메시지를 다른 대상으로 보냅니다.

리팩터링된 main 함수는 더 간단합니다.

- 목적을 명확하게 명시합니다. 동일한 메시지를 여러 출력으로 보냅니다.
- 다양한 출력에 대한 메시지 핸들러 설정과 같은 반복적인 코드로 기본 로직을 복잡하게 만드는 것을 방지합니다.
- 한눈에 읽고 이해하기가 더 쉽습니다.

### 추출된 함수의 복잡성

대조적으로 **(메시지 보내기) 기능**은 자세한 로직이 있는 곳입니다. 이제 각 출력(GUI, 오류 콘솔, 터미널)에 대한 다양한 동작을 처리합니다. 이 기능은 이전보다 조금 더 복잡하지만 이제는 **중앙 집중화**되고 **격리**됩니다.

## 이를 함수형 프로그래밍과 연관짓기

함수형 프로그래밍에서 함수는 **일급 시민**으로 간주됩니다. 즉, 함수를 재사용하고 전달하고 결합하여 더 복잡한 동작을 형성할 수 있습니다. 목표는 다음과 같습니다.- **문제를** 더 작고 독립적인 조각으로 분해합니다.
- **복잡성을 분리**하여 `send-message`과 같이 특정 작업을 처리하는 더 작은 함수로 분리합니다.
- **상위 수준 기능을 단순하게 유지**하여 각 작업이 어떻게 수행되는지에 대한 세부 정보를 알 필요 없이 데이터와 작업의 흐름을 조정하는 데 집중할 수 있습니다.
- **관심사 분리**: 이 기능은 출력 유형에 따라 메시지가 전송되는 방식을 관리하며, 이는 이 논리를 기본 기능에서 분리합니다.
- **모듈화**: 모든 메시지 전송 로직을 한 곳에서 처리함으로써 기본 기능을 변경하지 않고도 쉽게 변경할 수 있습니다(예: 새 출력 옵션 추가).
- **재사용성**: `send-message` 함수는 재사용이 가능합니다. 즉, 코드의 다른 곳에서 여러 출력으로 메시지를 보내야 하는 경우 유사한 논리를 다시 작성하는 대신 이 함수를 호출하기만 하면 됩니다.

리팩토링을 통해 이 예제의 주요 함수는 무슨 일이 일어나고 있는지에 대한 **선언적** 문("세 곳으로 메시지 보내기")이 되는 반면, 해당 메시지를 보내는 방법의 복잡성은 `send-message` 함수로 추상화됩니다.