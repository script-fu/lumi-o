---
title: "기능"
type: docs
weight: 7
---
함수는 Scheme의 핵심 개념으로, 로직을 캡슐화하고 코드 재사용을 활성화하며 스크립트를 효과적으로 구성하는 수단을 제공합니다. 함수를 사용하면 Lumi의 기본 작업부터 고급 작업 흐름까지 광범위한 작업을 처리하는 유지 관리가 가능한 모듈식 스크립트를 만들 수 있습니다.

이 섹션에서는 Scheme의 기능을 소개하고 해당 기능의 유형, 정의 및 용도를 이해하기 위한 기초를 다집니다. 후속 섹션에서는 특정 기능 유형과 고유한 기능을 더 자세히 살펴보겠습니다.

## 최소 구문 및 표현식

스키마 코드는 **표현식**으로 구성됩니다. 표현식은 값으로 평가됩니다. The syntax is uniform: parentheses form a call, with the operator or function name first.

```scheme
(+ 1 2)         ; Adds 1 and 2, resulting in 3
(if #t 1 0)     ; Evaluates to 1 because the condition is true
(list 1 2 3)    ; Creates a list: (1 2 3)
```

Because everything is an expression, control flow fits naturally into the same style as function calls.

## 함수가 중요한 이유

함수는 여러 가지 이유로 Scheme에서 중추적인 역할을 합니다.

- **코드 재사용성:** 로직을 재사용 가능한 구성 요소로 캡슐화하여 반복을 방지합니다.
- **모듈화:** 복잡한 작업을 관리하기 쉬운 작은 조각으로 나눕니다.
- **동적 동작:** 다양한 입력을 처리하거나 다양한 상황에 적응하기 위해 매개변수를 허용합니다.
- **고차원적인 추상화:** 함수가 '어떻게' 수행하는지보다는 '무엇'을 수행하는지에 초점을 맞춰 논리를 단순화합니다.

## 함수 유형 개요

Scheme은 각각 특정 사용 사례에 적합한 다양한 기능 구성을 제공합니다.

1. **명명된 함수**
   These are standard functions defined with `define`. 이는 대부분의 스크립트의 중추를 형성합니다.

   ```scheme
   (define (square x)
     (* x x))
   ```

2. **익명 기능**
   **람다 함수**라고도 알려진 이 함수는 일회성 사용을 위해 인라인으로 정의된 이름 없는 함수입니다.

   ```scheme
   (lambda (x) (* x x))
   ```

3. **고차 함수**
   다른 함수를 인수로 사용하거나 함수를 결과로 반환하는 함수로 매핑, 필터링, 축소와 같은 강력한 추상화가 가능합니다.

   ```scheme
   (map (lambda (x) (* x x)) '(1 2 3 4))  ; Returns (1 4 9 16)
   ```

## 함수의 일반 구문

Scheme의 함수에는 간단하고 일관된 구문이 있습니다.

```scheme
(define (function-name parameter1 parameter2 ...)
  body-expression)
```

- **`function-name`:** The name of the function.
- **`parameter1, parameter2, ...`:** The arguments the function takes.
- **`body-expression`:** The logic executed when the function is called.

예:

```scheme
(define (add x y)
  (+ x y))

(add 3 5)  ; Returns 8
```

## 부작용과 전역 상태

In Lumi, many useful procedures have **side effects**: they modify an image, change a drawable, write a file, or display output.

- Isolate side effects in small, clearly named procedures.
- Avoid changing global context unless you need to.
- 컨텍스트(색상, 브러시 등)를 변경할 때 작업을 `lumi-context-push` 및 `lumi-context-pop`으로 래핑하여 사용자 상태를 복원합니다.