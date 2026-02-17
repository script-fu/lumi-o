---
title: "변수 및 범위"
type: docs
weight: 1
---
Scheme에서 변수와 그 범위를 관리하는 것은 효율적이고 유지 관리 가능한 스크립트를 작성하기 위한 핵심 개념입니다. 변수는 스크립트가 조작할 수 있는 데이터 값을 저장하는 반면 범위는 해당 변수에 액세스할 수 있는 위치를 정의합니다. 변수를 효과적으로 정의하고 사용하는 방법을 이해하면 구조화되고 재사용 가능하며 오류 없는 코드를 만들 수 있습니다.

### 동적 타이핑

Scheme is dynamically typed: you don’t declare types up front, and a variable can hold values of different kinds over time.

```scheme
(define x 42)       ; x is a number
(set! x "hello")    ; now x is a string
```

### 스킴에서 변수 정의와 범위의 역할

변수를 정의하고 해당 범위를 관리하는 데는 여러 가지 목적이 있습니다.
- **데이터 구성:** 변수는 정보를 저장하여 스크립트를 더 읽기 쉽고 관리하기 쉽게 만듭니다.
- **재사용성 향상:** 범위가 지정된 변수를 사용하면 충돌 없이 코드 섹션을 재사용할 수 있습니다.
- **캡슐화:** 지역화된 범위는 스크립트의 여러 부분에 있는 변수 간의 의도하지 않은 상호 작용을 방지합니다.
- **논리 단순화:** 제한된 범위의 임시 변수는 대규모 계산이나 작업흐름의 복잡성을 줄여줍니다.

### 변수 정의 유형 및 범위

Scheme은 변수를 정의하고 범위 지정하기 위한 여러 구성을 제공합니다.
- **`let`:** Creates local bindings for variables within a specific block of code.
- **`let*`:** A sequential version of `let` where each binding can depend on the previous ones.
- **Named `let`:** A powerful construct for defining recursive local procedures or loops.
- **`define`:** Creates global variables or functions that are accessible throughout your script.

### 변수 정의 및 범위 작동 방식

변수 정의 및 범위에는 일반적으로 다음이 포함됩니다.
1. **변수 선언:** 특정 컨텍스트에서 변수에 값을 할당합니다.
2. **Limiting Scope:** Controlling where the variable is accessible (e.g., within a `let` block or globally).
3. **변수 사용:** 계산, 논리 또는 절차적 작업을 수행하기 위해 변수 값에 액세스하고 수정합니다.

### Example: Using `let` for Local Variables

The `let` construct allows you to define temporary variables that are available only within a specific block:

```scheme
(let ((x 10)
      (y 20))
  (+ x y))
```

- This example declares `x` and `y` with local values and computes their sum.

### Example: Using `define` for Global Variables

The `define` construct creates variables or functions with global scope:

```scheme
(define pi 3.14159)
(define (circle-area radius)
  (* pi radius radius))
```

- This script defines a global constant `pi` and a function `circle-area` that uses it.

### 범위 비교: 로컬 및 글로벌

| 기능 | Local Scope (`let`, `let*`)               | Global Scope (`define`)                       |
|------------------|------------------------------------------|-----------------------------------------------|
| **접근성** | 정의된 블록으로 제한됨 | 전체 스크립트에서 액세스 가능 |
| **캡슐화** | 의도하지 않은 상호작용 방지 | 전역적으로 정의된 다른 변수와 충돌할 수 있음 |
| **사용 사례** | 특정 작업을 위한 임시 변수 | 전체에서 사용되는 공유 변수 또는 함수 |

### 요약- **변수 정의 및 범위**는 Scheme 스크립트에서 데이터를 구성하고 관리하는 데 기초가 됩니다.
- Use **local scope** (`let`, `let*`, named `let`) to encapsulate temporary variables and avoid conflicts.
- Use **global scope** (`define`) for reusable functions or constants shared across your script.
- 이러한 구문을 명확하게 이해하면 코드의 가독성, 유지 관리성 및 안정성이 향상됩니다.