---
title: "언제"
type: docs
weight: 5
---
Scheme에서는 `if`이 우아하고 다재다능하지만 명시적인 `else` 없이 사용하면 혼란스러울 수 있습니다. `false` 사례에 대한 대체 작업 없이 조건이 true인 경우에만 단일 코드 분기를 실행하려는 의도가 있는 경우 특히 그렇습니다. 이러한 시나리오에서는 `when` 구문이 더 명확하고 간결한 대안을 제공합니다.

`when`의 기본 형식은 다음과 같습니다.

```scheme
(when test-is-true
  do-this
  do-that)
```

- `test`이 true(`#t`)로 평가되면 `when` 구문 본문의 모든 표현식이 순차적으로 실행됩니다.
- `test`이 false(`#f`)로 평가되면 아무 일도 일어나지 않으며 값도 반환되지 않습니다.

### 예

```scheme
(when (< 0 1)
  (lumi-message "Condition is true!")
  (lumi-message "Executing additional actions."))
```

### `if`과 `when` 대조

`if`과 `when`의 차이점을 더 잘 이해하려면 두 가지가 함께 사용되는 다음 예를 고려하십시오.

```scheme
(if (= 0 1)
  (lumi-message "This will not run")
  (when (< 0 1)
    (lumi-message "The 'when' condition is true!")
    (lumi-message "Executing multiple actions within 'when'.")))
```

#### 설명:

1. **`if` 조건**:
   - `(= 0 1)` 테스트는 0이 1과 같은지 확인합니다.
   - 거짓(`#f`)이므로 `if`의 `else` 브랜치가 실행됩니다.

2. **`else` 지점의 `when` 구성**:
   - `when` 테스트 `(< 0 1)`은 0이 1보다 작은지 확인합니다.
   - 이것이 사실이므로(`#t`), `when` 본문 내의 모든 표현식이 순차적으로 실행됩니다.
     - First, it prints `"The 'when' condition is true!"`.
     - Then, it prints `"Executing multiple actions within 'when'."`.

#### 여기서 `when`을 사용하는 이유는 무엇입니까?

- 다른 `if` 대신 `when`을 사용하면 조건에 대해 명시적인 `else` 분기가 필요하지 않을 때 논리가 단순화됩니다.
- `when`은 실제 분기만 관련이 있음을 분명히 하여 잠재적인 혼란을 줄입니다.

### 요약

- 참 분기와 거짓 분기가 모두 필요한 경우 `if`을 사용하세요.
- 실제 사례에 대한 분기가 하나만 있는 경우, 특히 여러 작업을 실행해야 하는 경우 `when`을 사용하세요.
- `if`과 `when`을 결합하면 더 복잡한 조건문을 명확하고 간결하게 구성하는 데 도움이 될 수 있습니다.