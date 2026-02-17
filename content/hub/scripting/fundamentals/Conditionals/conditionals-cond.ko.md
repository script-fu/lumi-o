---
title: "조건"
type: docs
weight: 5
---
Scheme에서 `cond` 조건은 여러 테스트를 기반으로 실행할 수 있는 여러 코드 블록 중 하나를 선택하는 데 사용됩니다. 이는 일치하는 항목이 발견될 때까지 각 분기를 순서대로 확인하는 다중 분기 `if`과 같습니다.

### 구문

```scheme
(cond
  (test-1 consequent-1)
  (test-2 consequent-2)
  ...
  (else fallback-consequent))
```

- 각 시험은 작성된 순서대로 평가됩니다.
- 테스트가 true(`#t`)로 평가되면 해당 **결과**가 실행되고 `cond` 표현식은 추가 테스트 평가를 중지합니다.
- `else` 절은 선택 사항이며 테스트 중 어느 것도 true로 평가되지 않는 경우 대체 역할을 합니다.

### 작동 방식

1. **각 조건 테스트**:
   - `cond`은 나열된 순서대로 테스트를 평가합니다.

2. **일치 결과 실행**:
   - true(`#t`)로 평가되는 첫 번째 테스트가 발견되면 해당 **결과**가 실행됩니다.
   - true로 평가되는 테스트가 없고 `else` 절이 있는 경우 **fallback-consequent**가 실행됩니다.

### 예

#### 예 1: 단일 표현식 결과

```scheme
(cond
  ((< 3 2) "This won't run")
  ((= 3 3) "This will run")
  (else "Fallback"))
```

- 첫 번째 테스트 `(< 3 2)`은 false(`#f`)로 평가됩니다.
- 두 번째 테스트 `(= 3 3)`이 true(`#t`)로 평가되므로 `"This will run"`이 반환됩니다.
- 일치하는 항목이 이미 발견되었기 때문에 `else` 절이 실행되지 않습니다.

결과: **"실행됩니다"**

#### 예 2: `begin`을 사용한 여러 작업

결과에 여러 작업이 포함된 경우 `begin`을 사용하여 그룹화합니다.

```scheme
(cond
  ((< 5 3)
    (begin
      (lumi-message "This won't run")
      (* 2 3)))
  ((> 5 3)
    (begin
      (lumi-message "Condition met")
      (* 5 5)))
  (else
    (begin
      (lumi-message "Fallback")
      0)))
```

- 첫 번째 테스트 `(< 5 3)`은 false(`#f`)로 평가됩니다.
- 두 번째 테스트 `(> 5 3)`이 true로 평가됩니다(`#t`).
  - `"Condition met"`을 인쇄합니다.
  - 그런 다음 `(* 5 5)`을 계산하고 `25`을 반환합니다.

결과: **"조건 충족"을 인쇄하고 25를 반환합니다.**

#### 예시 3: 결과에 `let` 블록 사용

지역 변수를 도입해야 하는 경우 `let` 블록을 사용하세요.

```scheme
(cond
  ;; Case 1: If 0 is less than -1
  ((< 0 -1)
    (let ((x 10))
      (* x x)))

  ;; Case 2: If 0 is greater than -1
  ((> 0 -1)
    (let ((y 20))
      (lumi-message "Positive condition met")
      (+ y y)))

  ;; Default case: If none of the above conditions are met
  (else
    (let ((z 0))
      z)))
```

- 첫 번째 테스트 `(< 0 -1)`이 거짓입니다.
- 두 번째 테스트 `(> 0 -1)`이 true이므로 다음과 같습니다.
  - `let` 블록이 실행되어 `y`을 `20`에 바인딩합니다.
  - `"Positive condition met"`을 인쇄합니다.
  - 그런 다음 `(+ y y)`을 계산하고 `40`을 반환합니다.

결과: **"긍정적 조건 충족"을 인쇄하고 40을 반환합니다.**

#### 예시 4: `else`을 사용한 대체

```scheme
(cond
  ((< 5 3) "This won't run")
  ((= 5 3) "This won't run either")
  (else "Fallback value"))
```

- 처음 두 테스트 중 어느 것도 참으로 평가되지 않습니다.
- `else` 절이 실행되어 `"Fallback value"`을 반환합니다.

결과: **"대체 값"**

### 요약

- 여러 조건을 명확하고 간결하게 처리하려면 `cond`을 사용하세요.
- 결과는 `begin`을 사용하는 단일 표현식 또는 그룹화된 작업일 수 있습니다.
- 결과에 `let`을 사용하여 계산을 위한 지역 변수를 선언합니다.
- 예상치 못한 경우를 처리하기 위해 항상 `else` 절을 대체 수단으로 포함하세요.

이러한 유연성 덕분에 `cond`은 복잡한 분기 논리를 처리하기 위한 강력하고 읽기 쉬운 도구입니다.