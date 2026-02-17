---
title: "만약에"
type: docs
weight: 4
---
가장 간단한 형태로 Scheme의 `if` 조건은 테스트를 평가하고 그 결과에 따라 가능한 두 코드 블록 중 하나를 실행합니다. 가장 간단한 형태는 다음과 같습니다.

```scheme
(if test-is-true
  do-this)
```

- `test`이 true(`#t`)로 평가되면 **결과의 코드 블록**이 실행됩니다. 블록은 값을 반환하거나 변수 할당 또는 출력 인쇄와 같은 다른 작업을 수행할 수 있습니다.

### 예

```scheme
(if (< 0 1)
  (lumi-message "True!"))
```

- 이 경우 `test`은 `(< 0 1)`입니다(0이 1보다 작은지 확인).
- 테스트 결과가 true(`#t`)로 평가되므로 `"True!"`을 인쇄하는 코드 블록 `(lumi-message "True!")`이 실행됩니다.

### Else 조건 추가: `if-else`

대체 코드 블록(`else` 사례)과 함께 `if` 조건을 사용하는 경우 구조는 다음과 같습니다.

```scheme
(if test
  do-this
  else-do-this)
```

- `test`이 true(`#t`)로 평가되면 **consequent** 코드 블록이 실행됩니다.
- `test`이 false(`#f`)로 평가되면 **대체** 코드 블록이 실행됩니다.

```scheme
(if test
  consequent
  alternative)
```

### 작동 방식

1. **테스트 표현식**:
   - `test` 표현식이 먼저 평가됩니다.

2. **테스트 기반 결과**:
   - `test`이 true(`#t`)로 평가되면 **결과 코드 블록**이 실행됩니다.
   - `test`이 false(`#f`)로 평가되면 **대체 코드 블록**이 실행됩니다.

`consequent` 및 `alternative` 코드 블록은 모두 값 반환, 변수 수정 또는 프로시저 실행을 포함하여 유효한 Scheme 작업을 수행할 수 있습니다.

### 예

#### 예시 1: 값 반환

```scheme
(if (< 0 1)
  1
  0)
```

- 여기서 `test`은 `(< 0 1)`입니다(0이 1보다 작은지 확인).
- 테스트 결과가 true(`#t`)로 평가되었으므로 **consequent** 블록(`1`)이 실행되고 해당 값이 반환됩니다.

결과: **1**

#### 예시 2: 시작 블록 평가

조건이 true 또는 false일 때 여러 작업을 수행해야 하는 경우 `begin` 또는 `let`을 사용하여 그룹화할 수 있습니다.

```scheme
(if (= 0 1)
  (begin
    (lumi-message "This won't run")
    1)
  (begin
    (lumi-message "False condition met, calculating...")
    (* 3 4)))
```

- 이 예에서 `test`은 `(= 0 1)`입니다(0이 1인지 확인).
- 테스트가 false(`#f`)로 평가되었으므로 **대체** 블록이 실행됩니다.
  - 먼저 `"False condition met, calculating..."`을 출력합니다.
  - 그런 다음 `(* 3 4)`을 계산하여 `12`을 반환합니다.

결과: **"거짓 조건 충족, 계산 중..."이 인쇄되고 12가 반환됩니다.**

#### 예시 3: let 문 평가

`let`을 사용하면 코드 블록 내에서 로컬 범위 변수를 선언할 수 있습니다.

```scheme
(if (= 1 1)
  (let (x -1)
    (lumi-message "True condition met, calculating...")
    (* x 10))
  (let (y 4)
    (lumi-message "This won't run")
    (* 3 y)))
```

- 이 예에서 `test`은 `(= 1 1)`입니다(1이 1인지 확인).
- 테스트가 true(`#t`)로 평가되었으므로 **consequent** 블록이 실행됩니다.
  - 먼저 `"True condition met, calculating..."`을 출력합니다.
  - 그런 다음 `(* -1 10)`을 계산하여 `-10`을 반환합니다.

결과: **"True 조건 충족, 계산 중..."을 인쇄하고 -10을 반환합니다.**

### 요약- `if` 조건은 테스트를 평가하고 해당 코드 블록을 실행하기 위한 Scheme의 강력한 도구입니다.
- 값을 반환하거나 변수를 수정하거나 부작용을 수행하는 간단한 표현식과 복잡한 코드 블록을 모두 처리할 수 있습니다.
- 기억하세요: 명시적인 `else` 블록이 없으면 `if`은 테스트가 참인 경우에만 **결과**를 평가하고 실행합니다. 그렇지 않으면 **대안**을 평가하고 실행합니다.