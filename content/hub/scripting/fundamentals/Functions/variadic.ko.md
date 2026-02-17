---
title: "가변 함수"
type: docs
weight: 2
---
**Scheme의 가변 함수**는 가변 개수의 인수를 허용하는 함수입니다. 이러한 기능은 매우 다양하며 유연하고 재사용 가능한 코드를 만들 수 있습니다. 함수형 프로그래밍에서 가변 함수는 숫자 목록 합산 또는 문자열 연결과 같이 임의 개수의 입력을 처리해야 하는 작업을 단순화합니다.

Variadic 함수는 다음과 같은 경우에 특히 유용합니다.

- 인수 개수는 미리 정할 수 없습니다.
- 동적 입력 목록에 동일한 작업을 적용해야 합니다.
- 데이터 집계 또는 변환을 위한 유틸리티 작성.

### 가변 함수 구문

Variadic 함수는 마지막 매개변수 이름 앞에 `.` 기호를 사용하여 정의됩니다. 이 마지막 매개변수는 나머지 모든 인수를 목록으로 수집합니다.

```scheme
(define (function-name fixed-parameters . variadic-parameter)
  body-expression)
```

- **`fixed-parameters`:** 함수가 허용하는 필수 고정 인수입니다.
- **`variadic-parameter`:** 추가 인수를 목록으로 수집하는 `.` 앞에 오는 특수 매개변수입니다.
- **`body-expression`:** 함수가 호출될 때 실행되는 로직입니다.

### 가변 함수의 예

#### 기본 가변 함수

```scheme
(define (sum . numbers)
  (apply + numbers))
```

- **설명**:
  - `numbers`은 모든 인수를 목록으로 수집합니다.
  - `apply`은 목록의 모든 요소에 `+` 기능을 적용합니다.

**용도**:
```scheme
(sum 1 2 3 4 5)  ; Returns 15
```

#### 고정 매개변수를 갖는 가변 함수

고정 매개변수와 가변 매개변수를 결합하여 보다 유연한 기능을 만들 수 있습니다.

```scheme
(define (greet prefix . names)
  (map (lambda (name) (string-append prefix " " name)) names))
```

- **설명**:
  - `prefix`은 고정 인수입니다.
  - `names`은 나머지 인수를 목록으로 수집합니다.
  - 각 이름에는 `map` 및 `lambda`을 사용하여 지정된 문자열이 접두사로 붙습니다.

**용도**:
```scheme
(greet "Hello" "Alice" "Bob" "Charlie")  ; Returns ("Hello Alice" "Hello Bob" "Hello Charlie")
```

#### 고정 및 가변 논리 결합

```scheme
(define (describe-collection collection-name . items)
  (string-append collection-name ": " (string-join items ", ")))
```

- **설명**:
  - `collection-name`은 고정된 매개변수입니다.
  - `items`은 추가 인수를 목록으로 수집합니다.
  - 이 함수는 컬렉션 이름과 항목을 단일 문자열로 연결합니다.

**용도**:
```scheme
(describe-collection "Fruits" "Apple" "Banana" "Cherry")
; Returns "Fruits: Apple, Banana, Cherry"
```

### 고급 사용 사례

#### 임의 입력 처리

Variadic 함수는 임의의 데이터를 처리하는 데 탁월합니다. 다음은 양수만 합산하는 예입니다.

```scheme
(define (sum-positive . numbers)
  (apply + (filter (lambda (x) (> x 0)) numbers)))
```

- 합산하기 전에 양수가 아닌 숫자를 필터링합니다.

**용도**:
```scheme
(sum-positive -5 3 7 -2 8)  ; Returns 18
```

#### 재귀 논리를 사용한 가변 함수

```scheme
(define (max-value first . rest)
  (if (null? rest)
      first
      (max first (apply max rest))))
```

- **설명**:
  - `first`이 첫 번째 인수를 처리합니다.
  - `rest`은 나머지 인수를 목록으로 수집합니다.
  - 최대값을 재귀적으로 계산합니다.

**용도**:
```scheme
(max-value 10 20 5 40 15)  ; Returns 40
```

### 가변 함수의 이점

- **유연성:** 광범위한 입력 사례를 처리합니다.
- **간결성:** 여러 개의 오버로드된 함수에 대한 필요성을 줄입니다.
- **동적 작업:** 인수 개수를 미리 알지 않고도 런타임 데이터 처리를 활성화합니다.

### 가변 함수를 사용해야 하는 경우

다음과 같은 경우 가변 함수를 사용합니다.

- 함수는 알 수 없는 개수의 인수를 처리해야 합니다.
- 단일 작업이 모든 입력에 적용됩니다(예: 합산, 연결 또는 매핑).
- 동적 인수를 사용하여 고차 논리를 단순화합니다.

다음과 같은 경우에는 가변 함수를 사용하지 마세요.

- 입력 유효성 검사 또는 유형 검사가 복잡합니다.
- 필요한 논리에 대해 고정 인수로 충분합니다.
- 지나치게 복잡한 작업으로 인해 가독성이 저하됩니다.

### 결론Scheme의 Variadic 기능은 동적 입력을 처리하기 위한 강력한 메커니즘을 제공합니다. 해당 구문과 사용법을 이해하면 다양한 시나리오에 적응하는 유연하고 강력한 스크립트를 만들 수 있습니다. 고차 함수와 결합된 가변 함수는 코드를 더욱 간결하고 표현력있게 만듭니다.