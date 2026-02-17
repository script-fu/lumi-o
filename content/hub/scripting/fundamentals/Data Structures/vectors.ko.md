---
title: "벡터"
type: docs
weight: 5
---
Scheme에서 벡터는 값을 그룹화하는 데 사용되는 또 다른 기본 데이터 구조입니다. 목록과 달리 벡터는 고정 크기의 인덱스 요소 컬렉션으로, 더 빠른 임의 액세스 및 업데이트를 제공합니다. 벡터의 각 요소는 다른 벡터를 포함하여 모든 유형이 될 수 있습니다. 벡터는 # 다음에 괄호를 사용하여 표시됩니다. `#(1 2 3)`

벡터와 목록은 유사해 보일 수 있지만 Scheme 프로그래밍에서는 서로 다른 목적으로 사용됩니다.

- 목록은 연결된 노드 구현을 통해 재귀 분해를 통해 시작 및 순회를 효율적으로 조작할 수 있으므로 재귀 작업 및 동적 구조에 더 일반적으로 사용됩니다.

- 반면 벡터는 특정 인덱스의 요소 또는 업데이트에 대한 무작위 액세스가 필요한 시나리오에 최적화되어 조회 테이블, 고정 크기 구성 또는 성능이 중요한 인덱싱 작업과 같은 사용 사례에 더 적합합니다.

본질적으로 목록은 재귀 알고리즘과 동적 크기 데이터에 대한 자연스러운 선택인 반면, 벡터는 고정 크기 또는 인덱스 액세스 패턴이 가장 중요한 경우에 빛을 발합니다.

### 간단한 벡터

```scheme
(vector 1 2 3)
```

- `1`, `2` 및 `3`의 세 가지 요소로 구성된 벡터를 생성합니다.

결과: **`#(1 2 3)`**

#### 벡터 요소에 액세스하기

벡터의 요소는 `vector-ref` 프로시저를 사용하여 액세스되며 지정된 인덱스(`0`에서 시작)에서 요소를 검색합니다.

```scheme
(define my-vector (vector 1 2 3))
(vector-ref my-vector 0)  ; Retrieves the element at index 0
(vector-ref my-vector 1)  ; Retrieves the element at index 1
```

#### 반복: 벡터의 각 요소 처리

루프나 재귀를 사용하여 벡터를 반복할 수 있습니다. 체계는 벡터의 크기를 결정하기 위해 `vector-length`을 제공합니다. 다음은 벡터의 모든 요소를 인쇄하는 간단한 루프입니다.

```scheme
(define (print-elements vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (begin
        (lumi-message (number->string (vector-ref vec i))) ; Print the element
        (loop (+ i 1)))                                    ; Process the next index
      (lumi-message "done"))))                             ; End loop
```

- **기본 사례:** `i` 인덱스가 벡터 길이에 도달하면 루프를 중지합니다.
- **재귀 사례:** `i` 인덱스의 요소를 인쇄한 다음 `i`을 증가시킵니다.

#### 사용 예

```scheme
(print-elements (vector 1 2 3))
```

결과:

- `"1"`
- `"2"`
- `"3"`

결과: "완료"

### 혼합 벡터

벡터에는 문자열, 부울, 숫자, 기타 벡터 또는 표현식 결과를 비롯한 다양한 유형의 요소가 포함될 수 있습니다.

```scheme
(vector 42 "hello" #t (vector 1 2) (+ 3 4))
```

그러면 다음을 사용하여 벡터가 생성됩니다.
  - 숫자(`42`)
  - 문자열(`"hello"`)
  - 부울(`#t`)
  - 또 다른 벡터(`#(1 2)`)
  - 표현식의 결과(`(+ 3 4)`, `7`로 평가됨)

결과: **`#(42 "hello" #t #(1 2) 7)`**

### 벡터 구성하기

벡터는 `vector`을 사용하여 생성되거나 `make-vector`을 사용하여 초기 값으로 고정된 크기의 벡터를 생성합니다.

```scheme
(make-vector 5 0)
```

`0`으로 초기화된 모든 요소를 사용하여 `5` 크기의 벡터를 생성합니다.

결과: `#(0 0 0 0 0)`

### 벡터 업데이트 중

`vector-set!` 프로시저는 지정된 인덱스에 있는 벡터의 요소를 업데이트합니다.

```scheme
(define my-vector (vector 1 2 3))
(vector-set! my-vector 1 42)  ; Sets the second element to 42
my-vector
```

결과: `#(1 42 3)`

### 벡터 확인 중

`vector?` 프로시저는 주어진 값이 벡터인지 확인합니다.

```scheme
(vector? (vector 1 2 3))  ; Checks if #(1 2 3) is a vector
(vector? 42)              ; Checks if 42 is a vector
```

결과:

- `(vector? (vector 1 2 3))`은 `#t`을 반환합니다(true).
- `(vector? 42)`은 `#f`을 반환합니다(거짓).

### 벡터와 참조에 의한 전달 동작Scheme에서 벡터는 변경 가능하며 참조로 전달됩니다. 이는 벡터를 함수에 전달하면 함수가 원래 벡터를 직접 수정할 수 있음을 의미합니다. 함수 내부의 벡터에 대한 모든 변경 사항은 함수 외부에도 반영됩니다. 이 동작은 여러 기능에서 데이터를 효율적으로 공유하고 업데이트하는 데 유용하지만 의도하지 않은 부작용을 방지하려면 주의가 필요합니다.

#### 예: 함수에서 벡터 수정

다음은 벡터가 참조로 전달되고 수정되는 방법을 보여주는 예입니다.

```scheme
(define (modify-vector vec index new-value)
  (vector-set! vec index new-value))  ; Updates the vector at the specified index

(define my-vector (vector 10 20 30))
(modify-vector my-vector 1 99)         ; Modifies the second element to 99
my-vector                              ; The original vector is now updated
```

결과: `#(10 99 30)`

#### 단계별 설명

1. **벡터 생성:** `my-vector`은 `10`, `20` 및 `30` 값으로 초기화됩니다.
2. **함수에 전달:** `my-vector`은 업데이트할 인덱스 및 새 값과 함께 `modify-vector`에 전달됩니다.
3. **함수 수정:** `vector-set!` 프로시저는 원본 벡터에서 직접 지정된 인덱스의 값을 업데이트합니다.
4. **변경 사항 반영:** 벡터는 참조로 전달되므로 함수 내에서 변경된 내용이 원본 벡터에 반영됩니다.

#### 참조에 의한 전달의 의미

- **성능:** 참조로 벡터를 전달하는 것은 큰 구조를 복사하는 것을 방지하므로 효율적입니다.
- **부작용:** 공유 데이터에 대한 의도하지 않은 수정을 방지하려면 기능 간에 벡터를 공유할 때 주의하십시오.

### 벡터 작업

Scheme은 다음을 포함하여 벡터 작업을 위한 여러 내장 프로시저를 제공합니다.

- `vector-length`: 벡터의 요소 수를 반환합니다.
- `vector->list`: 벡터를 목록으로 변환합니다.
- `list->vector`: 목록을 벡터로 변환합니다.

```scheme
(vector-length (vector 1 2 3))         ; Returns 3
(vector->list (vector 1 2 3))          ; Converts vector to list: (1 2 3)
(list->vector (list 1 2 3))            ; Converts list to vector: #(1 2 3)
```

결과:

- `(vector-length (vector 1 2 3))`은 `3`을 반환합니다.
- `(vector->list (vector 1 2 3))`은 `(1 2 3)`을 반환합니다.
- `(list->vector (list 1 2 3))`은 `#(1 2 3)`을 반환합니다.

### 중첩 벡터

Scheme의 벡터는 다른 벡터를 요소로 포함하여 중첩 구조를 만들 수 있습니다.

```scheme
(define nested-vector (vector (vector 1 2) (vector 3 4) (vector 5)))
```

각 요소 자체가 벡터인 세 요소의 벡터를 만듭니다.

결과: **`#(#(1 2) #(3 4) #(5))`**

#### 중첩된 데이터에 액세스

중첩된 벡터 내의 요소에 액세스하려면 `vector-ref`을 여러 번 사용하여 구조를 탐색하세요.

#### 예: 요소에 액세스하기

```scheme
(vector-ref nested-vector 0)              ; Retrieves the first element: #(1 2)
(vector-ref (vector-ref nested-vector 0) 1) ; Retrieves the second element of the first vector: 2
```

### 요약

- Scheme의 **벡터**는 고정 크기의 인덱스 데이터 구조입니다.
- `vector`을 사용하여 벡터를 생성하고, `vector-ref`을 사용하여 요소에 액세스하고, `vector-set!`을 사용하여 요소를 업데이트합니다.
- `vector-length`, `vector->list` 및 `list->vector`과 같은 내장 프로시저를 통해 유연한 작업이 가능합니다.
- 중첩된 벡터는 복잡한 계층적 데이터 구조를 허용합니다.