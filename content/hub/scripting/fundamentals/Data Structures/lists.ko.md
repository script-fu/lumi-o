---
title: "기울기"
type: docs
weight: 4
---
Scheme에서 **목록**은 값을 그룹화하는 데 사용되는 기본 데이터 구조입니다. 목록은 각 요소가 다른 목록을 포함하여 모든 유형일 수 있는 순서가 지정된 요소 모음입니다. 목록은 데이터 저장 및 프로그램 구조 모두를 위해 Scheme에서 널리 사용됩니다.

### 예시 1: 단순 목록

```scheme
(list 1 2 3)
```

- `1`, `2` 및 `3`의 세 가지 요소 목록을 생성합니다.

결과: **`(1 2 3)`**

---

#### 목록 요소에 액세스하기

목록의 요소는 `car` 및 `cdr` 프로시저를 사용하여 액세스됩니다.

- `car`은 목록의 첫 번째 요소를 검색합니다.
- `cdr`은 목록의 나머지 부분(첫 번째 요소를 제외한 모든 항목)을 검색합니다.

#### 예

```scheme
(define my-list (list 1 2 3))
(car my-list)  ; Retrieves the first element
(cdr my-list)  ; Retrieves the rest of the list
```

결과:

- `(car my-list)`은 `1`을 반환합니다.
- `(cdr my-list)`은 `(2 3)`을 반환합니다.

---

#### 단순 재귀: 목록 반복

목록의 `cdr`에서 `car`을 재귀적으로 호출하면 목록을 순회할 때까지 각 요소를 하나씩 처리할 수 있습니다. 이는 많은 목록 처리 알고리즘의 기초를 형성합니다.

#### 예: 목록의 각 요소 인쇄

다음은 목록의 모든 요소를 인쇄하는 간단한 재귀 함수입니다.

```scheme
(define (print-elements lst)
  (if (null? lst)
    (lumi-message "done")
    (begin
      (lumi-message (number->string (car lst))) ;; Print the first element
      (print-elements (cdr lst)))))             ;; Process the rest of the list
```

- **기본 사례:** 목록이 비어 있는 경우(`null? lst`) 재귀를 중지합니다.
- **재귀 사례:** 첫 번째 요소(`car lst`)를 인쇄한 다음 목록의 나머지 부분(`cdr lst`)에서 함수를 호출합니다.

#### 사용 예

```scheme
(print-elements (list 1 2 3))
```

출력:

- `"1"`
- `"2"`
- `"3"`

결과: "완료"

---

#### 작동 방식

1. 함수는 `car`을 사용하여 목록의 첫 번째 요소를 검색하고 처리합니다.
2. 그런 다음 나머지 목록(`cdr`)을 사용하여 자신을 호출합니다.
3. 이 프로세스는 목록이 비어 있을 때까지 반복됩니다(`null? lst`).

---

### 예시 2: 혼합 유형

목록에는 문자열, 부울, 숫자, 기타 목록 또는 표현식 결과를 비롯한 다양한 유형의 요소가 포함될 수 있습니다.

```scheme
(list 42 "hello" #t (list 1 2) (+ 3 4))
```

- 다음을 포함하는 목록이 생성됩니다.
  - 숫자(`42`)
  - 문자열(`"hello"`)
  - 부울(`#t`)
  - 또 다른 목록 (`(1 2)`)
  - 표현식의 결과(`(+ 3 4)`, `7`로 평가됨)

결과: **`(42 "hello" #t (1 2) 7)`**

---

이러한 예는 Scheme의 목록이 데이터를 구성하고 조작하기 위한 강력한 도구가 된다는 점을 보여줍니다.

### 목록 구성

`cons` 프로시저는 요소를 기존 목록과 결합하여 새 목록을 구성하는 데 사용됩니다.

```scheme
(cons new-element existing-list)
```

#### 예

```scheme
(cons 0 (list 1 2 3))
```

- `(1 2 3)` 목록의 시작 부분에 `0`을 추가합니다.

결과: **`(0 1 2 3)`**

---

### 목록 확인 중

`list?` 프로시저는 주어진 값이 목록인지 확인합니다.

```scheme
(list? value)
```

#### 예: 목록?

```scheme
(list? (list 1 2 3))  ; Checks if (list 1 2 3) is a list
(list? 42)            ; Checks if 42 is a list
```

결과:

- `(list? (list 1 2 3))`은 `#t`을 반환합니다(true).
- `(list? 42)`은 `#f`을 반환합니다(거짓).

---

### 목록 작업

Scheme은 다음을 포함하여 목록 작업을 위한 몇 가지 내장 프로시저를 제공합니다.

- `length`: 목록의 요소 수를 반환합니다.
- `append`: 두 개 이상의 목록을 하나로 결합합니다.
- `reverse`: 요소가 역순으로 포함된 새 목록을 반환합니다.

```scheme
(length (list 1 2 3))          ; Returns 3
(append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
(reverse (list 1 2 3))         ; Returns (3 2 1)
```

결과:

- `(length (list 1 2 3))`은 `3`을 반환합니다.
- `(append (list 1 2) (list 3 4))`은 `(1 2 3 4)`을 반환합니다.
- `(reverse (list 1 2 3))`은 `(3 2 1)`을 반환합니다.#### `list-ref` 사용

`list-ref` 프로시저는 목록의 지정된 인덱스(0부터 시작하는 인덱스)에 있는 요소를 검색합니다.

```scheme
(list-ref lst index)
```

- **`lst`**: 요소를 검색할 목록입니다.
- **`index`**: 반환할 요소를 나타내는 0부터 시작하는 인덱스입니다.

##### 예: 목록-ref

```scheme
(list-ref (list 10 20 30 40) 2)  ; Retrieves the element at index 2
```

결과: `30`

---

### 중첩 목록

구성표의 목록은 다른 목록을 요소로 포함하여 중첩 구조를 만들 수 있습니다.

#### 예: 중첩 목록 만들기

```scheme
(define nested-list (list (list 1 2) (list 3 4) (list 5)))
```

- 각 요소 자체가 목록인 세 가지 요소의 목록을 만듭니다.

결과: **`((1 2) (3 4) (5))`**

---

#### 중첩된 데이터에 액세스

중첩 목록 내의 요소에 액세스하려면 `car` 및 `cdr` 조합을 사용하여 구조를 탐색할 수 있습니다.

#### 예: 요소에 액세스하기

```scheme
(car nested-list)              ; Retrieves the first element: (1 2)
(car (car nested-list))        ; Retrieves the first element of the first sublist: 1
(cdr (car nested-list))        ; Retrieves the rest of the first sublist: (2)
(car (cdr (car nested-list)))  ; Retrieves the second element of the first sublist: 2
```

---

#### 설명

1. **`car nested-list`**:
   - `nested-list`의 첫 번째 요소인 `(1 2)`을 검색합니다.

2. **`car (car nested-list)`**:
   - `(1 2)`의 첫 번째 요소인 `1`을 검색합니다.

3. **`cdr (car nested-list)`**:
   - `(1 2)`의 나머지 부분인 `(2)`을 검색합니다.

4. **`car (cdr (car nested-list))`**:
   - `(2)`의 첫 번째 요소인 `2`을 검색합니다.

---

#### 예: 다른 하위 목록의 요소에 액세스

```scheme
(car (cdr nested-list))        ; Retrieves the second sublist: (3 4)
(car (car (cdr nested-list)))  ; Retrieves the first element of the second sublist: 3
```

---

이 접근 방식을 사용하면 중첩 목록의 특정 요소를 체계적으로 탐색하고 액세스할 수 있으므로 계층적 데이터 작업에 강력한 유연성을 제공합니다.

### 요약

- Scheme의 **리스트**는 다양하고 필수적인 데이터 구조입니다.
- `list`을 사용하여 목록을 생성하고, `car` 및 `cdr`을 사용하여 요소에 접근하고, `cons`을 사용하여 목록을 구성합니다.
- `length`, `append`, `reverse` 및 `list-ref`과 같은 내장 프로시저를 통해 목록 작업을 쉽고 효율적으로 수행할 수 있습니다.
- 목록을 중첩할 수 있어 고급 사용 사례를 위한 복잡한 데이터 구조가 가능합니다.