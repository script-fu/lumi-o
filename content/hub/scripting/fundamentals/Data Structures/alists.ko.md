---
title: "연관 목록(Alist)"
type: docs
weight: 6
---
**연관 목록**(또는 **alist**)은 키-값 쌍 모음을 나타내는 데 사용되는 Scheme의 기본 데이터 구조입니다. 이는 쌍 목록으로 구현되며, 각 쌍은 키(일반적으로 기호)를 값과 연결합니다. Alist는 간단하고 유연하며 중소 규모 데이터세트에 적합합니다.

### 연관 목록의 구조

alist는 각 요소가 **쌍**(`cons`으로 구성됨)인 목록입니다. 각 쌍은 다음으로 구성됩니다.

- **키**: 첫 번째 요소(일반적으로 기호)입니다.
- **값**: 모든 데이터 유형이 될 수 있는 두 번째 요소입니다.

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

- **키**: `'name`, `'age`, `'city`
- **값**: `"Alice"`, `30`, `"Paris"`
- **구조**: 쌍 목록:
  `((name . "Alice") (age . 30) (city . "Paris"))`

### Alist 만들기

수동으로 쌍을 구성하거나 `cons`을 사용하여 프로그래밍 방식으로 빌드하여 목록을 생성할 수 있습니다.

#### 작은따옴표 사용(`'`)

작은따옴표(`'`)는 **인용**을 줄여서 Scheme이 표현식을 평가하지 못하도록 합니다. 이는 모든 키와 값이 하드코딩된 정적 목록을 생성하는 데 이상적입니다.

```scheme
;; Manually define an alist
(define alist '((name . "Alice") (age . 30) (city . "Paris")))

;; Programmatically add a new pair
(define updated-alist (cons '(country . "France") alist))
```

**결과**:
`((country . "France") (name . "Alice") (age . 30) (city . "Paris"))`

#### 역따옴표 사용(`` ```) and Comma (`,`)

역따옴표(`` ` ``) is similar to the single quote but allows you to dynamically insert evaluated expressions using the comma (`,`) 연산자입니다. 이는 런타임에 키나 값이 계산되는 목록을 만드는 데 유용합니다.

```scheme
(define key 'name)
(define value "Alice")

(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

**결과**:
`((name . "Alice") (age . 30) (city . "Paris"))`

### 예시 비교

`'`을 사용하는 정적 목록:

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
```

`` ``` and ``을 사용하는 동적 목록:

```scheme
(define key 'name)
(define value "Alice")
(define alist `((,key . ,value) (age . 30) (city . "Paris")))
```

### Alist의 데이터에 액세스

alist에서 값을 검색하려면 해당 키로 쌍을 찾는 `assoc` 함수를 사용할 수 있습니다.

```scheme
(assoc 'name alist)   ; Returns (name . "Alice")
(assoc 'country alist) ; Returns #f (key not found)
```

### 값 추출

`assoc`을 사용하여 쌍을 검색한 후 `cdr`을 사용하여 값을 추출합니다.

```scheme
(cdr (assoc 'name alist))   ; Returns "Alice"
```

### 주요 기능 요약

- **작은 따옴표(`'`)**: 모든 요소가 리터럴 데이터인 정적 alist를 생성합니다.
- **역따옴표(`` ```)**: Allows dynamic creation of alists by mixing static elements with evaluated expressions (using `,`).
- **점 표기법(`.`)**: 목록의 값과 키를 연결하여 쌍을 구성하는 데 사용됩니다.