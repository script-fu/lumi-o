---
title: "기호"
type: docs
weight: 6
---
기호는 고유하고 변경할 수 없는 식별자를 나타내는 Scheme의 핵심 데이터 유형 중 하나입니다. 이는 주로 프로그램에서 키, 마커 또는 자리 표시자로 사용되므로 깔끔하고 표현력이 풍부한 코드를 작성하는 데 필수적입니다.

Scheme의 기호는 문자열과 유사하지만 기호가 **고유**하고 **원자**라는 점에서 다릅니다. 이는 동일한 이름을 가진 두 개의 기호가 동일한 객체임을 보장하여 데이터 구조에서 빠른 동등성 검사와 효율적인 사용을 가능하게 함을 의미합니다.

### 구문

기호는 일련의 문자로 작성됩니다.

- 문자로 시작하고 그 뒤에 문자, 숫자 또는 `-`, `+` 또는 `*`과 같은 특수 문자가 옵니다.
- 기호는 기본적으로 대소문자를 구분합니다.

예:

```scheme
'hello       ; A symbol named `hello`
'foo-bar     ; A symbol named `foo-bar`
'*special*   ; A symbol named `*special*`
```

## 기호 만들기

기호는 일반적으로 **quote** 연산자(`'`)를 사용하여 생성됩니다. 이는 Scheme이 이름을 변수나 함수로 평가하는 대신 기호로 처리하도록 지시합니다.

### 예

```scheme
'my-symbol   ; Creates the symbol `my-symbol`
```

문자열을 기호로 변환하는 `string->symbol` 프로시저를 사용하여 프로그래밍 방식으로 기호를 생성할 수도 있습니다.

```scheme
(string->symbol "dynamic-symbol")
```

**결과**: `'dynamic-symbol`


## 기호 비교

기호는 고유하므로 `eq?`을 사용하여 효율적으로 비교할 수 있습니다.

### 예

```scheme
(eq? 'apple 'apple)   ; #t (same symbol)
(eq? 'apple 'orange)  ; #f (different symbols)
```

따라서 기호는 데이터 구조의 키 또는 코드의 마커로 사용하기에 이상적입니다.

## 기호 사용

기호는 다음과 같은 용도로 구성표에서 자주 사용됩니다.

1. **연관 목록의 키:**

```scheme
(define alist '((name . "Alice") (age . 30) (city . "Paris")))
(assoc 'name alist)   ; Returns (name . "Alice")
```

2. **코드의 식별자:**

```scheme
   (define my-symbol 'foo)
   (if (eq? my-symbol 'foo)
       "It's foo!"
       "It's something else.")
```

## 기호 작업 절차

Scheme은 기호 작업을 위한 기본 제공 절차를 제공합니다.

| 절차 | 설명 |
|--------------------|--------------------------------------------------|
| **`symbol?`** | 객체가 기호인지 확인합니다.                                            |
| **`eq?`** | 두 기호를 동일성으로 비교합니다(빠른 비교).                       |
| **`symbol->string`** | 기호를 문자열로 변환합니다(표시 또는 디버깅에 유용함).          |
| **`string->symbol`** | 문자열을 기호로 변환합니다(식별자의 동적 생성에 유용함). |

### 예

```scheme
(symbol? 'example)            ; #t (true: it's a symbol)
(symbol->string 'example)     ; "example"
(string->symbol "new-symbol") ; 'new-symbol
```

## 요약

기호는 Scheme에서 식별자, 키 및 마커를 나타내는 가볍고 효율적인 방법입니다. 불변성과 빠른 신원 확인 덕분에 많은 프로그래밍 작업에 이상적입니다. 기호를 효과적으로 사용하는 방법을 이해하면 깔끔하고 표현력이 풍부한 Scheme 코드를 작성하는 능력이 향상됩니다.