---
title: "디버깅"
type: docs
weight: 5
---
스크립팅에서는 오류가 없는 기능이 없습니다. 가장 신뢰할 수 있는 명령이라도 예상치 못한 입력이나 조건이 발생하면 실패할 수 있습니다. 이를 방지하기 위해 맞춤형 디버깅 시스템을 구현하고 방어 프로그래밍 기술을 채택할 수 있습니다. 오류 처리 메커니즘으로 표준 기능을 래핑하고 유익한 피드백을 제공함으로써 스크립트를 더욱 강력하고 문제 해결하기 쉽게 만들 수 있습니다.

이 전략의 핵심 부분은 전역 디버그 플래그를 사용하여 자세한 출력을 제어함으로써 정상적인 실행 중에 출력을 깨끗하게 유지하면서 필요할 때 자세한 디버깅 정보를 활성화할 수 있도록 하는 것입니다.

## 전역 디버그 플래그

전역 디버그 플래그는 스크립트 실행 중 정보 출력 수준을 제어하는 간단하면서도 효과적인 방법입니다. 활성화되면 문제를 추적하는 데 매우 유용한 자세한 디버깅 메시지를 제공합니다. 비활성화되면 프로덕션 용도로 출력을 간결하게 유지합니다.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #f)
```

기본적으로 디버깅은 꺼져 있습니다. 개발 중에 자세한 출력을 활성화하려면 플래그를 `#t`으로 설정하기만 하면 됩니다.

```scheme
;; Purpose: Global flag to control debug output.
(define debug #t)
```

도우미 함수를 사용하여 특정 코드 섹션에 대한 디버깅을 일시적으로 활성화하거나 비활성화할 수도 있습니다.

### 로컬 디버그 제어

보다 세부적인 제어를 위해 도우미 함수를 사용하여 스크립트의 특정 부분 내에서 디버깅을 켜거나 끌 수 있습니다.

```scheme
;; Purpose: Turn off debug mode for a section of code.
(define (debug-off)
  (set! debug #f))

;; Purpose: Turn on debug mode for a section of code.
(define (debug-on)
  (set! debug #t))
```

이를 통해 디버깅을 동적으로 제어할 수 있습니다.

```scheme
(debug-on)  ;; Enable verbose output

;; Some script logic here

(debug-off) ;; Disable verbose output
```

## 디버그 메시징 시스템

Scheme에서 디버그 출력을 효율적으로 처리하기 위해 여러 도우미 기능이 포함된 구조화된 접근 방식을 사용합니다. 이러한 기능을 사용하면 디버그 및 경고 메시지가 명확하고, 읽기 쉽고, 유지 관리 가능합니다.

### 디버그 메시징 시스템 개요

디버그 메시징 시스템은 다음 구성 요소로 구성됩니다.

1. `debug-message` – 디버깅이 활성화되면 디버그 메시지를 표시합니다.
2. `serialize-item` – 다양한 Scheme 데이터 유형을 문자열 표현으로 변환합니다.
3. `concat` – 여러 항목을 단일 문자열로 연결합니다.
4. `list->string` – 목록을 읽을 수 있는 문자열로 형식화합니다.
5. `message` – Lumi의 메시지 콘솔에 출력을 표시합니다.
6. `warning-message` – 경고가 활성화되면 경고 메시지를 표시합니다.

각 기능은 구조화된 메시지의 형식을 지정하고 표시하는 역할을 합니다.

---

### 디버그 메시지 기능

`debug-message` 함수는 디버그 출력을 표시하는 핵심 방법입니다. 디버깅이 활성화된 경우에만 메시지가 표시되도록 합니다.

```scheme
;; Purpose: Display a debug message.
(define (debug-message . items)
  (when debug (message "> " (apply concat items))))
```

- `when debug` 조건은 디버깅이 활성화된 경우에만 메시지가 표시되도록 보장합니다.
- 명확성을 위해 메시지 앞에 `"> "`이 붙습니다.
- 이 함수는 `concat`을 사용하여 메시지 내용의 형식을 지정합니다.
- 마지막으로 `message`을 호출하여 출력을 Lumi의 메시지 콘솔로 보냅니다.

사용 예:

```scheme
;; Purpose: Returns the item's tree position or #f if the item is invalid
(define (get-item-tree-position image item)
  (if (item-is-valid? item)
    (let ((position (list->item (lumi-image-get-item-position image item))))
      (debug-message "item : " (item-get-name item) " has tree position : " position)
      position)
    #f))
```

디버깅이 활성화되면 출력은 다음과 같을 수 있습니다.

```scheme
> item: background-layer has tree position : 3
```

### 디버그 메시지용 데이터 직렬화

메시지에는 목록, 벡터, 숫자 등 다양한 데이터 유형이 포함될 수 있습니다. 형식이 올바른지 확인하기 위해 `serialize-item`을 사용합니다.

```scheme
;; Purpose: Converts various Scheme data types (lists, vectors, pairs, etc.)
;;          into a string representation.
(define (serialize-item item)
  (cond
    ((and (list? item) (null? item)) "\"\"")          ; Empty list
    ((and (string? item) (string=? item "")) "\"\"")  ; Empty string
    ((list? item) (list->string item))                ; Nested list
    ((vector? item)                                   ; Handle vectors
     (string-append "#("
                    (string-join (map serialize-item (vector->list item)) " ")
                    ")"))
    ((pair? item)                                     ; Handle pairs
     (string-append "("
                    (serialize-item (car item))
                    " . "
                    (serialize-item (cdr item))
                    ")"))
    ((number? item) (number->string item))            ; Numbers
    ((symbol? item) (symbol->string item))            ; Symbols
    ((boolean? item) (if item "#t" "#f"))             ; Booleans
    ((string? item) item)                             ; Strings
    (else (warning-message "serialize-item: Unsupported item type!" item))))
```

사용 예:

```scheme
(serialize-item '(1 2 3))
```

출력:

```scheme
list:
1
2
3
```

### 메시지 연결

여러 메시지 구성 요소를 단일 문자열로 병합하려면 `concat`을 사용합니다.

```scheme
;; Purpose: Concatenate multiple items into a single string.
(define (concat . items)
  (apply string-append (map serialize-item items)))
```

사용 예:

```scheme
(concat "Image size: " 1920 "x" 1080)
```

### 목록을 문자열로 형식화하기

`list->string` 함수는 목록을 형식이 지정된 문자열로 변환합니다.

```scheme
;; Purpose: Convert a list of items into a readable string.
(define (list->string list)
  (if (list? list)
      (string-append "list: \n" (string-join (map serialize-item list) "\n"))
      (warning-message "list->string: Input is not a list!")))
```

### 경고 메시지`warning-message` 함수는 `debug-message`과 유사하게 작동하지만 디버깅이 비활성화된 경우에도 경고를 표시합니다.

```scheme
;; Purpose: Display a warning message.
(define (warning-message . items)
  (if warning
    (message "Warning: " (apply concat items)))
    #f)
```

- 경고가 활성화된 경우에만 메시지가 표시되도록 합니다(`warning` 플래그는 `common.scm`에 `#t`으로 설정됨).
- `concat`을 호출하여 메시지 내용의 형식을 지정합니다.
- `message`을 사용하여 Lumi에 출력을 보냅니다.

## 표준 기능 강화

디버깅 시스템이 구축되면 자세한 메시지를 통합하여 기능 라이브러리를 향상시킬 수 있습니다. 이를 통해 항목 상태, 변수 값 및 함수 호출에 대한 통찰력을 얻을 수 있습니다.

일반적인 예는 `item-is-valid?`입니다. 이는 `lumi-item-id-is-valid`을 래핑하여 `#t` 또는 `#f`을 반환합니다. `#f`이 반환되면 호출 코드에서 `warning-message`을 트리거할 수 있습니다. 입력이 숫자가 아닌 경우 함수에서 경고를 표시할 수 있습니다.

```scheme
;; Purpose: Check if an item is valid, returning #t or #f.
;;          Issues a warning if the item is not a number.
(define (item-is-valid? item)
  (if (number? item)
      (= (list->item (lumi-item-id-is-valid item)) 1)
      (begin
        (warning-message "item-is-valid?: Expected a number, but received: " item)
        #f)))
```

## 실제 사용법

Scheme 플러그인을 개발할 때 이러한 방식으로 함수를 래핑하면 디버깅 시간이 크게 줄어들고 강력하고 유지 관리 가능한 코드가 보장됩니다. 디버깅 시스템을 갖추고 있으면 스위치를 한 번만 누르면 오류 콘솔에서 구조화된 디버그 스트림을 생성할 수 있습니다.

이 디버그 스트림에서 함수 호출은 별표(*)로 표시되어 특히 복잡한 플러그인에서 스크립트 실행을 추적하고 오류를 찾아내기가 더 쉽습니다. 이러한 가시성은 작업 흐름을 이해하고 예상치 못한 동작을 효율적으로 진단하는 데 도움이 됩니다.

`*`을 사용하기 위한 메시지 기능의 래퍼

```scheme
(define (call . items)
  (when debug (message "* (" (apply concat items) ")")))
```

실제로 사용되는 `call`의 예:

```scheme
;; Purpose: Apply the texturing process to the given list of group masks
(define (process-masks groups pattern) (call 'process-masks)
  (for-each
    (lambda (group)
      (let ((mask (add-mask-to-layer group ADD-MASK-WHITE)))
        (message "Process mask : " (item-get-name group))
        (fill-and-adjust-group-mask group mask pattern)
        (lumi-layer-set-opacity group (get 'color-opacity))
        (lumi-item-set-expanded (item-get-parent group) 0)
        (lumi-selection-none (get-image))))
    (ensure-list groups)))
```

플러그인이 실행되는 디버그 스트림의 예:

```scheme
> Recording the plug-in settings
* (convert-gui-settings)
> all-masks : 1
> strokes : 1
> color : 1
> plate-layer : 1
> drawables : #(37)
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _colour
blue_colour
yellow_colour
gray_colour
* (filter-list-for-matching-groups)
> all-masks : #t
> sub-groups of group : root
blue
blue_strokes
blue_colour
yellow
yellow_strokes
yellow_colour
gray
gray_strokes
gray_colour
> groups with identifier in name: _strokes
blue_strokes
yellow_strokes
gray_strokes
* (begin-apply-texture)

Start Apply Texture

> color : #t

Texturing color group masks
> color-pattern : 2655
* (process-masks)
Process mask : blue_colour
* (fill-and-adjust-group-mask)
> Fill-and-adjust : blue_colour mask
> using pattern for fill : 2655
* (apply-color-effect)
> color-contrast : 64
> color-levels-gamma : 10
> levels on drawable: blue_colour mask
>   gamma: 8.2
>   low-in: 0.7278  high-in: 0.9222
>   low-out: 0  high-out: 1
> light-opacity : 6
> light-opacity : 6
* (apply-light-effect)
> apply-light-effect opacity : 6
> from layer : light_blue
> edit-copy light_blue
> edit-paste blue_colour mask
> shade-opacity : 60
> shade-opacity : 60
* (apply-light-effect)
> apply-light-effect opacity : 60
> from layer : shad_blue_opa*5
> edit-copy shad_blue_opa*5
> edit-paste blue_colour mask
* (apply-opaque-effect)
> children in : blue_colour
blue_colour
hue_blue
light_blue
shad_blue_opa*5
base_blue
...
...
...
Finished Apply Texture!
```

이 구조화된 로그는 함수 호출 및 데이터 변경에 대한 명확한 타임라인을 제공하므로 디버깅 및 성능 분석이 훨씬 쉬워집니다.

## 결론

구조화된 디버깅 시스템을 구현함으로써 실행에 대한 실시간 통찰력을 제공하는 보다 안전하고 유지 관리가 용이한 스크립트를 만듭니다.

### 주요 내용

- **자세한 내용 제어** – 전역 디버그 플래그를 사용하여 출력 레벨을 관리합니다.
- **명확한 피드백 제공** – 유용한 디버그 메시지로 표준 기능을 래핑합니다.
- **강건성 향상** – 예상치 못한 입력을 적절하게 처리하여 오류를 방지합니다.
- **문제 해결 단순화** – 구조화된 디버그 메시지를 사용하면 문제를 더 쉽게 진단하고 해결할 수 있습니다.

이러한 접근 방식을 사용하면 스크립트가 데이터를 처리할 때 효과적으로 "자기 설명"하여 좌절감을 줄이고 작업 흐름 효율성을 향상시킵니다. 디버깅은 사후 대응적인 일이 아닌 사전 예방적인 도구가 되어 스크립팅 프로세스를 더욱 원활하고 보람되게 만듭니다.