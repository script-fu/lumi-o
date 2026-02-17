---
title: "필터 플러그인"
type: docs
weight: 2
---
[First Step](../../first-step/) 튜토리얼에서는 _procedure_ 플러그인을 사용했습니다. 이러한 유형의 플러그인은 입력으로 이미지나 드로어블이 필요 없이 작동합니다. 일반적으로 우리는 플러그인을 사용하여 이미지와 해당 드로어블을 변경합니다. 이와 같은 플러그인을 _filter_ 플러그인이라고 합니다.

### 드로어블이란 무엇입니까?

Lumi의 **드로어블**은 레이어나 채널과 같이 그릴 수 있는 이미지 요소를 나타냅니다. 필터 플러그인은 일반적으로 이러한 요소에서 작동합니다.

### 간단한 필터 플러그인 예

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-simple-filter-plug-in image drawables)
  ;; Use a let statement to define a message variable and core code
  (let ((message "hello, world"))
    ;; Display the message in Lumi's error console
    (lumi-message message)
    ;; Invert the colors of the first selected drawable
    (lumi-drawable-invert (vector-ref drawables 0) 1)))

;; Register the plug-in
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Main procedure name
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Give yourself some credit
  "License"                                ;; License
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates this plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables

;; Specify the menu location for the plug-in
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

텍스트를 복사하여 Lumi 플러그인 폴더 중 하나에 있는 `simple-filter-plug-in` 폴더에 `simple-filter-plug-in.scm`으로 저장합니다. Lumi 플러그인 폴더는 다음 아래에 나열된 _모든_ 폴더입니다.
 **Lumi > 편집 > 환경 설정 > 폴더 > 플러그인**

Linux에서는 `simple-filter-plug-in.scm` 파일을 마우스 오른쪽 버튼으로 클릭하고 **속성 > 권한**으로 이동한 다음 **프로그램으로 파일 실행 허용**을 선택하세요. 파일이 올바른 위치에 있고 실행 가능하며 구문 오류가 없으면 Lumi를 다시 시작할 때 **플러그인**이라는 메뉴 내 상단 메뉴 헤더 표시줄에 나타납니다.

### 플러그인 실행

1. 이미지를 엽니다(이 필터 플러그인이 작동하려면 이미지가 필요합니다).
2. **Windows > 도킹 가능한 대화 상자 > 오류 콘솔**을 열어 메시지를 확인합니다.
3. **플러그인** 메뉴에서 **간단한 필터 플러그인 데모**를 선택합니다.
4. 선택한 레이어 중 하나의 색상이 반전되고 오류 콘솔에 메시지가 인쇄됩니다.

### 플러그인 편집

`.scm` 파일을 편집하여 플러그인을 사용자 정의할 수 있습니다. 예를 들어 표시되는 메시지를 변경하려면 다음을 수행하십시오.

1. 파일을 열고 `message`을 정의하는 줄을 찾습니다.
2. `"hello, world"`을 사용자 정의 텍스트로 바꾸세요.
3. 파일을 저장합니다.

Lumi 버전 3에서는 저장된 변경 사항을 적용하기 위해 플러그인을 새로 고칠 필요가 없습니다. 업데이트된 메시지를 보려면 플러그인을 다시 실행하기만 하면 됩니다.

### 플러그인 시험

#### 셰뱅라인

첫 번째 줄은 스크립트가 Lumi 3에서 플러그인으로 작동하는지 확인합니다.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

#### 절차 정의

이 프로시저는 활성 이미지와 선택한 드로어블이라는 두 가지 인수를 허용합니다.

```scheme
(define (scheme-simple-filter-plug-in image drawables)
```

#### 핵심 로직

`let` 문은 변수를 정의하고 드로어블에 대한 작업을 수행합니다.

```scheme
(let ((message "hello, world"))
  (lumi-message message) ;; Displays a message in Lumi's error console
  (lumi-drawable-invert (vector-ref drawables 0) 1)) ;; Inverts the colors of the first selected drawable
```

### 플러그인 등록

플러그인은 Lumi에 필터 플러그인으로 등록됩니다.

```scheme
(scheme-register-filter
  "scheme-simple-filter-plug-in"           ;; Register the main procedure
  "Simple Filter Plug-in Demo"             ;; The name as it appears in the Lumi menu
  "Tests a basic Scheme filter plug-in"    ;; Tool-tip description
  "Author Name"                            ;; Author's name
  "License"                                ;; License type
  "Date written"                           ;; Date written
  "*"                                      ;; Indicates the plug-in requires an image
  SF-ONE-OR-MORE-DRAWABLE)                 ;; Requires one or more selected drawables
```

#### 메뉴등록
이 줄은 플러그인의 메뉴 위치를 지정합니다.

```scheme
(scheme-menu-register
  "scheme-simple-filter-plug-in"
  "<Image>/Plug-in")
```

### 문제 해결

플러그인이 나타나지 않으면 플러그인 위치, 이름, 실행 속성을 확인하세요.

위치는 플러그인 검색 경로에 있어야 합니다.
파일 이름은 포함된 폴더의 이름과 일치해야 합니다.
파일은 실행 파일로 설정되어야 합니다.


**오류 콘솔**은 사용자 정의 플러그인 문제를 해결하는 데 유용한 도구입니다. 플러그인이 예상대로 작동하지 않으면 여기에서 오류 메시지나 로그를 확인하세요. **터미널** 창에서는 디버깅 정보를 제공하고 로드 문제를 보고할 수도 있습니다.