---
title: "안녕하세요 월드!"
type: docs
weight: 1
---
이 튜토리얼에서는 Scheme 플러그인의 최소 구조를 안내합니다. 일부 줄은 "상용구"입니다. 아직 완전히 이해하지 못하더라도 Lumi가 파일을 로드하는 데 필요합니다.

```bash
# !/usr/bin/env lumi-scheme-interpreter-0.1
```

높은 수준에서는 다음을 수행합니다.

1. 함수 정의
2. 프로시저 데이터베이스에 나타나도록 등록하세요.
3. (선택사항) 메뉴 항목 추가
4. 플러그인 폴더에 파일 설치

### 함수 정의

_프로시저_라고도 알려진 함수는 이름과 목적이 있는 코드 덩어리로, 입력을 받아 출력을 생성합니다.

**입력** > **_함수_** > **출력**

### 기능 등록

등록이란 루미가 알 수 있도록 함수 이름을 목록에 올리는 행위입니다.

```scheme
(scheme-register-procedure "scheme-hello-world"...
```

### 메뉴 링크

이는 Lumi의 메뉴 시스템에서 기능을 찾을 수 있는 위치를 알려줍니다.

```scheme
(scheme-menu-register "scheme-hello-world" "<Image>/Funky")
```

그러면 메인 메뉴 표시줄에 "Funky" 메뉴가 표시됩니다. 플러그인을 다른 곳에 배치하려면 경로를 변경하세요. `<Image>/Funky` 경로는 플러그인이 **이미지** 메뉴 범주 아래에 표시된다는 의미입니다. 플러그인을 표시하려는 위치에 따라 `<Image>`을 `<Tools>`, `<Filters>` 등으로 변경할 수 있습니다.

### 댓글

Scheme의 기본 언어인 Scheme에서 주석은 일반적으로 유용한 텍스트 줄 앞에 `;;`을 추가하여 수행됩니다. 주석 사용은 코더로서의 유창성에 따라 달라집니다. 가끔씩 코딩한다면 더 많은 주석이 도움이 될 것입니다. 항상 코드를 작성한다면 코드는 주석만큼 읽기 쉽습니다. 또한 기능적으로 프로그래밍할 때 코드는 스크립트처럼 읽을 수 있을 만큼 설명적이 되는 경향이 있습니다.

### 구문

코드에는 한 줄에 항목을 배치하는 방법에 대한 규칙이 거의 없으므로 줄을 쉽게 읽을 수 있습니다. 예를 들어, 문장의 쉼표나 마침표 뒤에 공백이 있을 수 있습니다. 가독성에 도움이 됩니다.

코드는 유사한 방식으로 항목을 정렬할 수 있으며 처음에는 이상하게 보일 수 있습니다.

```scheme
(define (function-name input-a
                       input-b
                       input-c))
```

## 예제 코드

다음은 전체 예입니다. 대부분의 Lumi 프로시저에는 `lumi-` 접두사가 붙습니다. 예를 들어 `lumi-message`은 구성된 메시지 처리기에 문자열을 인쇄합니다.

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(define (scheme-hello-world)

  ;; Set the message handler to output the message to a GUI dialog box
  (lumi-message-set-handler 0)
  (lumi-message "Hello world!\n")

  ;; Set the message handler to output the message to the Error Console
  (lumi-message-set-handler 2)
  (lumi-message "Hello world!\n")

  ;; Send the message to the terminal, the OS window that launched Lumi
  (display "Hello world!\n"))


(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```

### 플러그인 설치

1. **Lumi -> 편집 -> 환경 설정 -> 폴더 -> 플러그인**으로 이동합니다.
2. [repo](/hub/scripting/tools/git) 플러그인 폴더를 목록에 추가하세요.
3. 플러그인용 폴더를 생성하고 위의 예제 코드를 `hello-world.scm`으로 저장합니다.
  - `your-plug-ins-repo/hello-world/hello-world.scm`
4. `hello-world.scm` 파일을 마우스 오른쪽 버튼으로 클릭하세요.
5. **속성 -> 권한 -> 파일을 프로그램으로 실행 허용**으로 이동합니다.
6. 루미를 다시 시작하세요.

### 플러그인 사용해 보기

이제 플러그인이 기본 Lumi 창의 "Funky" 메뉴 아래에 표시됩니다. 클릭하면 "Hello world!"가 표시됩니다. 메시지. 메시지 텍스트 변경 등 코드를 수정하고 파일을 저장해 보세요. 플러그인을 다시 실행하면 Lumi를 다시 시작하지 않고도 변경 사항이 반영됩니다.

메뉴 경로를 변경하여 실험해 보세요. 예를 들어 `"<Image>/File"`은 파일 메뉴에 넣고 `"<Image>/File/Funky"`은 파일 메뉴에 새 섹션을 만듭니다. 이는 플러그인이 표시되는 위치를 사용자 정의하고 도구를 구성하는 좋은 방법입니다.