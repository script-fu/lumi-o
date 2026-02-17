---
title: "프로시저 브라우저"
type: docs
weight: 1
---
**Lumi 프로시저 브라우저**를 사용하면 사용 가능한 프로시저(내장 및 플러그인 제공)를 검색하고 해당 매개변수와 반환 값을 검사할 수 있습니다.

### Lumi 프로시저 브라우저를 찾을 수 있는 곳

**도움말** 메뉴를 통해 Lumi의 Procedure Browser에 액세스할 수 있습니다.

- **도움말** -> **프로시저 브라우저**

### 프로시저 브라우저의 기능

Procedure Browser에는 방금 설치한 프로시저를 포함하여 플러그인에 의해 추가된 프로시저와 함께 Lumi의 모든 내부 프로시저가 나열됩니다. 각 프로시저 항목은 다음을 포함한 유용한 정보를 제공합니다.

- 프로시저 이름.
- 어떤 일을 하는지에 대한 설명입니다.
- 허용되는 매개변수(입력 값)입니다.
- 반환 값(출력).

호출 서명을 확인하거나 정확한 시술명을 확인해야 할 경우 키워드나 시술명으로 검색하세요.

프로시저 브라우저의 ####(lumi-message)

매개변수와 반환 값을 보려면 `lumi-message`을 검색하세요.

### 플러그인 찾기

일단 "Hello World!"를 설치했다면 플러그인의 경우 Procedure Browser에 나열된 것을 찾을 수 있습니다. Lumi에 등록한 함수 이름(이 경우 "scheme-hello-world")을 검색하기만 하면 됩니다. 항목에는 간단한 설명과 함께 플러그인과 관련된 매개변수 및 반환 값이 표시됩니다. 또한 등록 프로세스 중에 입력 매개변수로 입력한 텍스트 줄 중 일부가 **추가 정보** 섹션 아래에 표시되는 위치도 확인할 수 있습니다.

```scheme
(scheme-register-procedure "scheme-hello-world"   ;; Procedure name
  "Hello world!"                                        ;; Menu item name
  "A Scheme procedure plug-in"                       ;; Tool tip and description
  "Your Name"                                           ;; Author
  "Under GNU GENERAL PUBLIC LICENSE Version 3"          ;; License
  "2024")                                               ;; Copyright Date
```

이를 통해 플러그인이 올바르게 등록되었는지 쉽게 확인할 수 있으며 Lumi의 다른 절차와 상호 작용하는 방식을 빠르게 검토할 수 있습니다. Procedure Browser는 Lumi 내에서 사용 가능한 모든 프로시저를 탐색하여 플러그인을 디버깅하고 확장하기 위한 강력한 도구입니다.