---
title: "메시징 라이브러리"
type: docs
weight: 6
---
시간이 지나면서 메시지를 보내는 단일 기능으로 시작된 것이 관련 기능의 모음으로 발전했습니다. 이러한 기능은 이제 GUI, 오류 콘솔, 터미널 등 다양한 대상으로의 출력을 처리하도록 설계된 **메시징 라이브러리**의 기반을 형성합니다.

### 메시징 라이브러리가 필요한 이유

요구 사항이 증가함에 따라 여러 출력에 걸쳐 메시지를 처리하려면 보다 모듈화되고 확장 가능한 접근 방식이 필요합니다. 모든 작업을 수행하는 단일 기능 대신 프로세스를 재사용 가능한 구성 요소로 나누어 유연성을 높였습니다. 이제 이 라이브러리를 다른 플러그인이나 기능에서 빌릴 수 있는 범용 메시징 도구로 사용할 수 있습니다.

### 메시징 라이브러리의 기능은 무엇입니까?

메시징 라이브러리에는 현재 다음 기능이 포함되어 있습니다.

- **send-to-gui**: Lumi GUI 대화 상자에 메시지를 보냅니다.
- **send-to-error-console**: Lumi 오류 콘솔로 메시지를 보냅니다.
- **터미널로 보내기**: 터미널 창으로 메시지를 보냅니다.
- **send-message**: 메시지를 적절한 출력으로 보내는 디스패처 기능입니다.
- **validate-message**: 보내기 전에 메시지와 출력이 유효한지 확인합니다.

### 라이브러리 확장

**메시징 라이브러리**는 추가 출력을 지원하도록 쉽게 확장할 수 있습니다. 예를 들면:

- **파일로 보내기**: 메시지를 로그 파일에 저장합니다.
- **로거로 전송**: 외부 로깅 시스템과 통합됩니다.
- **알림으로 보내기**: 메시지를 시스템 알림으로 표시합니다.

모듈식 디자인과 재사용 가능한 기능의 동일한 패턴을 따르면 이 라이브러리는 모든 종류의 메시징 작업을 처리하기 위한 포괄적인 도구로 성장할 수 있습니다.

## 메시징 라이브러리의 이점

- **재사용성**: 다양한 플러그인이나 프로젝트에서 기능을 재사용할 수 있습니다.
- **모듈화**: 각 함수는 하나의 특정 작업을 처리하므로 코드를 더 쉽게 유지 관리하고 확장할 수 있습니다.
- **일관성**: 동일한 유효성 검사 및 메시지 처리 기능을 사용하면 애플리케이션 전체에서 일관된 동작이 보장됩니다.

**메시징 라이브러리**는 프로젝트에서 메시지를 관리하는 방법을 단순화할 수 있는 더 광범위한 프레임워크의 시작입니다. 라이브러리가 성장함에 따라 새로운 플러그인은 이를 쉽게 활용하여 필요한 곳 ​​어디든 메시지를 보낼 수 있습니다.

파일 구조를 조정할 수 있습니다.

```plaintext
/home/your-username/code/
  ├── script-fu/
      ├── library/
      │     └── send-message.scm -> messaging.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

그리고 메인 플러그인에서 `load`을 조정하는 것을 잊지 마세요:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/messaging.scm")

(define (scheme-hello-world)
  (let ((message "Hello world!\n"))
    (send-message message 'gui)
    (send-message message 'error-console)
    (send-message message 'terminal)))

(scheme-register-procedure "scheme-hello-world"
  "Hello world!"
  "A Scheme procedure plug-in refactored"
  "Mark Sweeney"
  "Under GNU GENERAL PUBLIC LICENSE Version 3"
  "2024")

(scheme-menu-register
  "scheme-hello-world"
  "<Image>/Funky")
```