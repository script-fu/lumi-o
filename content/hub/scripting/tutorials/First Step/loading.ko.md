---
title: "로드 중"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_source_sha256: f278c01f86610dfeccac49fa73803a405bad82f7ef3b60226ff4350fb4ec257b
translation_lock: true
url: "hub/scripting/tutorials/First Step/loading"
---
도우미 함수가 커지면 이를 작은 라이브러리 파일로 옮깁니다. 이를 통해 플러그인에 집중하고 도우미를 여러 플러그인에서 재사용할 수 있습니다.

### 라이브러리 함수 만들기

메시지 보내기 기능을 사용하여 해당 내용을 포함하는 새 파일을 만들 수 있습니다. 파일을 플러그인 부분이 아닌 repo 폴더(아마도 최상위 수준 근처)에 저장하세요.

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: 이것은 Scheme 코드를 저장하기 위한 기본 디렉터리입니다.
  - **라이브러리/**: `send-message.scm`과 같은 공유 기능이 있는 곳입니다.
  - **플러그인/**: 개별 플러그인이 저장되는 곳입니다.
    - **hello-world/**: 특정 "Hello World!" 플러그인용 폴더.
      - **hello-world.scm**: 플러그인의 스크립트 파일.

라이브러리 함수 send-message.scm의 예

```scheme
;; 다양한 대상으로 메시지 출력을 처리하는 함수
(define (send-message message output)
  (cond
    ;; 메시지 콘솔로 보내기
    ((eq? output 'error-console)
       ;; 핸들러를 메시지 콘솔로 설정
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; GUI 대화 상자로 보내기
    ((eq? output 'gui)
       ;; 핸들러를 GUI 대화 상자로 설정
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; 터미널 창으로 보내기
    ((eq? output 'terminal)
       ;; terminal 출력은 display로 처리
       (display message)))

  ;; 기본 메시지 핸들러를 메시지 콘솔로 복원
  (lumi-message-set-handler 2))
```

### 라이브러리 기능 로드

Scheme `load` 명령을 사용하여 해당 라이브러리 함수를 로드할 수 있습니다.

라이브러리 파일 로드:

```scheme
#!/usr/bin/env lumi-scheme-interpreter-0.1

(load "/home/mark/code/github/script-plugins/funky-library/send-message.scm")

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

이봐! 이제 우리는 읽기에 더 간단하고 짧은 내용을 갖게 되었습니다. 그런 종류의 설명은 주석 없이 설명됩니다. 이것이 리팩토링의 만족스러운 결론입니다.