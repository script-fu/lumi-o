---
title: "로드 중"
type: docs
weight: 3
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
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

라이브러리 함수 send-message.scm의 예

```scheme
;; Function to handle message output to various destinations
(define (send-message message output)
  (cond
    ;; Send to the Error Console
    ((eq? output 'error-console)
       ;; Set the handler to Error Console
       (lumi-message-set-handler 2)
       (lumi-message message))

    ;; Send to the GUI dialog box
    ((eq? output 'gui)
       ;; Set the handler to GUI dialog
       (lumi-message-set-handler 0)
       (lumi-message message))

    ;; Send to the terminal window
    ((eq? output 'terminal)
       ;; Terminal output is handled with display
       (display message)))

  ;; Restore the default message handler to the Error Console
  (lumi-message-set-handler 2))
```

### 라이브러리 기능 로드

Scheme `load` 명령을 사용하여 해당 라이브러리 함수를 로드할 수 있습니다.

라이브러리 파일 로드:

```scheme
# !/usr/bin/env lumi-scheme-interpreter-0.1

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