---
title: "로드 중"
type: docs
weight: 3
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 3dd031042d2683ece82da9ee4444cc1818609d9acf5f609bb1a42115c39275d8
---
As soon as a helper function grows, move it into a small library file. That keeps the plug-in focused and makes the helper reusable across multiple plug-ins.

### Make a Library Function

We can take the send-message function and make a new file with that as its content. Save the file into your repo folder, not the plugins part, perhaps near the top level;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: This is your main directory for storing your Scheme code.
  - **library/**: This is where shared functions like `send-message.scm` live.
  - **plug-ins/**: This is where your individual plug-ins are stored.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Example of a library function send-message.scm

```scheme
;; 다양한 대상으로 메시지 출력을 처리하는 함수
(define (send-message message output)
  (cond
    ;; Message console로 보내기
    ((eq? output 'error-console)
       ;; 핸들러를 Message console로 설정
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

  ;; 기본 메시지 핸들러를 Message console로 복원
  (lumi-message-set-handler 2))
```

### Load the Library Function

We can load that library function with the Scheme `load` command;

Loading a library file:

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

이봐! We've now got something simpler and shorter to read, that kind of describes itself without comments. This is the satisfying conclusion of refactoring.