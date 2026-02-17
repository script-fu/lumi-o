---
title: "Carregando"
type: docs
weight: 3
---
Assim que uma função auxiliar crescer, mova-a para um pequeno arquivo de biblioteca. Isso mantém o plug-in focado e torna o auxiliar reutilizável em vários plug-ins.

### Faça uma função de biblioteca

Podemos pegar a função enviar mensagem e criar um novo arquivo com ela como conteúdo. Salve o arquivo em sua pasta repo, não na parte de plugins, talvez próximo ao nível superior;

```plaintext
/home/your-username/code/
  ├── scheme/
      ├── library/
      │     └── send-message.scm
      └── plug-ins/
            └── hello-world/
                  └── hello-world.scm
```

- **scheme/**: Este é o diretório principal para armazenar o código do esquema.
  - **library/**: É aqui que funcionam funções compartilhadas como `send-message.scm`.
  - **plug-ins/**: É aqui que seus plug-ins individuais são armazenados.
    - **hello-world/**: A folder for the specific "Hello World!" plug-in.
      - **hello-world.scm**: The script file for the plug-in.

Exemplo de função de biblioteca send-message.scm

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

### Carregar a função da biblioteca

Podemos carregar essa função de biblioteca com o comando Scheme `load`;

Carregando um arquivo de biblioteca:

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

Ei! Agora temos algo mais simples e curto de ler, que se descreve sem comentários. Esta é a conclusão satisfatória da refatoração.